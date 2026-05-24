;;; elot-id.el --- Pluggable identifier minting and verification for ELOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Kluwer

;; Author: Johan W. Kluwer
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Keywords: languages tools org ontology ai

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 10 -- pluggable identifier-minting
;; framework.  Different ontology projects use radically different
;; identifier schemes (timestamp-encoded, OBO-style prefix+counter,
;; UUID, human-readable slug, hash-based, ...).  Rather than baking
;; one scheme into ELOT, this file provides a small registry of
;; `elot-id-scheme' records: each scheme implements its own mint and
;; verify functions, and the project selects the active scheme via
;; the buffer-local `elot-id-scheme' variable, an Org file-local
;; `#+ELOT_ID_SCHEME' / `:elot-id-scheme:' property, or the global
;; `elot-id-default-scheme' defcustom.
;;
;; Public API:
;;
;;   (elot-id-mint   SCHEME LABEL &optional CONTEXT)  -> CURIE string
;;   (elot-id-verify SCHEME CURIE &optional CONTEXT)  -> t / nil
;;   (elot-id-register-scheme SCHEME)                  -> SCHEME
;;   (elot-id-scheme-for-buffer)                       -> SCHEME
;;
;; CONTEXT is a plist with optional keys:
;;
;;   :file          -- path to the ELOT .org file being edited
;;   :prefix        -- CURIE prefix to use (overrides the scheme's
;;                     default `prefix' slot; this is how the buffer's
;;                     `:ELOT-default-prefix:' property is forwarded)
;;   :kind          -- one of: class, object-property, data-property,
;;                     annotation-property, individual, datatype
;;                     (used by schemes that distinguish; the `acme'
;;                     scheme uses this to pick the TYPE letter)
;;   :existing-iris -- list of CURIE/IRI strings already declared in
;;                     the target ontology (for collision detection)
;;   :prefix-table  -- alist of (prefix . iri) for the target ontology
;;
;; Built-in schemes (registered on `(require 'elot-id)'):
;;
;;   uuid     -- prefix + lowercase 32-char hex from `org-id-uuid'.
;;   slug     -- prefix + kebab-case ASCII slug of the label, with
;;               numeric `-N' suffix on collision.
;;   counter  -- prefix + zero-padded numeric suffix; reads
;;               `:existing-iris' to allocate the next number.
;;   acme     -- prefix + 16-char `T_sssssTTTTRRRRC' local-name with
;;               a Crockford Base32 checksum.  See the `Acme local-
;;               name backend' section below.  Default scheme.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function org-id-uuid "org-id")

;;; ---------------------------------------------------------------------------
;;; Customization
;;; ---------------------------------------------------------------------------

(defgroup elot-id nil
  "Pluggable identifier minting and verification for ELOT."
  :prefix "elot-id-"
  :group 'elot)

(defcustom elot-id-default-scheme 'acme
  "Symbol naming the identifier scheme used when none is specified.
The named scheme must be registered via `elot-id-register-scheme'
before mint or verify calls reach it."
  :type 'symbol
  :group 'elot-id)

(defcustom elot-id-acme-type-letters
  '((class               . "C")    ; Class
    (annotation-property . "A")    ; Annotation property
    (object-property     . "R")    ; Relation (object property)
    (data-property       . "D")    ; Data property
    (individual          . "X")    ; indiXidual
    (datatype            . "T"))   ; Type (datatype; outside CARD-X)
  "Map ELOT entity kinds to the single uppercase TYPE letter for `acme'.
The default CARD-X mnemonic (C lass / A nnotation property /
R elation / D ata property / indi X idual) plus T for datatype
keeps every letter visually distinct and avoids overlap with
Crockford Base32's excluded glyphs."
  :type '(alist :key-type symbol :value-type string)
  :group 'elot-id)

(defcustom elot-id-acme-collision-retries 8
  "Maximum number of re-mints attempted when the `acme' scheme collides.
A collision is detected by comparing the freshly minted CURIE
against `:existing-iris' in CONTEXT.  When the retry budget is
exhausted, `elot-id-mint' signals `user-error'."
  :type 'integer
  :group 'elot-id)

(defcustom elot-id-counter-pad-width 6
  "Default zero-pad width for the built-in `counter' scheme.
A scheme may override per-instance via its `metadata' plist
\(key `:pad-width')."
  :type 'integer
  :group 'elot-id)

(defvar-local elot-id-scheme nil
  "Buffer-local override for the active identifier scheme.
When nil, resolution falls back to the Org file-local
`:elot-id-scheme:' property and then to `elot-id-default-scheme'.")

;;; ---------------------------------------------------------------------------
;;; Scheme registry
;;; ---------------------------------------------------------------------------

(cl-defstruct elot-id-scheme
  "Pluggable identifier-minting scheme.

NAME            -- symbol, user-facing.
DESCRIPTION     -- short string for tool descriptions and `customize'.
MINT-FN         -- (LABEL CONTEXT) -> CURIE string.
VERIFY-FN       -- (CURIE CONTEXT) -> t / nil.
COLLISION-CHECK-FN  -- optional (CURIE CONTEXT) -> t / nil.  When
                  non-nil it is consulted in addition to a generic
                  `:existing-iris' check.
MINT-BATCH-FN   -- optional (N CONTEXT LABELS) -> list of N CURIEs.
                  When nil, `elot-id-mint-batch' falls back to a
                  per-call loop that threads each freshly minted
                  CURIE into CONTEXT `:existing-iris' so the next
                  draw treats it as taken.  Schemes only need to
                  set this when they can do better than the loop
                  (e.g. a single SQL range reservation).
PREFIX          -- default CURIE prefix (string, no trailing colon).
METADATA        -- plist for scheme-specific options.

Scheme-specific runtime parameters (e.g. the counter template, the
acme `slug' flag) ride on CONTEXT under the key `:scheme-params',
which is a plist parsed from the trailing tokens of a scheme spec
string like \"counter GO_0000000\" or \"acme slug:t\".  See
`elot-id-parse-spec'."
  name
  description
  mint-fn
  verify-fn
  collision-check-fn
  mint-batch-fn
  prefix
  metadata)

(defvar elot-id-schemes nil
  "Alist (NAME-SYMBOL . SCHEME) of registered identifier schemes.")

(defun elot-id-register-scheme (scheme)
  "Register SCHEME (an `elot-id-scheme' struct) in `elot-id-schemes'.
Replaces any prior entry with the same `name'.  Returns SCHEME."
  (unless (elot-id-scheme-p scheme)
    (user-error "elot-id: not an elot-id-scheme: %S" scheme))
  (let ((name (elot-id-scheme-name scheme)))
    (unless (symbolp name)
      (user-error "elot-id: scheme name must be a symbol: %S" name))
    (setq elot-id-schemes
          (cons (cons name scheme)
                (assq-delete-all name elot-id-schemes))))
  scheme)

(defun elot-id-scheme-by-name (name)
  "Return the scheme registered under NAME, or signal `user-error'."
  (or (cdr (assq (if (stringp name) (intern name) name) elot-id-schemes))
      (user-error "elot-id: scheme not registered: %s" name)))

(defun elot-id--parse-spec-tokens (str)
  "Parse STR into a plist of scheme params.
Tokens of the form KEY:VALUE become plist entries (key as a
keyword, value as a string).  Bare positional tokens accumulate
under the key `:positional' as a list of strings, in order.
Returns nil when STR is nil or all-whitespace."
  (let (plist positional)
    (dolist (tok (and str (split-string str "[ \t]+" t)))
      (if (string-match "\\`\\([A-Za-z][A-Za-z0-9_-]*\\):\\(.*\\)\\'" tok)
          (setq plist (plist-put plist
                                 (intern (concat ":" (match-string 1 tok)))
                                 (match-string 2 tok)))
        (push tok positional)))
    (when positional
      (setq plist (plist-put plist :positional (nreverse positional))))
    plist))

(defun elot-id-parse-spec (spec)
  "Parse SPEC (a string or symbol) into (NAME-SYMBOL . PARAMS-PLIST).
SPEC has the form \"NAME\" or \"NAME TOKEN ...\".  A symbol is
treated as a bare scheme name.  Returns (nil . nil) on empty input."
  (cond
   ((null spec) (cons nil nil))
   ((symbolp spec) (cons spec nil))
   ((stringp spec)
    (let* ((trimmed (string-trim spec))
           (tokens (and (not (string-empty-p trimmed))
                        (split-string trimmed "[ \t]+" t))))
      (if (null tokens)
          (cons nil nil)
        (cons (intern (car tokens))
              (elot-id--parse-spec-tokens
               (mapconcat #'identity (cdr tokens) " "))))))
   (t (user-error "elot-id: cannot parse scheme spec: %S" spec))))

(defun elot-id-resolve-for-buffer (&optional buffer)
  "Resolve (SCHEME . PARAMS-PLIST) for BUFFER (defaults to current).
Resolution order:
  1. buffer-local `elot-id-scheme' (non-nil),
  2. Org file-local keyword `#+ELOT_ID_SCHEME: SPEC'
     or property `:elot-id-scheme: SPEC',
  3. `elot-id-default-scheme'.

SPEC may be either a bare scheme name or NAME followed by
scheme-specific parameter tokens (see `elot-id-parse-spec')."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((raw (or elot-id-scheme
                    (and (derived-mode-p 'org-mode)
                         (or (elot-id--org-keyword "ELOT_ID_SCHEME")
                             (elot-id--org-property "ELOT-ID-SCHEME")))
                    elot-id-default-scheme))
           (parsed (cond
                    ((elot-id-scheme-p raw)
                     (cons (elot-id-scheme-name raw) nil))
                    ((or (symbolp raw) (stringp raw))
                     (elot-id-parse-spec raw))
                    (t (user-error "elot-id: cannot resolve scheme: %S" raw))))
           (name (car parsed)))
      (unless name
        (user-error "elot-id: empty scheme spec"))
      (cons (elot-id-scheme-by-name name) (cdr parsed)))))

(defun elot-id-scheme-for-buffer (&optional buffer)
  "Resolve the active scheme for BUFFER (defaults to current).
Like `elot-id-resolve-for-buffer' but discards the params plist;
retained for back-compat with pre-spec callers."
  (car (elot-id-resolve-for-buffer buffer)))

(defun elot-id--org-keyword (kw)
  "Return the value of Org top-level keyword KW, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (re (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote kw))))
      (when (re-search-forward re nil t)
        (let ((v (match-string-no-properties 1)))
          (and v (not (string-empty-p (string-trim v)))
               (string-trim v)))))))

(defun elot-id--org-property (prop)
  "Return inherited Org PROP at point, or nil."
  (when (fboundp 'org-entry-get-with-inheritance)
    (ignore-errors (org-entry-get-with-inheritance prop))))

;;; ---------------------------------------------------------------------------
;;; Public mint / verify entry points
;;; ---------------------------------------------------------------------------

(defun elot-id--resolve-scheme (scheme-or-name)
  "Coerce SCHEME-OR-NAME to an `elot-id-scheme' struct."
  (cond
   ((null scheme-or-name) (elot-id-scheme-for-buffer))
   ((elot-id-scheme-p scheme-or-name) scheme-or-name)
   (t (elot-id-scheme-by-name scheme-or-name))))

(defun elot-id--context-prefix (scheme context)
  "Return the CURIE prefix to use for SCHEME under CONTEXT.
CONTEXT's `:prefix' overrides SCHEME's default `prefix' slot."
  (or (plist-get context :prefix)
      (elot-id-scheme-prefix scheme)
      (user-error "elot-id: no CURIE prefix in context or scheme")))

(defun elot-id--curie-collides-p (scheme curie context)
  "Return non-nil when CURIE collides under CONTEXT for SCHEME."
  (let ((existing (plist-get context :existing-iris))
        (extra-fn (elot-id-scheme-collision-check-fn scheme)))
    (or (and existing (member curie existing))
        (and extra-fn (funcall extra-fn curie context)))))

(defun elot-id-mint (scheme-or-name label &optional context)
  "Mint a fresh CURIE under SCHEME-OR-NAME for LABEL.
CONTEXT is a plist (see commentary).  Returns the CURIE string.
The scheme's mint-fn is responsible for collision handling; this
wrapper merely resolves the scheme and validates the result is a
non-empty string."
  (unless (and (stringp label) (not (string-empty-p label)))
    (user-error "elot-id: label must be a non-empty string"))
  (let* ((scheme (elot-id--resolve-scheme scheme-or-name))
         (fn (elot-id-scheme-mint-fn scheme))
         (curie (funcall fn label context)))
    (unless (and (stringp curie) (not (string-empty-p curie)))
      (user-error "elot-id: scheme %s returned an invalid CURIE: %S"
                  (elot-id-scheme-name scheme) curie))
    curie))

(defun elot-id--placeholder-labels (n context)
  "Synthesise N placeholder labels appropriate to CONTEXT `:kind'.
Used by `elot-id-mint-batch' when the caller supplies no LABELS
list.  Returns a list like (\"new-class-1\" \"new-class-2\" ...)."
  (let* ((kind (plist-get context :kind))
         (stem (cond
                ((null kind) "new-item")
                ((symbolp kind) (format "new-%s" kind))
                ((stringp kind) (format "new-%s" kind))
                (t "new-item"))))
    (cl-loop for i from 1 to n
             collect (format "%s-%d" stem i))))

(cl-defun elot-id-mint-batch (scheme-or-name n &optional context &key labels)
  "Mint N fresh CURIEs under SCHEME-OR-NAME, returning a list.
The batch is internally collision-free: each freshly minted CURIE
is prepended to CONTEXT `:existing-iris' before the next mint, so
counter monotone, slug suffixing, and acme collision retry all
behave as if the prior draws had already been written to disk.

LABELS, when supplied, must be a list of length N; each label is
passed to the corresponding mint call (so `slug' and `acme' can
derive label-derived slugs).  When omitted, placeholder labels
are synthesised from CONTEXT `:kind' via
`elot-id--placeholder-labels'.

A scheme may override the per-call loop by populating its
`mint-batch-fn' slot; that function is called as
  (FN N CONTEXT LABELS)
and must return a list of N non-empty CURIE strings.  The
framework still validates the count and shape of the result."
  (unless (and (integerp n) (> n 0))
    (user-error "elot-id: batch size must be a positive integer: %S" n))
  (let* ((scheme (elot-id--resolve-scheme scheme-or-name))
         (labels (or labels (elot-id--placeholder-labels n context))))
    (unless (= (length labels) n)
      (user-error "elot-id: labels length %d does not match batch size %d"
                  (length labels) n))
    (let ((batch-fn (elot-id-scheme-mint-batch-fn scheme))
          (result nil))
      (cond
       (batch-fn
        (setq result (funcall batch-fn n context labels))
        (unless (and (listp result) (= (length result) n))
          (user-error
           "elot-id: scheme %s mint-batch-fn returned %d items, expected %d"
           (elot-id-scheme-name scheme) (length result) n))
        (dolist (c result)
          (unless (and (stringp c) (not (string-empty-p c)))
            (user-error
             "elot-id: scheme %s mint-batch-fn returned invalid CURIE: %S"
             (elot-id-scheme-name scheme) c))))
       (t
        (let* ((ctx (copy-sequence context))
               (existing (append (plist-get ctx :existing-iris) nil)))
          (dolist (label labels)
            (setq ctx (plist-put ctx :existing-iris existing))
            (let ((curie (elot-id-mint scheme label ctx)))
              (push curie result)
              (push curie existing)))
          (setq result (nreverse result)))))
      result)))

(defun elot-id-verify (scheme-or-name curie &optional context)
  "Return t when CURIE is well-formed under SCHEME-OR-NAME, nil otherwise.
CONTEXT is forwarded to the scheme's verify-fn."
  (unless (and (stringp curie) (not (string-empty-p curie)))
    (user-error "elot-id: curie must be a non-empty string"))
  (let* ((scheme (elot-id--resolve-scheme scheme-or-name))
         (fn (elot-id-scheme-verify-fn scheme)))
    (and (funcall fn curie context) t)))

;;; ---------------------------------------------------------------------------
;;; CURIE helpers shared by built-in schemes
;;; ---------------------------------------------------------------------------

(defun elot-id--make-curie (prefix local)
  "Combine PREFIX and LOCAL into a CURIE string."
  (format "%s:%s" prefix local))

(defun elot-id-heading-curie-regexp (curie)
  "Return a regex matching an Org heading line declaring CURIE.
The heading shape is `* Label (CURIE)' (the ELOT convention).  The
regex tolerates the following decorations that Org users add to
heading lines, all of which trail the title proper:

  - one or more statistics cookies (`[N/M]' or `[N%]'),
  - an Org tag string (`:tag:tag2:'), roughly right-adjusted at
    end of line,
  - trailing whitespace.

TODO keywords (`TODO', `DONE', etc.) and priority cookies
(`[#A]') at the /start/ of the title are handled implicitly by
the `^\\*+ .*' prefix.  Group 0 covers the whole line; no
sub-groups are exposed (callers use the regex purely as an
anchor)."
  (concat "^\\*+ .*("
          (regexp-quote curie)
          ")"
          ;; Zero or more statistics cookies like [1/4] or [25%].
          "\\(?:[ \t]+\\[[0-9]+\\(?:/[0-9]+\\|%\\)\\]\\)*"
          ;; Optional Org tag string like ":foo:bar:".
          "\\(?:[ \t]+:[[:alnum:]_@#%:]+:\\)?"
          "[ \t]*$"))

(defun elot-id--curie-split (curie)
  "Split CURIE into (PREFIX . LOCAL).  Signals on malformed input."
  (unless (string-match "\\`\\([A-Za-z_][A-Za-z0-9_-]*\\)?:\\(.+\\)\\'" curie)
    (user-error "elot-id: not a CURIE: %s" curie))
  (cons (or (match-string 1 curie) "") (match-string 2 curie)))

(defun elot-id--curie-prefix-matches-p (curie prefix)
  "Return non-nil when CURIE's prefix equals PREFIX."
  (let ((parts (ignore-errors (elot-id--curie-split curie))))
    (and parts (string= (car parts) prefix))))

;;; ---------------------------------------------------------------------------
;;; Built-in scheme: uuid
;;; ---------------------------------------------------------------------------

(defconst elot-id--uuid-localname-regexp
  "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
  "Regexp matching the local-name portion of a UUID-scheme CURIE.")

(defun elot-id--uuid-mint (_label context)
  (require 'org-id)
  (let ((scheme (elot-id-scheme-by-name 'uuid)))
    (elot-id--make-curie (elot-id--context-prefix scheme context)
                         (downcase (org-id-uuid)))))

(defun elot-id--uuid-verify (curie _context)
  (let ((parts (ignore-errors (elot-id--curie-split curie)))
        (case-fold-search nil))
    (and parts
         (string-match-p elot-id--uuid-localname-regexp (cdr parts)))))

;;; ---------------------------------------------------------------------------
;;; Built-in scheme: slug
;;; ---------------------------------------------------------------------------

(defun elot-id--slugify (label)
  "Return a kebab-case ASCII slug derived from LABEL.
Folds case, strips diacritics where possible (via `replace-regexp-
in-string'), keeps a-z 0-9, joins runs of non-letters with `-'."
  (let* ((lower (downcase label))
         (ascii (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trim  (replace-regexp-in-string "\\(?:\\`-+\\|-+\\'\\)" "" ascii)))
    (if (string-empty-p trim) "x" trim)))

(defun elot-id--slug-disambiguate (curie existing)
  "Return CURIE or CURIE-N (smallest N >= 2) that avoids EXISTING."
  (if (not (member curie existing))
      curie
    (let ((n 2))
      (while (member (format "%s-%d" curie n) existing)
        (setq n (1+ n)))
      (format "%s-%d" curie n))))

(defun elot-id--slug-mint (label context)
  (let* ((scheme (elot-id-scheme-by-name 'slug))
         (prefix (elot-id--context-prefix scheme context))
         (slug   (elot-id--slugify label))
         (curie  (elot-id--make-curie prefix slug))
         (existing (plist-get context :existing-iris)))
    (elot-id--slug-disambiguate curie existing)))

(defun elot-id--slug-verify (curie _context)
  (let ((parts (ignore-errors (elot-id--curie-split curie)))
        (case-fold-search nil))
    (and parts
         (string-match-p "\\`[a-z0-9]+\\(?:-[a-z0-9]+\\)*\\'"
                         (cdr parts)))))

;;; ---------------------------------------------------------------------------
;;; Built-in scheme: counter
;;; ---------------------------------------------------------------------------

(defun elot-id--counter-pad-width (scheme)
  "Return the pad width configured for SCHEME (defaults to global)."
  (or (plist-get (elot-id-scheme-metadata scheme) :pad-width)
      elot-id-counter-pad-width))

(defun elot-id--counter-parse-template (template)
  "Parse TEMPLATE into (ALPHA . PAD-WIDTH).
TEMPLATE has the literal form of the desired local-name, e.g.
\"GO_0000000\" -> (\"GO_\" . 7), \"CHEBI00000\" -> (\"CHEBI\" . 5),
\"0000\" -> (\"\" . 4).  Signals on missing trailing digit run."
  (if (and (stringp template)
           (string-match "\\`\\(.*?\\)\\([0-9]+\\)\\'" template))
      (cons (match-string 1 template)
            (length (match-string 2 template)))
    (user-error
     "elot-id: counter template must end in digits: %S" template)))

(defun elot-id--counter-template (context scheme)
  "Return (ALPHA . PAD-WIDTH) for the counter scheme under CONTEXT.
When CONTEXT carries a `:scheme-params' plist whose `:template'
key -- or first bare positional token -- names a template literal,
that template defines both the alpha prefix and pad width.
Otherwise fall back to an empty alpha and the scheme's configured
pad width."
  (let* ((params (plist-get context :scheme-params))
         (template (or (plist-get params :template)
                       (car (plist-get params :positional)))))
    (if template
        (elot-id--counter-parse-template template)
      (cons "" (elot-id--counter-pad-width scheme)))))

(defun elot-id--counter-existing-max (prefix alpha existing)
  "Highest existing numeric local-name under PREFIX:ALPHA<digits> in EXISTING."
  (let ((max 0)
        (re (concat "\\`" (regexp-quote prefix) ":"
                    (regexp-quote alpha) "\\([0-9]+\\)\\'")))
    (dolist (e existing)
      (when (and (stringp e) (string-match re e))
        (let ((n (string-to-number (match-string 1 e))))
          (when (> n max) (setq max n)))))
    max))

(defun elot-id--counter-mint (_label context)
  (let* ((scheme (elot-id-scheme-by-name 'counter))
         (prefix (elot-id--context-prefix scheme context))
         (tpl    (elot-id--counter-template context scheme))
         (alpha  (car tpl))
         (pad    (cdr tpl))
         (existing (plist-get context :existing-iris))
         (next   (1+ (elot-id--counter-existing-max prefix alpha existing)))
         (local  (concat alpha (format (format "%%0%dd" pad) next))))
    (elot-id--make-curie prefix local)))

(defun elot-id--counter-verify (curie context)
  (let* ((scheme (elot-id-scheme-by-name 'counter))
         (tpl    (elot-id--counter-template context scheme))
         (alpha  (car tpl))
         (pad    (cdr tpl))
         (parts  (ignore-errors (elot-id--curie-split curie)))
         (prefix (and parts (car parts)))
         (local  (and parts (cdr parts)))
         (target-prefix (or (plist-get context :prefix)
                            (elot-id-scheme-prefix scheme))))
    (and parts
         (or (null target-prefix)
             (string= prefix target-prefix))
         (let ((case-fold-search nil))
           (string-match-p
            (format "\\`%s[0-9]\\{%d,\\}\\'" (regexp-quote alpha) pad)
            local)))))

;;; ---------------------------------------------------------------------------
;;; Acme local-name backend
;;;
;;; Mint and verify 16-character local names of the form
;;;
;;;     T_sssssTTTTRRRRC
;;;
;;;   T     -- single uppercase TYPE letter
;;;   _     -- literal separator
;;;   sssss -- 5-char lowercase a-z slug (non-letters stripped,
;;;            padded with `x' to length 5)
;;;   TTTT  -- 4-char Crockford Base32 day count since 2020-01-01 UTC
;;;   RRRR  -- 4-char Crockford Base32 random suffix (20 bits)
;;;   C     -- single Crockford Base32 check character (mod-32 sum
;;;            over the core, underscore ignored)
;;;
;;; `elot-acme-mint TYPE LABEL' returns the bare local name (no
;;; prefix); `elot-acme-verify ID-STR' returns t when ID-STR has the
;;; correct shape and a valid checksum.  Both functions are pure.
;;; ---------------------------------------------------------------------------

(defconst elot-acme-alph "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32 alphabet used by the `acme' identifier backend.")

(defvar elot-acme-val-table
  (let ((tbl (make-hash-table :test 'equal))
        (lower-alph "abcdefghijklmnopqrstuvwxyz"))
    ;; 1. Crockford Base32 (values 0-31).
    (dotimes (i (length elot-acme-alph))
      (puthash (substring elot-acme-alph i (1+ i)) i tbl))
    ;; 2. Lowercase slug characters (values 32-57).
    (dotimes (i (length lower-alph))
      (puthash (substring lower-alph i (1+ i)) (+ 32 i) tbl))
    ;; 3. The four uppercase TYPE characters absent from Crockford
    ;;    (values 58-61) so they participate in the checksum sum.
    (puthash "I" 58 tbl)
    (puthash "L" 59 tbl)
    (puthash "O" 60 tbl)
    (puthash "U" 61 tbl)
    tbl)
  "Character-to-value lookup table for the `acme' checksum.")

(defun elot-acme-slug5 (label)
  "Generate a 5-character ASCII slug from LABEL.
Lowercases, strips non-letters, and pads with `x' to length 5."
  (let* ((lower (downcase label))
         (letters (replace-regexp-in-string "[^a-z]" "" lower))
         (padded (concat letters "xxxxx")))
    (substring padded 0 5)))

(defun elot-acme-b32 (n width)
  "Encode integer N in Crockford Base32, left-padded to WIDTH chars."
  (if (= n 0)
      (make-string width ?0)
    (let ((result ""))
      (while (> n 0)
        (let ((r (mod n 32)))
          (setq result (concat (substring elot-acme-alph r (1+ r)) result))
          (setq n (/ n 32))))
      (concat (make-string (max 0 (- width (length result))) ?0) result))))

(defun elot-acme-randb32 (width)
  "Generate a random Crockford Base32 string of WIDTH characters."
  (let* ((bits (* width 5))
         (bytes (ceiling bits 8.0))
         (n 0))
    (dotimes (_ bytes)
      (setq n (+ (* n 256) (random 256))))
    (setq n (logand n (1- (expt 2 bits))))
    (elot-acme-b32 n width)))

(defun elot-acme-chk (core)
  "Return the single check character for CORE (underscore ignored)."
  (let ((sum 0))
    (dotimes (i (length core))
      (let ((char (substring core i (1+ i))))
        (unless (string= char "_")
          (let ((val (gethash char elot-acme-val-table)))
            (when val
              (setq sum (+ sum val)))))))
    (substring elot-acme-alph (mod (- 32 (mod sum 32)) 32)
               (1+ (mod (- 32 (mod sum 32)) 32)))))

(defun elot-acme-mint (type &optional label-en)
  "Mint a new bare acme local-name.
TYPE is a string whose first character (upcased) becomes the
leading TYPE letter.

When LABEL-EN is a non-empty string a 5-char slug derived from it
is interpolated, producing the 16-char form
  T_sssssTTTTRRRRC
When LABEL-EN is nil or empty the slug is omitted and the result
is the 11-char form
  T_TTTTRRRRC
The slugless form is shorter but carries no hint of the original
label; readability then relies entirely on `rdfs:label'."
  (let* ((epoch 1577836800)              ; 2020-01-01 00:00:00 UTC
         (now (float-time))
         (days (floor (/ (- now epoch) 86400)))
         (time-part (elot-acme-b32 days 4))
         (rand-part (elot-acme-randb32 4))
         (type-letter (upcase (substring type 0 1)))
         (core (if (and (stringp label-en) (not (string-empty-p label-en)))
                   (format "%s_%s%s%s"
                           type-letter
                           (elot-acme-slug5 label-en)
                           time-part
                           rand-part)
                 (format "%s_%s%s"
                         type-letter time-part rand-part))))
    (concat core (elot-acme-chk core))))

(defun elot-acme-verify (id-str)
  "Return t when ID-STR is a well-formed acme local-name with valid checksum.
Accepts both the 16-char slug form T_sssssTTTTRRRRC and the
11-char slugless form T_TTTTRRRRC."
  (and
   (stringp id-str)
   (let ((case-fold-search nil))
     (or (string-match-p
          "\\`[A-Z]_[a-z]\\{5\\}[0-9A-HJKMNP-TV-Z]\\{9\\}\\'" id-str)
         (string-match-p
          "\\`[A-Z]_[0-9A-HJKMNP-TV-Z]\\{9\\}\\'" id-str)))
   (let ((sum 0))
     (dotimes (i (length id-str))
       (let ((char (substring id-str i (1+ i))))
         (unless (string= char "_")
           (let ((val (gethash char elot-acme-val-table)))
             (when val
               (setq sum (+ sum val)))))))
     (= (mod sum 32) 0))))

;;; ---------------------------------------------------------------------------
;;; Built-in scheme: acme
;;; ---------------------------------------------------------------------------

(defun elot-id--acme-type-letter (kind)
  "Return the single TYPE letter for KIND (defaulting to class).
Signals when KIND is recognised by ELOT but absent from
`elot-id-acme-type-letters'."
  (let* ((k (cond ((null kind) 'class)
                  ((symbolp kind) kind)
                  ((stringp kind) (intern kind))
                  (t (user-error "elot-id: bad :kind argument: %S" kind))))
         (letter (cdr (assq k elot-id-acme-type-letters))))
    (unless letter
      (user-error
       "elot-id: no acme TYPE letter for kind %s -- update `elot-id-acme-type-letters'"
       k))
    letter))

(defun elot-id--acme-slug-enabled-p (context)
  "Return non-nil when the acme scheme should include a label slug.
Reads CONTEXT's `:scheme-params' plist key `:slug'; default is nil
(slugless 11-char form).  String values `nil', `false', `no',
`0', `off' (case-insensitive) read as nil; everything else (`t',
`yes', `1', ...) reads as true."
  (let* ((params (plist-get context :scheme-params))
         (v (plist-get params :slug)))
    (cond
     ((null v) nil)
     ((stringp v)
      (not (member (downcase v) '("nil" "false" "no" "0" "off" ""))))
     (t (and v t)))))

(defun elot-id--acme-mint (label context)
  (let* ((scheme (elot-id-scheme-by-name 'acme))
         (prefix (elot-id--context-prefix scheme context))
         (kind   (plist-get context :kind))
         (type   (elot-id--acme-type-letter kind))
         (slug-enabled (elot-id--acme-slug-enabled-p context))
         (slug-label (and slug-enabled label))
         (retries (max 0 elot-id-acme-collision-retries))
         (attempt 0)
         (curie  nil))
    (catch 'done
      (while (<= attempt retries)
        (let* ((local (elot-acme-mint type slug-label))
               (candidate (elot-id--make-curie prefix local)))
          (unless (elot-id--curie-collides-p scheme candidate context)
            (setq curie candidate)
            (throw 'done curie)))
        (setq attempt (1+ attempt)))
      (user-error
       "elot-id: acme collision budget exhausted (slug=%s, label=%s); try a more specific label"
       (and slug-enabled (stringp label) (elot-acme-slug5 label))
       label))
    ;; Slug-loss disclosure only meaningful when slug is enabled.
    (when slug-enabled
      (let* ((slug (elot-acme-slug5 label))
             (degenerate (or (string= slug "xxxxx")
                             (string-match-p "x\\{2,\\}\\'" slug))))
        (when degenerate
          (put-text-property 0 (length curie) 'elot-id-acme-slug-warning
                             (format "slug %S carries little label hint" slug)
                             curie))))
    curie))

(defun elot-id--acme-verify (curie _context)
  (let ((parts (ignore-errors (elot-id--curie-split curie))))
    (and parts (elot-acme-verify (cdr parts)))))

;;; ---------------------------------------------------------------------------
;;; Built-in scheme registration
;;; ---------------------------------------------------------------------------

(defun elot-id--register-builtin-schemes ()
  "Register the four built-in identifier schemes.
Idempotent: replaces any prior registration with the same name."
  (elot-id-register-scheme
   (make-elot-id-scheme
    :name        'uuid
    :description "RFC 4122 UUID (lowercase 32-char hex) as the local name."
    :mint-fn     #'elot-id--uuid-mint
    :verify-fn   #'elot-id--uuid-verify
    :prefix      "ex"
    :metadata    nil))
  (elot-id-register-scheme
   (make-elot-id-scheme
    :name        'slug
    :description "Kebab-case ASCII slug from the label, suffix-disambiguated."
    :mint-fn     #'elot-id--slug-mint
    :verify-fn   #'elot-id--slug-verify
    :prefix      "ex"
    :metadata    nil))
  (elot-id-register-scheme
   (make-elot-id-scheme
    :name        'counter
    :description "OBO-style prefix + zero-padded numeric suffix."
    :mint-fn     #'elot-id--counter-mint
    :verify-fn   #'elot-id--counter-verify
    :prefix      "ex"
    :metadata    nil))
  (elot-id-register-scheme
   (make-elot-id-scheme
    :name        'acme
    :description "Timestamp+random+checksum localname (elot-acme-mint)."
    :mint-fn     #'elot-id--acme-mint
    :verify-fn   #'elot-id--acme-verify
    :prefix      "ex"
    :metadata    nil)))

;; Seed Emacs's PRNG from system entropy so a fresh batch session
;; does not produce predictable acme random suffixes.
(random t)

(elot-id--register-builtin-schemes)

(provide 'elot-id)
;;; elot-id.el ends here
