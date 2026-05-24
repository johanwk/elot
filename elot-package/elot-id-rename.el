;;; elot-id-rename.el --- Rename a resource CURIE everywhere it appears  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Kluwer

;; Author: Johan W. Kluwer
;; URL: https://github.com/johanwk/elot
;; Keywords: languages tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.4 -- substitute one
;; resource identifier (CURIE) for another, everywhere it appears in
;; the ontology source.
;;
;; Public entry points:
;;
;;   M-x elot-rename-resource           ; prompts; point on a resource
;;   (elot-rename-resource SOURCE TARGET &optional TARGET-IRI)
;;
;; Rewrite scope (per Step 12.4 of the plan):
;;
;;   - heading parenthetical `(prefix:local)' on the source heading;
;;   - CURIE tokens in description-list rows (`- key :: value' on a
;;     single leading space, plus indented annotation rows under OMN
;;     keywords such as `Domain ::', `SubClassOf:');
;;   - description-list /tag/ position (when the source is a property
;;     used elsewhere as a key);
;;   - src-block bodies whose language is `omn' or `sparql';
;;   - full-IRI occurrences (angle-bracketed `<http://...>' and bare
;;     `http://...' as annotation values) that expand to the source's
;;     IRI under the buffer's prefix table -- rewritten to the
;;     /full IRI form/ of the target, preserving angle vs bare.
;;
;; Out of scope:
;;
;;   - prefix-table rows (the table is reference data, not subject);
;;   - prose mentions (running text outside the description-list /
;;     code-like span set) -- counted and reported as a NOTE so the
;;     author can audit;
;;   - other src-block languages (`ttl', `sparql' .. -- actually we
;;     do touch `sparql' and `omn'; everything else is left alone);
;;   - cross-file occurrences (single buffer only -- M12.3-cousin).
;;
;; Prefix-resolution policy:
;;
;;   When TARGET's prefix is not declared in the buffer's prefix
;;   table, the rename consults `elot-db' (`global_prefixes' + every
;;   source's `prefixes' table) for candidate IRIs.  Interactive
;;   path prompts the user (single candidate -> `y-or-n-p';
;;   multiple -> `completing-read'; none -> `read-string').
;;   Programmatic / LLM path requires TARGET-IRI explicitly; without
;;   it `elot-rename-resource' signals a `user-error' carrying the
;;   candidate list.  Whichever resolves the IRI, the resulting
;;   (prefix . iri) row is added to the buffer's prefix table inside
;;   the SAME `atomic-change-group' as the CURIE rewrite.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-table)
(require 'elot-id nil 'noerror)
(require 'elot-tangle nil 'noerror)
;; For the interactive form's label-lookup completion + CURIE-at-point
;; probe.  Loaded lazily; absent in batch tests is tolerated by the
;; `fboundp' guards in the `(interactive ...)' spec below.
(require 'elot-id-move nil 'noerror)
;; For the interactive form's `mint a fresh target' branch -- pulls in
;; scheme/prefix/section helpers and `elot-id-mint'.  Optional load so
;; standalone byte-compile still works; `fboundp' guards cover the
;; runtime fallback.
(require 'elot-id-insert nil 'noerror)

(declare-function elot-db-expand-curie "elot-db" (curie &optional active-sources))
(declare-function elot-id-move--read-curie "elot-id-move"
                  (prompt &optional include-top initial))
(declare-function elot-id-move--section-kind-at-point "elot-id-move" ())
(declare-function elot-id-insert--scheme-spec "elot-id-insert" ())
(declare-function elot-id-insert--prefix "elot-id-insert" ())
(declare-function elot-id-insert--section-name "elot-id-insert" ())
(declare-function elot-id-insert--existing-iris "elot-id-insert" ())
(declare-function elot-id-parse-spec "elot-id" (spec))
(declare-function elot-id-mint "elot-id" (scheme-or-name label &optional context))

;; M9.3.F8: cache-staleness helpers.
(declare-function elot-headline-hierarchy-mark-stale "elot-tangle" ())
(declare-function elot-headline-hierarchy-ensure-fresh "elot-tangle" ())
(declare-function elot-update-headline-hierarchy "elot-tangle" ())
(defvar elot-headline-hierarchy)
(defvar elot-id-insert--section-kind-alist)

(defvar elot-db) ;; from elot-db.el; bound only when the DB is loaded.

;;; ---------------------------------------------------------------------------
;;; CURIE shape + boundary regexes
;;; ---------------------------------------------------------------------------

(defconst elot-id-rename--curie-re
  "\\`\\([A-Za-z_][A-Za-z0-9_-]*\\):\\([A-Za-z_][A-Za-z0-9_.-]*\\)\\'"
  "Strict CURIE shape, used for source/target validation.")

(defun elot-id-rename--split-curie (curie)
  "Return (PREFIX . LOCAL) for CURIE.  Signal `user-error' on malformed."
  (unless (and (stringp curie) (string-match elot-id-rename--curie-re curie))
    (user-error "elot-rename-resource: not a valid CURIE: %S" curie))
  (cons (match-string 1 curie) (match-string 2 curie)))

(defun elot-id-rename--token-boundary-regex (curie)
  "Return a regex matching CURIE as a whole CURIE token.
Group 1 captures the boundary character preceding the match (or
empty at BOL); group 2 the boundary following (or empty at EOL).
Boundary chars are anything outside the CURIE-name character set
\(letters, digits, `_', `-', `.', `:').  The surrounding chars are
captured rather than discarded so callers can preserve them in
the replacement."
  (concat "\\(^\\|[^A-Za-z0-9_:-]\\)"
          (regexp-quote curie)
          "\\([^A-Za-z0-9_:-]\\|$\\)"))

;;; ---------------------------------------------------------------------------
;;; Prefix table read / mutate
;;; ---------------------------------------------------------------------------

(defun elot-id-rename--goto-prefix-table ()
  "Move point to the first data row of the `prefix-table' table.
Returns t on success, nil when no such table is in the buffer.
The function leaves point on the first `|'-row line (which is the
header row in ELOT's convention)."
  (goto-char (point-min))
  (when (re-search-forward "^#\\+name:[ \t]*prefix-table\\b" nil t)
    (forward-line 1)
    (while (and (not (eobp))
                (looking-at "^[ \t]*\\(?:#\\+\\|$\\)"))
      (forward-line 1))
    (and (looking-at "^[ \t]*|") t)))

(defun elot-id-rename--read-prefix-table ()
  "Return the buffer's prefix-table as an alist of (PREFIX . IRI).
PREFIX is the bare prefix name (no trailing colon).  The header
row (where the prefix column is literally \"prefix\") and any
empty/separator rows are filtered out.  Returns nil when no
named prefix-table is present in the buffer."
  (save-excursion
    (when (elot-id-rename--goto-prefix-table)
      (let* ((rows (org-table-to-lisp))
             acc)
        (dolist (row rows)
          (when (and (listp row) (>= (length row) 2))
            (let* ((p-raw (string-trim (nth 0 row)))
                   (iri   (string-trim (nth 1 row)))
                   (p     (replace-regexp-in-string ":\\'" "" p-raw)))
              (unless (or (string-empty-p p)
                          (string-empty-p iri)
                          (string= p "prefix"))
                (push (cons p iri) acc)))))
        (nreverse acc)))))

(defun elot-id-rename--add-prefix-row (prefix iri)
  "Append `| PREFIX: | IRI |' as the last row of the prefix-table.
Re-aligns the table afterwards.  Signals when no prefix-table is
in the buffer (callers should check `--read-prefix-table' first)."
  (save-excursion
    (unless (elot-id-rename--goto-prefix-table)
      (error "elot-rename-resource: no prefix-table found in buffer"))
    ;; Move to end of table.
    (while (and (not (eobp)) (looking-at "^[ \t]*|"))
      (forward-line 1))
    (forward-line -1)
    (end-of-line)
    (insert (format "\n| %s: | %s |" prefix iri))
    (org-table-align)))

;;; ---------------------------------------------------------------------------
;;; IRI lookup
;;; ---------------------------------------------------------------------------

(defun elot-id-rename--expand-curie (curie prefix-table)
  "Return CURIE's full IRI via PREFIX-TABLE, or nil when undeclared."
  (let* ((parts (elot-id-rename--split-curie curie))
         (p (car parts))
         (local (cdr parts))
         (iri (cdr (assoc p prefix-table))))
    (and iri (concat iri local))))

(defun elot-id-rename--db-candidates (prefix)
  "Return distinct IRI expansions for PREFIX from `elot-db', as a list.
Consults `global_prefixes' UNION every source's `prefixes' table.
Returns nil when `elot-db' is not loaded or the database is not
open -- the caller treats that the same as `no candidates'."
  (when (and (boundp 'elot-db) elot-db (fboundp 'sqlite-select))
    (let ((rows
           (condition-case _err
               (sqlite-select
                elot-db
                "SELECT DISTINCT expansion FROM (
                   SELECT expansion FROM global_prefixes WHERE prefix = ?
                   UNION
                   SELECT expansion FROM prefixes        WHERE prefix = ?)"
                (list prefix prefix))
             (error nil))))
      (delete-dups (mapcar #'car rows)))))

(defun elot-id-rename--resolve-target-iri (prefix candidates target-iri)
  "Resolve the IRI to associate with PREFIX.
Returns the chosen IRI string.  CANDIDATES is the DB-derived
list (may be empty).  TARGET-IRI, when non-nil, short-circuits
the resolution.  When neither is supplied and we are running
non-interactively, signal a `user-error' carrying the candidate
list so the LLM wrapper can surface it."
  (cond
   ((and target-iri (stringp target-iri) (not (string-empty-p target-iri)))
    target-iri)
   ((and noninteractive (null candidates))
    (user-error
     "elot-rename-resource: undeclared prefix `%s:'; no candidates in elot-db; supply :target-iri"
     prefix))
   ((null candidates)
    (read-string (format "IRI expansion for `%s:': " prefix)))
   ((= (length candidates) 1)
    (if (or noninteractive
            (y-or-n-p
             (format "Add prefix `%s:' -> <%s> to this ontology? "
                     prefix (car candidates))))
        (car candidates)
      (user-error "elot-rename-resource: cancelled")))
   (t
    (if noninteractive
        (user-error
         "elot-rename-resource: undeclared prefix `%s:'; candidates: %s; supply :target-iri"
         prefix (mapconcat (lambda (i) (format "<%s>" i)) candidates " | "))
      (completing-read
       (format "IRI expansion for `%s:': " prefix) candidates nil t)))))

;;; ---------------------------------------------------------------------------
;;; Source-heading lookup + collision detection
;;; ---------------------------------------------------------------------------

(defun elot-id-rename--declared-curies ()
  "Return every CURIE declared as a resource heading in the buffer.
Reads `elot-headline-hierarchy' (refreshes it on demand via
`elot-headline-hierarchy-ensure-fresh' so this picks up freshly
inserted / moved / renamed headings written by earlier mutation
primitives in the same session), walks depth-first, collects
`:uri' values (first whitespace-separated token, in case the URI
is an ontology+version composite)."
  (cond
   ((fboundp 'elot-headline-hierarchy-ensure-fresh)
    (elot-headline-hierarchy-ensure-fresh))
   ((and (not (bound-and-true-p elot-headline-hierarchy))
         (fboundp 'elot-update-headline-hierarchy))
    (ignore-errors (elot-update-headline-hierarchy))))
  (let (acc)
    (cl-labels ((walk (n)
                  (let ((uri (plist-get n :uri)))
                    (when (and uri (stringp uri))
                      (let ((tok (car (split-string uri "[ \t]+" t))))
                        (when (and tok (string-match elot-id-rename--curie-re tok))
                          (push tok acc)))))
                  (mapc #'walk (plist-get n :children))))
      (let ((h (bound-and-true-p elot-headline-hierarchy)))
        (when h (walk h))))
    (nreverse acc)))

(defun elot-id-rename--heading-marker-for-curie (curie)
  "Return a marker at the resource heading declaring CURIE, or nil.
Searches the buffer for a heading shaped `* Label (CURIE)' (the
ELOT convention)."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (elot-id-heading-curie-regexp curie) nil t)
      (copy-marker (line-beginning-position)))))

(defun elot-id-rename--rewrite-heading-label (curie new-label)
  "Rewrite the label portion of the heading declaring CURIE to NEW-LABEL.

The heading is located via the shared tolerant regex
`elot-id-heading-curie-regexp', so trailing statistics cookies
and tags survive untouched.  Only the label text between the
leading stars (plus a space) and the ` (CURIE)' parenthetical is
replaced; everything from `(CURIE)' onward (cookie, tags,
trailing whitespace) is preserved verbatim.  Returns non-nil on
success; signals `user-error' when the heading cannot be found."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward
             (elot-id-heading-curie-regexp curie) nil t)
      (user-error
       "elot-id-rename: cannot locate heading for %s to rewrite label"
       curie))
    (beginning-of-line)
    ;; Match: stars + space + LABEL + ` (CURIE)' + tail.  LABEL is
    ;; captured non-greedily so the rightmost `(CURIE)' on the line
    ;; is the one that anchors the split.
    (let ((line-re (concat "^\\(\\*+ \\)\\(.*?\\)\\([ \t]+("
                           (regexp-quote curie) ")\\)")))
      (unless (re-search-forward line-re (line-end-position) t)
        (user-error
         "elot-id-rename: heading for %s has unexpected shape; label not rewritten"
         curie))
      (replace-match (concat (match-string 1) new-label (match-string 3))
                     t t))))

;;; ---------------------------------------------------------------------------
;;; Region classification + rewriters
;;; ---------------------------------------------------------------------------

;; `elot-id-remove-heading-promote-children' lives in `elot-id-move'
;; (the outline-surgery sibling module).  Declared here so the byte
;; compiler is happy when this file is compiled stand-alone; the real
;; definition arrives via the (require 'elot-id-move) above.
(declare-function elot-id-remove-heading-promote-children
                  "elot-id-move" (marker))

(defconst elot-id-rename--rewritable-src-langs
  '("omn" "sparql")
  "Src-block languages whose bodies are subject to CURIE rewrite.")

(defun elot-id-rename--line-class ()
  "Classify the current line for rewrite purposes.
Returns one of:
  `desc-list'   -- a top-level description-list row (` - key :: value');
  `desc-list-nested' -- an indented annotation row under one;
  `heading'     -- an Org heading line;
  `prefix-row'  -- a row of the prefix-table;
  `src-rewrite' -- inside a src block whose language is in
                   `elot-id-rename--rewritable-src-langs';
  `src-other'   -- inside any other src block (skipped);
  `prose'       -- everything else (skipped, counted)."
  (cond
   ((save-excursion (beginning-of-line) (looking-at "^\\*+ ")) 'heading)
   ((save-excursion
      (beginning-of-line)
      (looking-at "^[ \t]+-[ \t]"))
    'desc-list-nested)
   ((save-excursion (beginning-of-line) (looking-at "^ -[ \t]"))
    'desc-list)
   ((elot-id-rename--in-prefix-table-p) 'prefix-row)
   ((let ((info (org-element-context)))
      (and info
           (memq (org-element-type info) '(src-block inline-src-block))
           (let ((lang (or (org-element-property :language info) "")))
             (if (member (downcase lang)
                         elot-id-rename--rewritable-src-langs)
                 'src-rewrite
               'src-other)))))
   (t 'prose)))

(defun elot-id-rename--in-prefix-table-p ()
  "Return non-nil when point is on a row of the named `prefix-table'.
Walks backward over contiguous `|'-rows and `#+'-affiliated lines;
returns t when a `#+name: prefix-table' affiliated keyword is
found at the head of the table block."
  (save-excursion
    (beginning-of-line)
    (and (looking-at "^[ \t]*|")
         (let (found done)
           (while (not done)
             (forward-line -1)
             (cond
              ((bobp) (setq done t))
              ((looking-at "^[ \t]*\\(?:|\\|#\\+\\)")
               (when (looking-at "^#\\+name:[ \t]*prefix-table\\b")
                 (setq found t done t)))
              (t (setq done t))))
           found))))

(defconst elot-id-rename--curie-char-re "[A-Za-z0-9_:-]"
  "Character class for chars that may appear inside a CURIE token
for boundary-detection purposes.  The boundary check rejects
matches whose immediate neighbours are in this class (longer-
token cases like `ex:dogfood').  Note: `.' is intentionally
EXCLUDED -- a trailing period (sentence-end in prose, or after
an annotation value) should terminate a CURIE token.  CURIEs
whose local-name contains an internal `.' (e.g. `ex:foo.bar')
still match correctly as long as they are queried whole; partial
overlap with a longer dotted token is the rare edge case we
accept in exchange for sentence-end correctness.")

(defun elot-id-rename--curie-char-p (ch)
  "Return non-nil when CH is a CURIE-name character."
  (and ch (string-match-p elot-id-rename--curie-char-re
                          (char-to-string ch))))

(defun elot-id-rename--rewrite-curie-in-string (s source target)
  "Return a copy of S with whole-token SOURCE replaced by TARGET.
Boundary-aware: skips occurrences that are part of a longer CURIE
or local-name token.  Implementation uses plain `string-match' on
the bare SOURCE and checks neighbouring characters explicitly --
the boundary chars are NOT consumed by the regex, so two adjacent
CURIEs (`ex:dog ex:dog') and CURIEs at BOL/EOL are handled
uniformly."
  (let* ((quoted (regexp-quote source))
         (len (length s))
         (out "")
         (pos 0))
    (while (and (<= pos len) (string-match quoted s pos))
      (let* ((mb (match-beginning 0))
             (me (match-end 0))
             (before (and (> mb 0) (aref s (1- mb))))
             (after  (and (< me len) (aref s me)))
             (token-boundary
              (and (not (elot-id-rename--curie-char-p before))
                   (not (elot-id-rename--curie-char-p after)))))
        (if token-boundary
            (setq out (concat out (substring s pos mb) target)
                  pos me)
          (setq out (concat out (substring s pos me))
                pos me))))
    (concat out (substring s pos))))

(defun elot-id-rename--rewrite-line (source target)
  "Replace SOURCE with TARGET on the current line (whole-token).
Returns the number of replacements made on this line."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (orig (buffer-substring-no-properties bol eol))
         (rewritten (elot-id-rename--rewrite-curie-in-string
                     orig source target)))
    (if (string= orig rewritten)
        0
      (delete-region bol eol)
      (goto-char bol)
      (insert rewritten)
      ;; Count by comparing string lengths is unreliable; do a fresh
      ;; count via the regex.
      (let ((n 0) (start 0)
            (re (elot-id-rename--token-boundary-regex source)))
        (while (string-match re orig start)
          (setq n (1+ n)
                start (- (match-end 0)
                         (length (match-string 2 orig)))))
        n))))

;;; ---------------------------------------------------------------------------
;;; Full-IRI rewriting
;;; ---------------------------------------------------------------------------

(defconst elot-id-rename--iri-angle-re
  "<\\(https?://[^> \t\n]+\\)>")

(defconst elot-id-rename--iri-bare-re
  ;; Match a bare IRI in a context where one is plausible (RHS of `::',
  ;; whitespace-bounded).  We restrict bare matches to description-list
  ;; lines to avoid touching arbitrary prose.
  "\\(\\`\\| \\|\t\\)\\(https?://[^ \t\n<>]+\\)")

(defun elot-id-rename--rewrite-full-iris-in-region
    (beg end source-iri target-iri)
  "Replace SOURCE-IRI with TARGET-IRI between BEG and END.
Handles both `<...>' and bare forms; returns the replacement count."
  (let ((n 0))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward elot-id-rename--iri-angle-re end t)
        (when (string= (match-string-no-properties 1) source-iri)
          (replace-match (format "<%s>" target-iri) t t)
          (setq n (1+ n)))))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward elot-id-rename--iri-bare-re end t)
        (when (string= (match-string-no-properties 2) source-iri)
          (replace-match (concat (match-string 1) target-iri) t t)
          (setq n (1+ n)))))
    n))

;;; ---------------------------------------------------------------------------
;;; Buffer-wide rewrite driver
;;; ---------------------------------------------------------------------------

(defun elot-id-rename--rewrite-buffer (source target source-iri target-iri)
  "Perform the buffer-wide CURIE + full-IRI rewrite.
Returns a plist with counts: `:curie-count' (number of CURIE
rewrites), `:iri-count' (full-IRI rewrites), `:prose-skipped'
\(prose lines containing SOURCE that were left alone)."
  (let ((curie-count 0)
        (iri-count 0)
        (prose-skipped 0))
    ;; Pass 1: line-classified CURIE rewrite.  Walk every line; only
    ;; the lines whose class is rewritable get touched.
    (save-excursion
      (goto-char (point-min))
      (let ((re (elot-id-rename--token-boundary-regex source)))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (line (buffer-substring-no-properties bol eol))
                 (has-source (string-match-p re line)))
            (when has-source
              (let ((class (elot-id-rename--line-class)))
                (cond
                 ((memq class '(desc-list desc-list-nested heading
                                          src-rewrite))
                  (cl-incf curie-count
                           (elot-id-rename--rewrite-line source target)))
                 ((memq class '(prefix-row src-other))
                  nil)
                 (t
                  (cl-incf prose-skipped)))))
            (forward-line 1)))))
    ;; Pass 2: full-IRI rewrite (whole buffer; the IRI regex is
    ;; already prose-safe because it only matches inside `<...>' or
    ;; in whitespace-bounded annotation values; we further restrict
    ;; bare matches via the regex anchor).
    (when (and source-iri target-iri (not (string= source-iri target-iri)))
      (setq iri-count
            (elot-id-rename--rewrite-full-iris-in-region
             (point-min) (point-max) source-iri target-iri)))
    (list :curie-count curie-count
          :iri-count iri-count
          :prose-skipped prose-skipped)))

;;; ---------------------------------------------------------------------------
;;; Public entry point
;;; ---------------------------------------------------------------------------

;;;###autoload
(cl-defun elot-rename-resource (source target &key target-iri new-label
                                                   (op 'rename))
  "Rename declared resource SOURCE (CURIE) to TARGET (CURIE).

OP selects the rewrite mode (Slice A.3, briefing
`9.9.A.3-rename-merge-mode.org'):

  `rename' (default) -- the historical behaviour.  TARGET must
    /not/ already be declared in the buffer; collisions are
    refused.  Used by the canonical `rewrite-and-declare' path
    where TARGET is a fresh identifier.

  `merge' -- the inverse refusal.  TARGET /must/ already be
    declared; SOURCE's references are rewritten to TARGET and
    SOURCE's own heading is removed (its heading-nested children
    promoted one outline level).  Used by `elot_replace_with_parent'
    and any future caller that needs to collapse two co-extensive
    identifiers.  TARGET's prefix is therefore already in the
    buffer's prefix table by construction; no prefix-row addition
    is scheduled in this mode.


When NEW-LABEL is non-nil and differs from the current heading
label, the resource-declaring heading's title is also rewritten
to carry NEW-LABEL (inside the same atomic-change-group as the
CURIE rewrite).  This lets the interactive caller change the
rdfs:label in the same operation as the identifier change --
typical when the mint path prompts for a fresh label.

Rewrites every CURIE occurrence of SOURCE -- in heading
parentheticals, description-list rows (tag and value positions
including OMN axiom bodies under `Domain ::', `SubClassOf:'),
and src-block bodies whose language is `omn' or `sparql' -- to
TARGET.  Full-IRI occurrences (`<...>' or bare in annotation
values) that expand to SOURCE's IRI under the buffer's prefix
table are rewritten to the full IRI form of TARGET, preserving
the angle-vs-bare shape.  Prefix-table rows, other src-block
languages, and running prose are left alone; the count of prose
lines containing SOURCE is reported as a NOTE so the author
can audit.

When TARGET's prefix is not declared in the buffer's prefix
table, the resolved IRI is added as a new row in the same
`atomic-change-group' as the rewrite.  Resolution order:
  TARGET-IRI when supplied (programmatic / LLM path);
  exactly one `elot-db' candidate -> `y-or-n-p' confirmation;
  several candidates -> `completing-read';
  none -> `read-string' for a free-form IRI;
  non-interactive without TARGET-IRI -> `user-error' carrying
    the candidate list (the LLM wrapper surfaces it).

The whole operation runs inside a single
`atomic-change-group' -- a failure during rewrite or the
prefix-row addition rolls everything back, including any
already-applied substitutions.

Returns a plist:
  (:source S :target T :curie-count N :iri-count M
   :prose-skipped K :declared-prefix (PREFIX . IRI) | nil)"
  (interactive
   (let* ((curie-at-point
           (or
            ;; Preferred: the CURIE on the enclosing resource heading,
            ;; so the pre-selection works from anywhere inside the
            ;; subtree (body text, description-list rows, sub-headings).
            (and (fboundp 'elot-id-move--section-kind-at-point)
                 (save-excursion
                   (when (ignore-errors (org-back-to-heading t) t)
                     (catch 'curie
                       (while (org-current-level)
                         (let ((title (org-get-heading t t t t)))
                           (when (and title
                                      (string-match
                                       "(\\([A-Za-z_][A-Za-z0-9_-]*:[A-Za-z_][A-Za-z0-9_.-]*\\))"
                                       title))
                             (let ((c (match-string-no-properties 1 title)))
                               (when (elot-id-move--section-kind-at-point)
                                 (throw 'curie c)))))
                         (unless (org-up-heading-safe)
                           (throw 'curie nil)))
                       nil))))
            ;; Fallback: literal CURIE-shaped text under point.
            (save-excursion
              (skip-chars-backward "A-Za-z0-9_.:-")
              (and (looking-at
                    "\\([A-Za-z_][A-Za-z0-9_-]*\\):[A-Za-z_][A-Za-z0-9_.-]*")
                   (match-string-no-properties 0)))))
          (tgt-new-label nil)
          (src (if (fboundp 'elot-id-move--read-curie)
                   (elot-id-move--read-curie
                    (format "Source (label or CURIE)%s: "
                            (if curie-at-point
                                (format " [default %s]" curie-at-point) ""))
                    nil curie-at-point)
                 (read-string
                  (format "Source CURIE%s: "
                          (if curie-at-point
                              (format " (default %s)" curie-at-point) ""))
                  nil nil curie-at-point)))
          ;; Target: no completion (a fresh CURIE must not collide with
          ;; an existing declaration, so a label picker would never
          ;; yield a usable value).  Offer two paths:
          ;;   (a) mint a fresh CURIE under the ontology's declared
          ;;       `:ELOT-id-scheme:' -- prompts only for a label,
          ;;       reuses the same scheme/prefix/existing-IRIs context
          ;;       that `elot-insert-*-resource' uses;
          ;;   (b) type a CURIE manually (read-string, no completion).
          (tgt
           (let* ((spec
                   (and (fboundp 'elot-id-insert--scheme-spec)
                        (save-excursion
                          (let ((m (and (fboundp
                                         'elot-id-rename--heading-marker-for-curie)
                                        (ignore-errors
                                          (elot-id-rename--heading-marker-for-curie
                                           src)))))
                            (when m (goto-char m)))
                          (elot-id-insert--scheme-spec))))
                  (offer-mint (and spec
                                   (fboundp 'elot-id-mint)
                                   (fboundp 'elot-id-parse-spec)))
                  (mint? (and offer-mint
                              (y-or-n-p
                               (format "Mint a fresh CURIE for %s under scheme `%s'? "
                                       src spec)))))
             (if mint?
                 (save-excursion
                   (let ((m (and (fboundp
                                  'elot-id-rename--heading-marker-for-curie)
                                 (ignore-errors
                                   (elot-id-rename--heading-marker-for-curie
                                    src)))))
                     (when m (goto-char m)))
                   (let* ((parsed (elot-id-parse-spec spec))
                          (scheme-name (car parsed))
                          (scheme-params (cdr parsed))
                          (prefix (and (fboundp 'elot-id-insert--prefix)
                                       (elot-id-insert--prefix)))
                          (existing (and (fboundp 'elot-id-insert--existing-iris)
                                         (elot-id-insert--existing-iris)))
                          (section (and (fboundp 'elot-id-insert--section-name)
                                        (elot-id-insert--section-name)))
                          (kind (and section
                                     (boundp 'elot-id-insert--section-kind-alist)
                                     (cdr (assoc section
                                                 elot-id-insert--section-kind-alist))))
                          ;; Pre-fill the label prompt with the SOURCE's
                          ;; current rdfs:label, extracted from its heading
                          ;; title (everything before the `(curie)'
                          ;; parenthetical, trimmed).  In the typical case
                          ;; the user accepts with RET; rename is usually
                          ;; about changing the identifier, not the label.
                          (current-label
                           (let ((title (ignore-errors
                                          (org-get-heading t t t t))))
                             (when (and title
                                        (string-match
                                         (concat "\\`\\(.*?\\)[ \t]*("
                                                 (regexp-quote src)
                                                 ")")
                                         title))
                               (string-trim
                                (match-string-no-properties 1 title)))))
                          (label (read-string
                                  (format "Label for new identifier (replacing %s): "
                                          src)
                                  current-label))
                          (context (list :kind kind
                                         :scheme-params scheme-params
                                         :prefix prefix
                                         :existing-iris existing))
                          (minted (elot-id-mint scheme-name label context)))
                     ;; Stash the freshly entered label so the outer
                     ;; `let*' can pass it to `elot-rename-resource' as
                     ;; `:new-label' -- but only when the user actually
                     ;; changed it (RET on the pre-filled value should
                     ;; not rewrite the heading).
                     (when (and current-label
                                (not (string= label current-label)))
                       (setq tgt-new-label label))
                     (substring-no-properties minted)))
               (read-string "Target CURIE: ")))))
     (list src tgt :new-label tgt-new-label)))
  (unless (derived-mode-p 'org-mode)
    (user-error "elot-rename-resource: not in an Org buffer"))
  (elot-id-rename--split-curie source) ; shape-check; raises on bad input
  (elot-id-rename--split-curie target)
  (when (string= source target)
    (user-error "elot-rename-resource: SOURCE equals TARGET"))
  (let* ((declared (elot-id-rename--declared-curies))
         (_ (unless (member source declared)
              (user-error
               "elot-rename-resource: SOURCE %s is not a declared resource heading in this buffer"
               source)))
         (target-declared-p (member target declared))
         (_ (pcase op
              ('rename
               (when target-declared-p
                 (user-error
                  "elot-rename-resource: TARGET %s collides with an existing declaration"
                  target)))
              ('merge
               (unless target-declared-p
                 (user-error
                  "elot-rename-resource: op=merge requires TARGET %s to be declared; use op=rename (or omit op) for the rewrite-and-declare path"
                  target)))
              (_ (user-error "elot-rename-resource: unknown op %S (expected `rename' or `merge')" op))))
         (prefix-table (elot-id-rename--read-prefix-table))
         (tgt-prefix (car (elot-id-rename--split-curie target)))
         (tgt-local  (cdr (elot-id-rename--split-curie target)))
         (source-iri (elot-id-rename--expand-curie source prefix-table))
         (tgt-iri-base (cdr (assoc tgt-prefix prefix-table)))
         (declared-prefix nil)
         resolved-tgt-iri)
    ;; Resolve TARGET's IRI; possibly schedule a prefix-row addition.
    ;; In `merge' mode TARGET is declared by construction, so its
    ;; prefix MUST already be in the buffer's prefix table -- the
    ;; `declared-prefix' path can never fire and would be a bug if it
    ;; did (declaration without a prefix row is malformed input).
    (if tgt-iri-base
        (setq resolved-tgt-iri (concat tgt-iri-base tgt-local))
      (when (eq op 'merge)
        (error "elot-rename-resource: op=merge but TARGET prefix `%s:' is not in the prefix table (malformed input?)"
               tgt-prefix))
      (let* ((candidates (elot-id-rename--db-candidates tgt-prefix))
             (chosen (elot-id-rename--resolve-target-iri
                      tgt-prefix candidates target-iri)))
        (setq resolved-tgt-iri (concat chosen tgt-local)
              declared-prefix (cons tgt-prefix chosen))))
    ;; Atomic transaction: prefix-row addition (if any) + buffer
    ;; rewrite + hierarchy refresh.  `atomic-change-group' rolls
    ;; all of it back on any signal.
    (let (counts
          ;; Merge-mode bookkeeping.  Captured /before/ the rewrite
          ;; pass because the pass turns SOURCE's heading parenthetical
          ;; into TARGET's, after which the two declarations are
          ;; indistinguishable by CURIE alone.
          source-heading-marker)
      (atomic-change-group
        (when declared-prefix
          (elot-id-rename--add-prefix-row (car declared-prefix)
                                          (cdr declared-prefix)))
        (when (eq op 'merge)
          (setq source-heading-marker
                (elot-id-rename--heading-marker-for-curie source))
          (unless source-heading-marker
            (error "elot-rename-resource: op=merge cannot locate SOURCE heading for %s"
                   source)))
        (setq counts
              (elot-id-rename--rewrite-buffer source target
                                              source-iri
                                              resolved-tgt-iri))
        ;; Optional label rewrite on the resource-declaring heading.
        ;; The CURIE rewrite has already turned `Label (SOURCE) ...'
        ;; into `Label (TARGET) ...', so we look up TARGET now.
        ;; Merge mode skips this: TARGET's existing label stands; the
        ;; `new-label' arg is only meaningful for the rename mint path.
        (when (and (eq op 'rename)
                   new-label (stringp new-label)
                   (not (string-empty-p new-label)))
          (elot-id-rename--rewrite-heading-label target new-label))
        ;; Merge-mode tail: after the shared rewrite pass has rewritten
        ;; every reference to SOURCE -> TARGET, SOURCE's own heading
        ;; (which the rewriter turned into `Label (TARGET) ...', a
        ;; duplicate of TARGET's existing declaration) must be removed,
        ;; and SOURCE's heading-nested children promoted one outline
        ;; level so they reattach to SOURCE's former outline parent.
        (when (eq op 'merge)
          (elot-id-remove-heading-promote-children source-heading-marker))
        ;; M9.3.F8: O(1) staleness mark.  The next call into
        ;; `elot-id-rename--declared-curies' /
        ;; `elot-id-insert--existing-iris' / etc. picks up the rewrite
        ;; via `elot-headline-hierarchy-ensure-fresh' on first read.
        (cond
         ((fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale))
         ((fboundp 'elot-update-headline-hierarchy)
          (ignore-errors (elot-update-headline-hierarchy)))))
      (let ((msg (format
                  "elot-rename-resource: %s -> %s (%d CURIE, %d IRI; %d prose mention(s) skipped%s)"
                  source target
                  (plist-get counts :curie-count)
                  (plist-get counts :iri-count)
                  (plist-get counts :prose-skipped)
                  (if declared-prefix
                      (format "; declared prefix %s: -> <%s>"
                              (car declared-prefix)
                              (cdr declared-prefix))
                    ""))))
        (message "%s" msg))
      (list :source source
            :target target
            :curie-count (plist-get counts :curie-count)
            :iri-count (plist-get counts :iri-count)
            :prose-skipped (plist-get counts :prose-skipped)
            :declared-prefix declared-prefix))))

(provide 'elot-id-rename)
;;; elot-id-rename.el ends here
