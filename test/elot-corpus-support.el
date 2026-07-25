;;; elot-corpus-support.el --- Support for the ELOT external-quality corpus  -*- lexical-binding: t; -*-

;; Usage (from another test file):
;;   (require 'elot-corpus-support)
;;   (elot-corpus-fixtures)            ; -> list of file paths
;;   (elot-corpus-fixtures "oops")     ; -> only the OOPS! sub-tree
;;   (elot-corpus-fixture-meta FILE)   ; -> plist of :corpus-* fields
;;   (elot-corpus-cite META)           ; -> human-readable citation string

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 8 Step 8.1.  This file is the
;; small support library that the corpus meta-test (and later
;; per-milestone tests) use to discover fixtures and read their
;; provenance drawers.  Pure Elisp; no ROBOT, no DB.
;;
;; The corpus tree lives under `test/fixtures/corpus/'.  See its
;; README.org for the citation policy and accepted drawer fields.
;;
;; `:corpus-expects' values accept two shapes:
;;
;;   - an Elisp checker symbol like `elot/oops-duplicate-labels'
;;     (validated against `org-lint--checkers');
;;
;;   - a pitfall id of the form `BACKEND/ID' like `oops/P32'
;;     (validated against `elot-corpus--known-pitfalls', a
;;     registry the OOPS!/ROBOT backends populate as they come
;;     online; the registry is allowed to be empty at first
;;     commit and the meta-test will simply pass any well-formed
;;     pitfall id through).
;;
;; The literal value `none' is accepted on clean controls.
;;
;; An expects value of shape `planned/SLUG' (e.g.
;; `planned/rector-only-without-some') is also accepted: it marks
;; a fixture whose checker is not yet implemented.  The corpus
;; meta-test treats it as valid, and per-backend harnesses (e.g.
;; `elot-corpus-oops-test.el') skip it automatically because the
;; value is neither a checker symbol nor a registered pitfall id.
;;
;; The optional `:corpus-citation:' drawer field carries a free-
;; text bibliographic citation (paper + section/figure) for prose
;; sources like the Rector / OWL Pizzas tutorials, where
;; `:corpus-url:' alone (a DOI) is not enough.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst elot-corpus-known-sources
  '("OOPS!" "Rector" "ROBOT-report" "OBO" "OWL2-Profile" "OntoClean"
    "reasoner" "clean")
  "Allowed values for the `:corpus-source:' drawer field.")

(defvar elot-corpus--known-pitfalls (make-hash-table :test 'equal)
  "Registry of known pitfall ids, keyed by `BACKEND/ID' (lowercased).
Populated by the SPARQL-backed OOPS! suite and the ROBOT-report
backend as they come online.  Allowed to be empty.")

(defun elot-corpus-register-pitfall (backend id &optional meta)
  "Register pitfall BACKEND/ID with optional META plist.
BACKEND and ID are strings; the key is lowercased for matching."
  (let ((key (downcase (format "%s/%s" backend id))))
    (puthash key (or meta t) elot-corpus--known-pitfalls)
    key))

(defun elot-corpus-known-pitfall-p (value)
  "Return non-nil if VALUE (a string) is a registered pitfall id."
  (and (stringp value)
       (gethash (downcase value) elot-corpus--known-pitfalls)))

(defun elot-corpus--root ()
  "Absolute path of the corpus root directory.
Resolved from this file's location."
  (let ((here (or load-file-name buffer-file-name
                  (locate-library "elot-corpus-support"))))
    (unless here
      (error "elot-corpus-support: cannot resolve own file location"))
    (expand-file-name "fixtures/corpus/"
                      (file-name-directory here))))

(defun elot-corpus-fixtures (&optional subdir)
  "Return a list of fixture file paths under the corpus tree.

If SUBDIR is non-nil, restrict to that sub-tree (e.g. \"oops\",
\"clean\").  README.org files and dot-files are excluded.  The
search is recursive."
  (let* ((root (elot-corpus--root))
         (base (if subdir (expand-file-name subdir root) root)))
    (when (file-directory-p base)
      (seq-filter
       (lambda (f)
         (and (or (string-suffix-p ".org" f)
                  (string-suffix-p ".omn" f)
                  (string-suffix-p ".ttl" f))
              (not (string-equal "README.org" (file-name-nondirectory f)))
              (not (string-prefix-p "." (file-name-nondirectory f)))))
       (directory-files-recursively base "\\`[^.].*\\'")))))

(defconst elot-corpus--drawer-fields
  '(:corpus-source :corpus-id :corpus-severity :corpus-url
                   :corpus-expects :corpus-rationale :corpus-citation)
  "Drawer fields recognised by `elot-corpus-fixture-meta'.")

(defun elot-corpus--field-to-keyword (name)
  "Map drawer field NAME (lowercased, no colons) to a plist keyword."
  (intern (concat ":" (downcase name))))

(defun elot-corpus-fixture-meta (file)
  "Parse FILE's top property drawer and return a plist.

Returned plist always contains :file (absolute path).  Recognised
keys: :corpus-source, :corpus-id, :corpus-severity, :corpus-url,
:corpus-expects, :corpus-rationale.  Values are trimmed strings.
Missing fields are simply absent from the plist."
  (let ((meta (list :file (expand-file-name file))))
    (with-temp-buffer
      (insert-file-contents file nil 0 8192)
      (goto-char (point-min))
      ;; Accept either a :PROPERTIES:...:END: drawer at the very top of
      ;; the file or one attached to the first heading.
      (when (re-search-forward "^:PROPERTIES:[ \t]*$" nil t)
        (let ((end (save-excursion
                     (when (re-search-forward "^:END:[ \t]*$" nil t)
                       (line-beginning-position)))))
          (when end
            (save-restriction
              (narrow-to-region (point) end)
              (goto-char (point-min))
              (while (re-search-forward
                      "^:\\([A-Za-z][A-Za-z0-9_-]*\\):[ \t]*\\(.*\\)$"
                      nil t)
                (let* ((name (match-string 1))
                       (val (string-trim (match-string 2)))
                       (kw (elot-corpus--field-to-keyword name)))
                  (when (memq kw elot-corpus--drawer-fields)
                    (setq meta (plist-put meta kw val))))))))))
    meta))

(defun elot-corpus-meta-well-formed-p (meta)
  "Return non-nil if META has the required corpus drawer fields."
  (and (plist-get meta :corpus-source)
       (plist-get meta :corpus-id)
       (plist-get meta :corpus-url)
       (plist-get meta :corpus-expects)))

(defun elot-corpus-expects-shape (value)
  "Classify the `:corpus-expects:' VALUE.

Returns one of:
  - `none'           if VALUE is \"none\" (clean control)
  - `checker-symbol' if VALUE looks like an Elisp symbol with `/'
  - `planned'        if VALUE has prefix \"planned/\" (checker TBD)
  - `pitfall-id'     if VALUE matches BACKEND/ID
  - `unknown'        otherwise

Note: `checker-symbol' and `pitfall-id' both contain a slash; the
classifier prefers `checker-symbol' when the value's prefix
matches a known checker namespace (currently \"elot/\")."
  (cond
   ((or (null value) (not (stringp value))) 'unknown)
   ((string-equal "none" (downcase value)) 'none)
   ((string-prefix-p "planned/" value) 'planned)
   ((string-prefix-p "elot/" value) 'checker-symbol)
   ((string-match-p "\\`[A-Za-z][A-Za-z0-9_-]*/[A-Za-z0-9._-]+\\'"
                    value)
    'pitfall-id)
   (t 'unknown)))

(defun elot-corpus-expects-valid-p (value)
  "Return non-nil if `:corpus-expects:' VALUE resolves to a
registered checker or a known pitfall id, or is `none'."
  (pcase (elot-corpus-expects-shape value)
    ('none t)
    ('planned t)
    ('checker-symbol
     (require 'org-lint)
     (let ((sym (intern value)))
       (and (boundp 'org-lint--checkers)
            (seq-find (lambda (c)
                        (eq (org-lint-checker-name c) sym))
                      org-lint--checkers))))
    ('pitfall-id (elot-corpus-known-pitfall-p value))
    (_ nil)))

(defun elot-corpus-cite (meta)
  "Render META as a human-readable one-line citation string."
  (let ((src (plist-get meta :corpus-source))
        (id  (plist-get meta :corpus-id))
        (sev (plist-get meta :corpus-severity))
        (url (plist-get meta :corpus-url))
        (cit (plist-get meta :corpus-citation))
        (file (plist-get meta :file)))
    (format "[%s %s%s%s%s%s]"
            (or src "?")
            (or id "?")
            (if (and sev (not (string-empty-p sev))
                     (not (string-equal "-" sev)))
                (format ", %s" sev) "")
            (if (and url (not (string-empty-p url))
                     (not (string-equal "-" url)))
                (format ", %s" url) "")
            (if (and cit (not (string-empty-p cit))
                     (not (string-equal "-" cit)))
                (format ", %s" cit) "")
            (if file (format ", fixture=%s"
                             (file-name-nondirectory file))
              ""))))

(provide 'elot-corpus-support)
;;; elot-corpus-support.el ends here
