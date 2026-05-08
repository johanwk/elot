;;; elot.el --- Emacs Literate Ontology Tool (ELOT)   -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages outlines tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is for authoring OWL ontologies using org-mode.

;; To start an ontology from scratch using ELOT, open an Org file and
;; use predefined "tempo" templates.

;;  - insert `<odh' and hit <Tab> to add a header for the document
;;  - insert `<ods' and hit <Tab> to insert headers for the ontology,
;;    classes, properties, and individuals.

;; The ELOT easymenu ("ELOT" in the menu bar when `elot-mode' is
;; active) gives access to templates and export commands.

;; Optional external packages.  ELOT loads and `elot-mode' activates
;; cleanly without any of these; the features that depend on them
;; degrade to "not available" rather than erroring at load time:
;;
;;  - `sparql-mode'  -- query-block fontification
;;  - `ob-sparql'    -- execution of `#+begin_src sparql' blocks
;;                      (the ELOT prefix-injection / ROBOT advice is
;;                      installed only when this is present)
;;  - `ob-plantuml'  -- PlantUML rendering used by `rdfpuml-block'
;;  - `htmlize'      -- source-block fontification on HTML export
;;  - `omn-mode'     -- major mode for tangled `.omn' files

;; Please consult the package Github site for more information:
;;        <https://github.com/johanwk/elot>

;;; Code:

;; [[file:../elot-defs.org::src-require][src-require]]
(require 'elot-tangle) ; AST parsing, OMN generation, ROBOT post-processing
(require 'ob-lob) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions
(require 'xref) ; jump around
(require 'button) ; for text-buttons
(require 'help-mode) ; nice keymap & look
(require 'url) ; for opening online ontologies
(require 'url-http) ; for opening online ontologies

;; Try to load optional external dependencies
(defvar elot-missing-dependencies nil
  "List of missing external dependencies for legacy ELOT features.")

(dolist (pkg '(htmlize omn-mode sparql-mode ob-sparql ob-plantuml))
  (unless (require pkg nil t)
    (push pkg elot-missing-dependencies)))

(when elot-missing-dependencies
  (message "ELOT: Optional dependencies missing: %s. Some features will be disabled."
           (mapconcat #'symbol-name elot-missing-dependencies ", ")))
;; src-require ends here

;; [[file:../elot-defs.org::src-defvar][src-defvar]]
(defvar org-link-abbrev-alist-local)
;; Declared special so the `let' binding in
;; `elot--custom-org-babel-execute-sparql' is dynamic, reaching
;; ob-sparql's IRI-shortening helpers (which read this variable
;; via dynamic scope).  Without this `defvar', `lexical-binding'
;; turns the binding into a lexical one -- the byte-compiler then
;; flags it as unused, AND the merged prefix pairs never reach
;; ob-sparql.  See Decisions log entry 2026-05-07.
(defvar org-babel-sparql--current-curies)
;; src-defvar ends here

;; [[file:../elot-defs.org::src-settings-externals][src-settings-externals]]
(defcustom elot-default-image-path "./images/"
  "ELOT default output directory for generated images."
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-path (expand-file-name "~/bin/rdf2rml/bin/rdfpuml.pl")
  "Path to the rdfpuml Perl program."
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-options
  "hide empty members
  hide circle
  skinparam classAttributeIconSize 0"
  "Default options for rdfpuml."
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-command-str
  (if (executable-find "rdfpuml") ;; rdfpuml.exe available
      "rdfpuml"  ;; LC_ALL=C should be added, but not available in Windows
    (concat "perl -C -S " elot-rdfpuml-path))
  "Command to execute `rdfpuml'."
  :group 'elot
  :version "29.2"
  :type 'string)
(defun elot-rdfpuml-command (ttl-file)
  "Command to execute rdfpuml to generate diagram from TTL-FILE."
  (shell-command (concat elot-rdfpuml-command-str " " ttl-file)))
;; src-settings-externals ends here

;; [[file:../elot-defs.org::*Open existing OWL files or online ontologies][Open existing OWL files or online ontologies:1]]
(defun elot-open-owl (owl-source)
  "Open an OWL ontology from OWL-SOURCE by converting with `elot-exporter'.
OWL-SOURCE can be a local file or a URL.  The source is passed directly
to `elot-exporter', which handles both local files and URLs (with
content negotiation).  The output is captured into a buffer named after
the source, with `.org' as the extension."
  (interactive "sEnter OWL file path or URL: ")
  (let* ((is-url (string-match-p "\\`https?://" owl-source))
         (base-name (if is-url
                        (file-name-sans-extension
                         (file-name-nondirectory
                          (url-filename (url-generic-parse-url owl-source))))
                      (file-name-sans-extension
                       (file-name-nondirectory owl-source))))
         (output-buffer-name (concat base-name ".org"))
         (output-buffer (get-buffer-create output-buffer-name))
         (command (concat elot-exporter-command-str " "
                          (shell-quote-argument owl-source))))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command command output-buffer)
      (org-mode))
    (pop-to-buffer output-buffer)))
;; Open existing OWL files or online ontologies:1 ends here

;; [[file:../elot-defs.org::src-omn-latex-tt][src-omn-latex-tt]]
(defun elot-latex-filter-omn-item (text backend info)
  "Format OWL Manchester Syntax content TEXT in description lists.
Target output type BACKEND
The context INFO is ignored."
  (progn
    (always info) ;; ignore this argument
    (when (org-export-derived-backend-p backend 'latex)
      (when (seq-some
             (lambda (x)
               (string-match (concat "^\\\\item\\[{" x "}\\]") text))
             elot-omn-property-keywords)
        ;; make the description term texttt
        (setq text (replace-regexp-in-string
                    "\\\\item\\[{\\([-a-zA-Z]+\\)}\\]"
                    "\\\\item[\\\\normalfont\\\\ttfamily\\\\small \\1]"
                    text))
        ;; make the list entry content omn inline code unless it's a url
        (if (not (string-match "\\\\url{.*}$" text))
            (replace-regexp-in-string
             "^\\(.*\\] \\)\\(.*\\)"
             "\\1\\\\lstinline[language=omn]{\\2}"
             text)
          text)))))

;; The registration of `elot-latex-filter-omn-item' on
;; `org-export-filter-item-functions' is no longer done at
;; load time.  Installation is driven by the `elot-mode'
;; lifecycle (`elot-mode--enable' / `elot-mode--disable');
;; see `elot-mode.org'.
;; src-omn-latex-tt ends here

;; [[file:../elot-defs.org::src-puri-expand][src-puri-expand]]
(defun elot-omn-restriction-string (str)
 "STR is wanted as an OMN value.  Strip any meta-annotations, or return unchanged."
 str)
;; src-puri-expand ends here

;; [[file:../elot-defs.org::src-robot-query][src-robot-query]]
(defun elot-robot-execute-query (query inputfile format)
  "Execute SPARQL query QUERY with ROBOT on ontology file INPUTFILE.
Result FORMAT is the symbol `csv' or `ttl'.  Insert the result into
the current buffer.

Signals a `user-error' when ROBOT is not configured
\(see `elot-robot-jar-path'\) or when INPUTFILE does not exist.
Signals an `error' carrying ROBOT's stderr when the process exits
non-zero.  Query and result I/O are forced to UTF-8 end-to-end."
  (when (or (string= elot-robot-jar-path "")
            (not (file-exists-p elot-robot-jar-path)))
    (user-error
     "ELOT SPARQL: ROBOT not found.  Set `elot-robot-jar-path' with M-x customize-variable"))
  (let* ((abs-input (expand-file-name inputfile)))
    (unless (file-exists-p abs-input)
      (user-error
       "ELOT SPARQL: input ontology file does not exist: %s (default-directory: %s)"
       abs-input default-directory))
    (let* ((coding-system-for-read  'utf-8)
           (coding-system-for-write 'utf-8)
           (query-file
            (concat (org-babel-temp-directory) "/"
                    (file-name-base abs-input)
                    ".sparql"))
           (result-file
            (concat (file-name-sans-extension abs-input)
                    "." (symbol-name format)))
           (stderr-file (make-temp-file "elot-robot-stderr-"))
           (exit-code nil))
      (with-temp-file query-file
        (set-buffer-file-coding-system 'utf-8)
        (insert query))
      (unwind-protect
          (progn
            (setq exit-code
                  (call-process "java" nil (list nil stderr-file) nil
                                "-Dfile.encoding=UTF-8"
                                "-jar" elot-robot-jar-path
                                "query"
                                "--input"  abs-input
                                "--format" (symbol-name format)
                                "--query"  query-file
                                result-file))
            (unless (and (integerp exit-code) (zerop exit-code))
              (let ((stderr (with-temp-buffer
                              (when (file-exists-p stderr-file)
                                (insert-file-contents stderr-file))
                              (string-trim (buffer-string)))))
                (error "ELOT SPARQL: ROBOT query failed (exit %s)%s"
                       exit-code
                       (if (string-empty-p stderr)
                           ""
                         (concat ":\n" stderr)))))
            (insert-file-contents result-file))
        (when (file-exists-p stderr-file)
          (ignore-errors (delete-file stderr-file)))))))
;; src-robot-query ends here

;; [[file:../elot-defs.org::src-sparql-merge-prefixes][src-sparql-merge-prefixes]]
(defconst elot--sparql-prefix-line-re
  "\\`[ \t]*PREFIX[ \t]+\\([^[:space:]:]+\\):[ \t]*<\\([^>]*\\)>[ \t]*\\(?:#.*\\)?\\'"
  "Regexp matching a single SPARQL 1.1 PREFIX declaration line.
The keyword match is intended to be used with `case-fold-search'
bound to t.  Submatch 1 is the prefix label (without the colon);
submatch 2 is the IRI (without the angle brackets).  An optional
trailing SPARQL line-comment (\"# ...\" to end of line) after the
closing `>' is tolerated and ignored.")

(defun elot--sparql-normalise-label (s)
  "Strip a single trailing colon from prefix label S, if present."
  (if (and (stringp s) (string-suffix-p ":" s))
      (substring s 0 -1)
    s))

(defun elot--sparql-alist-pairs (alist)
  "Normalise a prefix ALIST to a list of (LABEL . URI) string pairs.
ALIST is in the shape of `org-link-abbrev-alist-local': each entry
is a cons whose car is a label (with or without a trailing colon)
and whose cdr is either a URI string or a one-element list
containing a URI string (the latter shape comes from Org tables).
A leading header row equal to (\"prefix\" . \"uri\") is skipped."
  (let ((rows (if (equal (car alist) '("prefix" . "uri"))
                  (cdr alist)
                alist)))
    (mapcar (lambda (row)
              (cons (elot--sparql-normalise-label (car row))
                    (if (listp (cdr row)) (cadr row) (cdr row))))
            rows)))

(defun elot--sparql-strip-inline-prefixes (body)
  "Parse and strip top-of-body PREFIX lines from BODY.
Return a cons (PAIRS . REST) where PAIRS is a list of (LABEL . URI)
string pairs in source order and REST is BODY with those lines
removed.  Parsing stops at the first non-blank, non-PREFIX line:
PREFIX declarations buried below the SELECT/CONSTRUCT keyword are
left untouched."
  (let ((case-fold-search t)
        (pairs nil)
        (lines (split-string (or body "") "\n"))
        (consumed 0)
        (stop nil))
    (while (and lines (not stop))
      (let ((line (car lines)))
        (cond
         ((string-match-p "\\`[ \t]*\\'" line)
          (cl-incf consumed)
          (setq lines (cdr lines)))
         ((string-match elot--sparql-prefix-line-re line)
          (push (cons (match-string 1 line) (match-string 2 line)) pairs)
          (cl-incf consumed)
          (setq lines (cdr lines)))
         (t
          (setq stop t)))))
    (cons (nreverse pairs)
          (mapconcat #'identity
                     (nthcdr consumed (split-string (or body "") "\n"))
                     "\n"))))

(defun elot--sparql-compute-merged-prefixes (alist body)
  "Compute merged prefix declarations and the body with PREFIX lines stripped.
ALIST is shaped like `org-link-abbrev-alist-local'.  Inline PREFIX
lines at the top of BODY (SPARQL 1.1 syntax, case-insensitive) are
recognised and stripped.

Return a cons (PAIRS . BODY-WITHOUT-PREFIX-LINES) where PAIRS is
an alist of (LABEL . URI) string pairs in canonical order (alist-
derived labels first, then inline-only labels in source order),
with each label appearing exactly once.  Inline declarations
override the alist: when the same label appears in both with
different URIs, a single `display-warning' is emitted under
category `elot-sparql' and the inline URI wins.  Identical
re-declarations (same label, same URI) are silently
de-duplicated."
  (let* ((injected (elot--sparql-alist-pairs alist))
         (parsed   (elot--sparql-strip-inline-prefixes body))
         (inline   (car parsed))
         (rest     (cdr parsed))
         (merged   (append injected inline))
         (table    (make-hash-table :test 'equal))
         (order    nil))
    (dolist (pair merged)
      (let* ((label (car pair))
             (uri   (cdr pair))
             (prev  (gethash label table)))
        (cond
         ((null prev)
          (puthash label uri table)
          (push label order))
         ((string= prev uri)
          nil) ;; silent dedup
         (t
          (display-warning
           'elot-sparql
           (format "Conflicting PREFIX %s: <%s> vs <%s> -- using last (<%s>)"
                   label prev uri uri)
           :warning)
          (puthash label uri table)))))
    (cons (mapcar (lambda (label) (cons label (gethash label table)))
                  (nreverse order))
          rest)))

(defun elot--sparql-format-prefix-block (pairs)
  "Format an alist PAIRS of (LABEL . URI) as a SPARQL prefix block.
Return a string containing one `PREFIX label: <uri>' declaration
per line, in the order of PAIRS.  Returns the empty string when
PAIRS is nil."
  (mapconcat (lambda (p)
               (format "PREFIX %-5s <%s>"
                       (concat (car p) ":")
                       (cdr p)))
             pairs
             "\n"))

(defun elot--sparql-merge-prefixes (alist body)
  "Merge prefix declarations from ALIST and inline PREFIX lines in BODY.
ALIST is shaped like `org-link-abbrev-alist-local'.  Inline PREFIX
lines at the top of BODY (SPARQL 1.1 syntax, case-insensitive) are
recognised and stripped.

Return a cons (PREFIX-BLOCK . BODY-WITHOUT-PREFIX-LINES) where
PREFIX-BLOCK is a string containing one `PREFIX label: <uri>'
declaration per line, with each label appearing exactly once and
in source order (injected first, then inline-only labels in the
order they appeared in BODY).  Inline declarations override the
alist: when the same label appears in both with different URIs, a
single `display-warning' is emitted under category `elot-sparql'
and the inline URI wins.  Identical re-declarations (same label,
same URI) are silently de-duplicated.  When no declarations apply,
PREFIX-BLOCK is the empty string.

This is a thin wrapper over `elot--sparql-compute-merged-prefixes'
plus `elot--sparql-format-prefix-block'; callers that also need
the merged (LABEL . URI) pairs for downstream consumers (such as
`org-babel-sparql--current-curies' for result-IRI shortening)
should call `elot--sparql-compute-merged-prefixes' directly."
  (let* ((computed (elot--sparql-compute-merged-prefixes alist body))
         (pairs    (car computed))
         (rest     (cdr computed)))
    (cons (elot--sparql-format-prefix-block pairs) rest)))
;; src-sparql-merge-prefixes ends here

;; [[file:../elot-defs.org::src-sparql-exec-patch][src-sparql-exec-patch]]
(defun elot--is-elot-buffer ()
  "Check if the current buffer is an ELOT buffer."
  (bound-and-true-p elot-mode))

(defconst elot--sparql-empty-result-sentinel
  "[empty result set]"
  "Sentinel returned by the ELOT SPARQL advice when a CSV result has
no rows.  Used in place of an empty CSV body so that
`org-babel-sparql-convert-to-table' is not invoked on input it
cannot parse.")

(defun elot--sparql-resolve-format (raw)
  "Classify the :format header value RAW as the symbol `ttl' or `csv'.
A nil or empty RAW maps to `csv'.  An unrecognised non-empty RAW
also maps to `csv', but emits a single `display-warning' under
category `elot-sparql' so that the silent fallback is at least
visible."
  (cond
   ((or (null raw) (and (stringp raw) (string-empty-p raw))) 'csv)
   ((not (stringp raw))
    (display-warning
     'elot-sparql
     (format "Non-string :format %S; defaulting to csv" raw)
     :warning)
    'csv)
   ((string-match-p "\\(turtle\\|ttl\\)" raw) 'ttl)
   ((string-match-p "\\(csv\\|text/csv\\)" raw) 'csv)
   (t (display-warning
       'elot-sparql
       (format "Unrecognised :format %S; defaulting to csv" raw)
       :warning)
      'csv)))

(defun elot--sparql-classify-url (url)
  "Classify URL as the symbol `endpoint' or `local-file'.
URL is the value of the :url header argument.  Signals a
`user-error' when URL is nil/empty, or when it is non-HTTP and
points to a local file that does not exist; the message in the
latter case names the resolved absolute path and the current
`default-directory'."
  (cond
   ((or (null url) (and (stringp url) (string-empty-p url)))
    (user-error
     "ELOT SPARQL: missing :url header argument (set :url to an HTTP endpoint or a local ontology file)"))
   ((not (stringp url))
    (user-error "ELOT SPARQL: :url must be a string, got %S" url))
   ((string-match-p "\\`https?:" url) 'endpoint)
   (t
    (let ((abs (expand-file-name url)))
      (unless (file-exists-p abs)
        (user-error
         "ELOT SPARQL: local :url file does not exist: %s (default-directory: %s)"
         abs default-directory))
      'local-file))))

(defun elot--sparql-result-empty-p (format-symbol)
  "Return non-nil if the current buffer holds an empty SPARQL result.
FORMAT-SYMBOL is `csv' or `ttl' as returned by
`elot--sparql-resolve-format'.  An empty result is a buffer that is
either completely empty, contains only whitespace, or -- for csv --
contains only a header row."
  (save-excursion
    (goto-char (point-min))
    (or (= (point-min) (point-max))
        (looking-at-p "\\`[ \t\n\r]*\\'")
        (and (eq format-symbol 'csv)
             (save-excursion
               (forward-line 1)
               (looking-at-p "[ \t\n\r]*\\'"))))))

(defun elot--custom-org-babel-execute-sparql (orig-fun &rest args)
  "ELOT-specific SPARQL execution with support for ROBOT.
This function provides advice :around `org-babel-execute:sparql'.
ORIG-FUN and ARGS are the original function and its arguments,
invoked unchanged when not called from an ELOT buffer.

In an ELOT buffer the advice:

- requires :url and verifies that local files actually exist
  \(via `elot--sparql-classify-url'\);
- normalises :format to one of the symbols `csv' or `ttl'
  \(via `elot--sparql-resolve-format'\);
- merges and de-duplicates prefix declarations from
  `org-link-abbrev-alist-local' and the query body
  \(via `elot--sparql-merge-prefixes'\);
- forces UTF-8 for query and result I/O so non-ASCII content
  round-trips cleanly;
- returns `elot--sparql-empty-result-sentinel' instead of throwing
  when a CSV result is empty;
- propagates ROBOT's stderr when the local-file branch fails."
  (if (not (elot--is-elot-buffer))
      (apply orig-fun args)
    (let* ((body          (nth 0 args))
           (params        (nth 1 args))
           (raw-url       (cdr (assoc :url params)))
           (raw-format    (cdr (assoc :format params)))
           (kind          (elot--sparql-classify-url raw-url))
           (format-symbol (elot--sparql-resolve-format raw-format))
           (expanded      (org-babel-expand-body:sparql body params))
           (computed      (elot--sparql-compute-merged-prefixes
                           org-link-abbrev-alist-local expanded))
           (merged-pairs  (car computed))
           (query-body    (cdr computed))
           (prefix-block  (elot--sparql-format-prefix-block merged-pairs))
           ;; Make inline-declared prefixes participate in result-IRI
           ;; shortening too: feed the *merged* pairs (alist + inline,
           ;; with inline overrides applied) to ob-sparql's curie
           ;; abbreviator instead of just `org-link-abbrev-alist-local'.
           (org-babel-sparql--current-curies
            (append merged-pairs org-link-abbrev-alist))
           (final-query
            (if (or (null prefix-block) (string-empty-p prefix-block))
                query-body
              (concat prefix-block "\n" query-body)))
           (coding-system-for-read  'utf-8)
           (coding-system-for-write 'utf-8))
      (message "ELOT SPARQL: %s, format=%s" kind format-symbol)
      (with-temp-buffer
        (set-buffer-file-coding-system 'utf-8)
        (pcase kind
          ('endpoint
           (sparql-execute-query final-query raw-url raw-format t))
          ('local-file
           (elot-robot-execute-query final-query raw-url format-symbol)))
        (org-babel-result-cond
            (cdr (assoc :result-params params))
          (buffer-string)
          (cond
           ((eq format-symbol 'csv)
            (if (elot--sparql-result-empty-p 'csv)
                elot--sparql-empty-result-sentinel
              (org-babel-sparql-convert-to-table)))
           (t (buffer-string))))))))

;; The advice is installed and removed by `elot-mode--enable' /
;; `elot-mode--disable' (see elot-mode.el).  Installing it here at
;; load time would mutate global state for every Emacs session that
;; merely loads ELOT, contrary to MELPA guidance.
;; src-sparql-exec-patch ends here

;; [[file:../elot-defs.org::src-babel-passthrough][src-babel-passthrough]]
(defun elot-org-babel-execute-passthrough (body params)
  "Return BODY unchanged when executing an Org Babel block.

This function is used to define a passthrough execution behavior
for Org Babel blocks with the language `ttl'.  It ensures that
the contents of a `#+begin_src ttl' block are returned as-is,
without any processing or transformation.

This is useful for passing Turtle (TTL) content to other source
blocks without modification.

PARAMS is ignored."
  (progn
    (always params)  ;; ignore argument
    body))

(unless (fboundp #'org-babel-execute:ttl)
  (defalias #'org-babel-execute:ttl #'elot-org-babel-execute-passthrough))
;; src-babel-passthrough ends here

;; [[file:../elot-defs.org::src-rdfpuml-execute][src-rdfpuml-execute]]
(defun elot-rdfpuml-execute (ttl &optional prefixes config add-options epilogue)
  "Run rdfpuml on Turtle RDF content and return PlantUML code.
TTL is a Turtle string, PREFIXES optional prefix block,
CONFIG optional Turtle for rdfpuml configuration,
ADD-OPTIONS a string of PlantUML options added to rdfpuml defaults,
EPILOGUE extra PlantUML clauses."
  (let* ((options-str
         (if add-options
             (concat "[] puml:options \"\"\""
                     elot-rdfpuml-options "\n"
                     add-options
                     "\n\"\"\".\n")))
        (input-ttl-file (org-babel-temp-file "rdfpuml-" ".ttl"))
        (output-puml-file (concat (file-name-sans-extension input-ttl-file) ".puml")))
    (with-temp-file input-ttl-file
      (insert (mapconcat #'identity
                         (list prefixes ttl config options-str) "\n")))
    ;; apparently prefixes.ttl is needed to reside in current dir, will overwrite
    (if prefixes (with-temp-file "prefixes.ttl"
                   (insert prefixes "\n")))
    (elot-rdfpuml-command input-ttl-file)
    (with-temp-file output-puml-file
      (insert-file-contents output-puml-file)
      ;; Perform the sed-like replacement
      (goto-char (point-min))
      (while (re-search-forward " : rdfs:\\(subClassOf\\|subPropertyOf\\)" nil t)
        (replace-match ""))
      (when epilogue
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "@enduml" nil t)
            (replace-match (concat epilogue "\n@enduml") t t)))))
    output-puml-file))
;; src-rdfpuml-execute ends here

;; [[file:../elot-defs.org::src-plantuml-execute][src-plantuml-execute]]
(defun elot-plantuml-execute (puml-file output-name format)
  "With PlantUML, read PUML-FILE and write image file to OUTPUT-NAME.FORMAT.
The file is stored in the ELOT default image directory.
Return output file name."
  (if (or (string= org-plantuml-jar-path "") (not (file-exists-p org-plantuml-jar-path)))
    (error "PlantUML not found.  Set org-plantuml-jar-path with M-x customize-variable"))
  (let ((tmp-output-file (concat (file-name-sans-extension puml-file) "." format))
  (output-file (concat elot-default-image-path output-name "." format)))
    (message (concat puml-file " --> " output-file))
    (make-directory elot-default-image-path :always)
    (shell-command
     (concat "java -jar " org-plantuml-jar-path " -t" format " " puml-file))
    (copy-file tmp-output-file output-file :allow-overwrite)
    output-file))
;; src-plantuml-execute ends here

;; [[file:../elot-defs.org::src-resolve-prefixes-on-export][src-resolve-prefixes-on-export]]
(defun elot--resolve-prefixes-in-description-list ()
  "Resolve RDF-style prefixes in description list values.
For lines matching ` - <term> :: <prefix>:` at the end,
replace <prefix> with the result of `elot-unprefix-uri`."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern "^\\s-*[-+]\\s-+\\(?:.*?\\)::\\s-*\\([[:word:]_./-]*:\\)\\s-*$"))
      (while (re-search-forward pattern nil t)
        (let* ((start (match-beginning 1))
               (end   (match-end 1))
               (prefix (buffer-substring-no-properties start end))
               (resolved (elot-unprefix-uri prefix org-link-abbrev-alist-local :noerror)))
          (when (stringp resolved)
            (delete-region start end)
            (goto-char start)
            (insert resolved)))))))
;; src-resolve-prefixes-on-export ends here

;; [[file:../elot-defs.org::src-linkify-codelist-items-in-buffer][src-linkify-codelist-items-in-buffer]]
(defun elot--linkify-codelist-items-in-buffer ()
  "Search for `elot-codelist-fontify-regexp` in the current buffer
and replace matches with Org-mode links, unless the match is
within a code block, example block, fixed-width area, or IN a headline's text.
The link description is obtained using `(elot-codelist-id-label MATCH)`."
  (save-excursion
    (goto-char (point-min))
    (while-let ((matched-text
                 (and (re-search-forward elot-codelist-fontify-regexp nil t)
                      (match-string-no-properties 0))))
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        ;; Corrected check:
        (if (let* ((element-at-start ;; Get element at the beginning of the match
                    (save-excursion
                      (goto-char start)
                      (org-element-context))))
              (or
               (eq (org-element-type element-at-start) 'headline)
               (org-element-lineage element-at-start
                                    '(src-block example-block fixed-width)
                                    t))) ; 't' checks element-at-start itself too
            nil ; In forbidden context, do nothing and skip to next match
          (let* ((label (elot-codelist-id-label matched-text))
                 (link-string (if label (format "[[#%s][%s]]" matched-text label))))
            (if label 
                (progn
                  (delete-region start end)
                  (goto-char start)
                  (insert link-string)))))))))
;; src-linkify-codelist-items-in-buffer ends here

;; [[file:../elot-defs.org::src-stable-links-export][src-stable-links-export]]
(defun elot--prepare-export-buffer (backend)
  "Prepare the export clone for Elot:

  - Give each resource-declaring headline a CUSTOM_ID (if missing).
  - Replace every visible CURIE with an internal link to that ID,
    except when the CURIE is inside a src/example/fixed-width block."
  (when (org-export-derived-backend-p backend 'latex)
    (cl-return-from elot--prepare-export-buffer))
  ;; ------------------------------------------------------------
  ;; 0  Re-parse the headline hierarchy in the export clone
  ;;    (buffer-local vars like `elot-headline-hierarchy' and
  ;;    `org-link-abbrev-alist-local' are nil in a fresh clone)
  ;; ------------------------------------------------------------
  (elot-update-headline-hierarchy)
  ;; ------------------------------------------------------------
  ;; 1  Turn uses of defined resources into links
  ;; ------------------------------------------------------------
  (org-fold-show-all)
  (elot-label-display-setup)
  (font-lock-fontify-buffer) ;; because "linkify" reads properties
  (elot--linkify-codelist-items-in-buffer)
  ;; ------------------------------------------------------------
  ;; 2  Ensure CUSTOM_ID drawers
  ;; ------------------------------------------------------------
  (let (pending)                                   ; (marker . id) pairs
    (org-with-wide-buffer
     (org-element-map (org-element-parse-buffer 'headline) 'headline
       (lambda (hl)
         (let* ((title (org-element-property :raw-value hl))
                (id    (elot-entity-from-header title t)))
           (when id
             (let ((m (copy-marker (org-element-property :begin hl))))
               (unless (org-entry-get m "CUSTOM_ID")
                 (push (cons m id) pending)))))))
     ;; Insert from bottom to top so earlier insertions don't shift markers
     (dolist (cell (nreverse pending))
       (org-with-point-at (car cell)
         (org-entry-put (car cell) "CUSTOM_ID" (cdr cell))))))
  ;; ------------------------------------------------------------
  ;; 3  Resolve prefixes in description list values
  ;; ------------------------------------------------------------
  (elot--resolve-prefixes-in-description-list))

;; The export hook is installed and removed by `elot-mode--enable' /
;; `elot-mode--disable' (see `elot-mode.el'); it is not added at
;; load time so that `(require 'elot)' has no global side effect.
;; src-stable-links-export ends here

;; [[file:../elot-defs.org::src-latex-section-export][src-latex-section-export]]
(defun elot-ontology-resource-section (level numbered-p)
  "Return LaTeX environment by subsection depth LEVEL.
If NUMBERED-P is `true', create a numbered section."
  (if numbered-p
    (cond
      ((= 1 level) "\\chapter{%s}")
      ((= 2 level) "\\section{%s}")
      ((= 3 level) "\\subsection{%s}")
      ((= 4 level) "\\subsubsection{%s}")
      ((= 5 level) "\\subsubsubsection{%s}")
      ((= 6 level) "\\paragraph{%s}")
      (t "\\subparagraph{%s}"))
    (cond ;; Koma-script commands, see https://tex.stackexchange.com/questions/193767/how-to-use-unnumbered-chapters-with-koma-script/193799#193799
     ((= 1 level) "\\addchap{%s}")
     ((= 2 level) "\\addsec{%s}")
     ((= 3 level) "\\subsection*{%s}")
     (t "\\subsubsection*{%s}"))))
;; src-latex-section-export ends here

;; [[file:../elot-defs.org::src-get-heading-nocookie][src-get-heading-nocookie]]
(defun elot-org-get-heading-nocookie (&optional no-tags no-todo no-priority no-comment)
  "Call `org-get-heading' but strip out any task progress cookie, like `[3/4]'.
If provided, optional arguments NO-TAGS, NO-TODO, NO-PRIORITY, and NO-COMMENT
are passed on to `org-get-heading'."
  (replace-regexp-in-string " \\[[[:digit:]/%]+\\]$" ""
                            (org-get-heading no-tags no-todo no-priority no-comment)))
;; src-get-heading-nocookie ends here

;; [[file:../elot-defs.org::src-org-find-description-value][src-org-find-description-value]]
(defun elot-org-find-description-value (term-regex &optional value-regex)
  "Find a description-list value for TERM-REGEX in the current Org section.

Walks the description list(s) in the body of the Org headline at point
(excluding descendant subsections) and returns the value of the first
item whose tag matches TERM-REGEX.  If several tags match, prefer the
first whose value matches VALUE-REGEX; otherwise fall back to the first
match.  Return nil if no tag matches.

VALUE-REGEX is optional; defaults to match anything."
  (setq value-regex (or value-regex ""))
  (save-excursion
    (save-restriction
      (widen)
      (org-back-to-heading t)
      (let* ((beg (point))
             (end (save-excursion (outline-next-heading) (point))))
        (narrow-to-region beg end)
        (let* ((tree (org-element-parse-buffer))
               (descriptions
                (org-element-map tree 'item
                  (lambda (it)
                    (when (org-element-property :tag it)
                      (elot-org-elt-item-str it)))))
               (matches (seq-filter
                         (lambda (pair)
                           (and (car pair)
                                (stringp (car pair))
                                (string-match-p term-regex (car pair))))
                         descriptions))
               (result
                (or (seq-some (lambda (pair)
                                (let ((value (cadr pair)))
                                  (when (and value
                                             (stringp value)
                                             (string-match-p value-regex value))
                                    value)))
                              matches)
                    (when matches (cadr (car matches))))))
          (and (stringp result) result))))))
;; src-org-find-description-value ends here

;; [[file:../elot-defs.org::src-get-description-entry :tangle no][src-get-description-entry :tangle no]]
(defun elot-org-get-description-entry (tag)
  "Search forward for TAG and return text of Org element found.
Remove string decorations.  Newlines are replaced by spaces in the result."
  (save-excursion
    (if (search-forward-regexp tag nil t)
        (let* ((element (org-element-at-point))
               (beg (org-element-property :contents-begin element))
               (end (org-element-property :contents-end element))
               (entry-text (buffer-substring-no-properties beg end)))
          (replace-regexp-in-string "\n\s*" " " entry-text)))))
;; src-get-description-entry :tangle no ends here

;; [[file:../elot-defs.org::src-latex-export-replacenames][src-latex-export-replacenames]]
(org-export-define-derived-backend 'ELOT-latex 'latex
  :translate-alist '((item . elot-my-item-translator)))
(defvar elot-item-process nil
  "Toggle during LaTeX export, to turn replacement of list items on or off.
Used in `elot-my-item-translator'.")

(defun elot-my-item-translator (item c info)
  "Translator for LaTeX export, replace RDF identifiers with simpler labels.
This makes for more readable output in description lists.  A list serves
to map selected annotation properties to shorter labels.  For example,
`iof-av:explanatoryNote' will be replaced by `explanatory note'.

ITEM is an entry in a description list.  C is the contents of the item.
INFO is a plist holding contextual information.  See the documentation
for `org-latex-item'.

Translation is turned on when the magic value `item-translate-start' is
found in a description list, and off when `item-translate-stop' is found.

This function is a workaround.  It relies on magic strings because
positions in the buffer are unpredictable while the export is being
conducted."
  (let* ((item-tag-maybe (car (org-element-property :tag item)))
         (item-tag-stringp (stringp item-tag-maybe))
         (item-tag (if item-tag-stringp (substring-no-properties item-tag-maybe) item-tag-maybe)))
    (if (and item-tag-stringp (string= item-tag "item-translate-start")) (setq elot-item-process t))
    (if (and item-tag-stringp (string= item-tag "item-translate-stop")) (setq elot-item-process nil))
    (when (and elot-item-process item-tag-stringp)
      (progn
                                        ;(message (substring-no-properties item-tag))
        (setf (plist-get (cadr item) :checkbox) nil)  ; set checkbox here
        (let ((tag-mapped (assoc item-tag
                 (quote
                  (("iof-av:isPrimitive" . "primitive?")
                   ("iof-av:naturalLanguageDefinition" . "definition")
                   ("iof-av:primitiveRationale" . "why primitive")
                   ("iof-av:usageNote" . "usage note")
                   ("owl:deprecated" . "deprecated?")
                   ("rdfs:seeAlso" . "see also")
                   ("skos:example" . "example")
                   ("skos:scopeNote" . "scope note")
                   ("skos:altLabel" . "alternative label")
                   ("iof-av:explanatoryNote" . "explanatory note")
                   ("rdfs:comment" . "comment")
                   ("rdfs:isDefinedBy" . "defined by")
                   ("iof-av:firstOrderLogicDefinition" . "first-order logic definition")
                   ("iof-av:semiFormalNaturalLanguageDefinition" . "semi-formal definition")
                   ("iof-av:semiFormalNaturalLanguageAxiom" . "semi-formal axiom")
                   ("iof-av:adaptedFrom" . "adapted from")
                   ("iof-av:synonym" . "synonym"))))))
          (if tag-mapped
              (setf (plist-get (cadr item) :tag) (cdr tag-mapped))))))
    (unless (and item-tag-stringp
                 (or (string= item-tag "item-translate-start") (string= item-tag "item-translate-stop")))
      (org-latex-item item c info))))
;; src-latex-export-replacenames ends here

;; [[file:../elot-defs.org::src-tempo-fwd-declare][src-tempo-fwd-declare]]
;; The former Shift-F5 menu has been retired in favour of the
;; easymenu defined in elot-mode.el, which covers the same
;; functionality.  The =F5= binding for `elot-key-toggle-labels'
;; is retained.
;; src-tempo-fwd-declare ends here

;; [[file:../elot-defs.org::src-keybinding][src-keybinding]]
(defcustom elot-key-toggle-labels (kbd "<f5>")
  "Keybinding to toggle label display in ELOT buffers."
  :type 'key-sequence
  :group 'elot)

(defun elot-setup-org-keybindings ()
  (local-set-key elot-key-toggle-labels #'elot-toggle-label-display))
;; src-keybinding ends here

;; [[file:../elot-defs.org::src-tsv-table][src-tsv-table]]
(defun elot-tsv-to-table (filename)
  "Read tab separated values file FILENAME and insert an Org table at point."
  (let* ((lines (with-temp-buffer
                 (insert-file-contents filename)
                 (split-string (buffer-string) "\n")))
         (header (split-string (car lines) "\t"))
         (body (mapcar
                (lambda (line) (split-string line "\t"))
                (butlast (cdr lines)))))  ;; check this is ok
    (cons header (cons 'hline body))))
;; src-tsv-table ends here

;; [[file:../elot-defs.org::src-load-entry-point][src-load-entry-point]]
(provide 'elot)

(require 'elot-mode)            ; minor mode, keymap, easymenu, lifecycle
(require 'elot-label-display)   ; label display overlays
(require 'elot-flymake)         ; flymake backend
(require 'elot-owl-grammar)     ; OMN PEG grammar
;; Optional sub-modules: do not break the main load if they are absent
;; (e.g. when sqlite support is unavailable).
(require 'elot-db nil t)
(require 'elot-sources nil t)
;; src-load-entry-point ends here

;;; elot.el ends here
