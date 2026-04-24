;;; elot-sources.el --- Source parsers for the ELOT label cache  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The ELOT authors
;; SPDX-License-Identifier: MIT

;; Author: ELOT contributors
;; Keywords: tools, hypermedia, data

;;; Commentary:

;; Step 1.3 of the ELOT global-slurp plan: one parser per source
;; type, all producing the common (ID LABEL PLIST) slurp shape
;; consumed by `elot-db-update-source'.  No DB writes happen here;
;; that wiring is Step 1.4.
;;
;; Supported formats (v1):
;;
;;   org   -- ELOT Org document, via `elot-build-slurp'
;;   csv   -- header-row-aware flat dict (UC3 primary path)
;;   tsv   -- header-row-aware flat dict (UC3 primary path)
;;   json  -- flat ({"id": "label"}) or nested
;;            ({"id": {"label": ..., "prop": ...}}) dict (UC3)
;;   ttl   -- Turtle via ROBOT + label SPARQL query
;;   rq    -- SPARQL query against a local file or endpoint, via
;;            ROBOT, with CSV result caching under .elot-cache/
;;
;; The dispatcher maps file extensions to parsers via the
;; user-extensible `elot-source-supported-extensions' defcustom.
;;
;; See ELOT-DB-PLAN.org (Step 1.3 and the UC3 subsection) for the
;; authoritative design.  Like `elot-db.el', this file is authored
;; directly as Elisp rather than tangled from an Org document.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function elot-build-slurp "elot-tangle" (&optional hierarchy))
(declare-function elot-update-headline-hierarchy "elot-tangle" ())
(defvar elot-robot-jar-path)

(defgroup elot-sources nil
  "Parsers for label sources consumed by ELOT's global-slurp cache."
  :group 'elot
  :prefix "elot-source-")

;;;; ------------------------------------------------------------------
;;;; ROBOT availability + invocation
;;;; ------------------------------------------------------------------

(defun elot-robot-available-p ()
  "Return non-nil if ROBOT is usable on this machine.
Checks `elot-robot-jar-path' (the canonical ELOT setting, pointing
at a `robot.jar' invoked via `java -jar') first, and falls back to
a `robot' executable on `PATH' for users with a shell shim."
  (require 'elot-tangle nil t)
  (or (and (bound-and-true-p elot-robot-jar-path)
           (stringp elot-robot-jar-path)
           (not (string-empty-p elot-robot-jar-path))
           (file-exists-p elot-robot-jar-path))
      (and (executable-find "robot") t)))

(defun elot-source--robot-run (query-file input-file output-file)
  "Run ROBOT's `query' subcommand: QUERY-FILE against INPUT-FILE -> OUTPUT-FILE.
OUTPUT-FILE's extension determines the result format (CSV when it
ends in `.csv', which is what the callers in this module use).

Prefers `java -jar elot-robot-jar-path'; falls back to a `robot'
executable on `PATH'.  Non-zero exit status raises `user-error'
with ROBOT's stderr captured from the process buffer."
  (require 'elot-tangle nil t)
  (unless (elot-robot-available-p)
    (user-error
     "elot-sources: ROBOT not available (set `elot-robot-jar-path' or install `robot')"))
  (let* ((use-jar (and (bound-and-true-p elot-robot-jar-path)
                       (stringp elot-robot-jar-path)
                       (not (string-empty-p elot-robot-jar-path))
                       (file-exists-p elot-robot-jar-path)))
         (program (if use-jar "java" (executable-find "robot")))
         (args    (append
                   (if use-jar
                       (list "-jar" elot-robot-jar-path)
                     nil)
                   (list "query"
                         "--input"  (expand-file-name input-file)
                         "--query"  (expand-file-name query-file)
                         (expand-file-name output-file))))
         (err-buf (generate-new-buffer " *elot-robot-stderr*"))
         exit)
    (unwind-protect
        (progn
          (setq exit (apply #'call-process program nil
                            (list err-buf t) nil args))
          (unless (and (integerp exit) (zerop exit))
            (let ((msg (with-current-buffer err-buf
                         (buffer-substring-no-properties
                          (point-min) (point-max)))))
              (user-error "ROBOT query failed (exit %s): %s"
                          exit (string-trim msg)))))
      (when (buffer-live-p err-buf) (kill-buffer err-buf)))))

;;;; ------------------------------------------------------------------
;;;; Dispatcher
;;;; ------------------------------------------------------------------

(defcustom elot-source-supported-extensions
  '(("org"  . elot-source-parse-org)
    ("csv"  . elot-source-parse-csv)
    ("tsv"  . elot-source-parse-tsv)
    ("json" . elot-source-parse-json)
    ("ttl"  . elot-source-parse-ttl)
    ("rq"   . elot-source-parse-rq))
  "Alist mapping source file extensions to parser functions.
Each entry is (EXT . FUNCTION).  EXT is a lower-case extension
without the leading dot.  FUNCTION is called with the source file
path and must return a list of (ID LABEL PLIST) triples, the
common slurp shape.

Users may register additional parsers (e.g. project-specific
JSON / YAML / XML variants) by customising this variable or
calling `add-to-list' on it without patching ELOT."
  :type '(alist :key-type string :value-type function)
  :group 'elot-sources)

(defun elot-source--extension (file)
  "Return the lower-cased extension of FILE without the dot, or nil."
  (let ((ext (file-name-extension file)))
    (and ext (downcase ext))))

(defun elot-source-parse (source &rest args)
  "Dispatch on SOURCE's extension to the registered parser.
Extra ARGS are forwarded to the parser (used by `.rq' to pass a
data-source).  Signals `user-error' if no parser is registered
for the extension."
  (let* ((ext (elot-source--extension source))
         (fn  (cdr (assoc ext elot-source-supported-extensions))))
    (unless fn
      (user-error "elot-sources: no parser registered for extension %S"
                  (or ext "(none)")))
    (apply fn source args)))

;;;; ------------------------------------------------------------------
;;;; Org
;;;; ------------------------------------------------------------------

(defun elot-source-parse-org (file)
  "Parse an ELOT Org FILE into slurp entries.
Visits FILE in a temporary buffer in `org-mode', runs
`elot-update-headline-hierarchy', and returns
`elot-build-slurp''s output."
  (require 'elot-tangle)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((default-directory (file-name-directory (expand-file-name file)))
          ;; Disable local vars for sandbox safety in tests.
          (enable-local-variables nil))
      (delay-mode-hooks (org-mode))
      (elot-update-headline-hierarchy)
      (elot-build-slurp))))

;;;; ------------------------------------------------------------------
;;;; CSV / TSV
;;;; ------------------------------------------------------------------

(defun elot-source--split-csv-line (line sep)
  "Split LINE on SEP (a single character string) honouring double-quoted fields.
Embedded double-quote characters inside a quoted field are
represented by a doubled double-quote (RFC 4180).  Fields are
returned with surrounding quotes stripped."
  (let ((fields nil)
        (i 0)
        (n (length line))
        (sep-char (aref sep 0)))
    (while (< i n)
      (let ((c (aref line i)))
        (cond
         ((eq c ?\")
          ;; Quoted field.
          (setq i (1+ i))
          (let ((start i) (buf nil) (done nil))
            (while (and (not done) (< i n))
              (let ((c2 (aref line i)))
                (cond
                 ((and (eq c2 ?\")
                       (< (1+ i) n)
                       (eq (aref line (1+ i)) ?\"))
                  ;; Escaped quote.
                  (push (substring line start i) buf)
                  (push "\"" buf)
                  (setq i (+ i 2))
                  (setq start i))
                 ((eq c2 ?\")
                  (push (substring line start i) buf)
                  (setq done t)
                  (setq i (1+ i)))
                 (t
                  (setq i (1+ i))))))
            ;; Consume up to next separator (or end).
            (when (and (< i n) (eq (aref line i) sep-char))
              (setq i (1+ i)))
            (push (apply #'concat (nreverse buf)) fields)))
         (t
          ;; Unquoted field: read until sep.
          (let ((start i))
            (while (and (< i n) (not (eq (aref line i) sep-char)))
              (setq i (1+ i)))
            (push (substring line start i) fields)
            (when (and (< i n) (eq (aref line i) sep-char))
              (setq i (1+ i))))))))
    ;; Handle trailing empty field after a final separator.
    (when (and (> n 0) (eq (aref line (1- n)) sep-char))
      (push "" fields))
    (nreverse fields)))

(defun elot-source--parse-separated (file sep)
  "Common CSV/TSV parser; SEP is a single-character separator string.

Recognises two language-tag conventions (Step 1.16.6):

- A header named exactly `lang' carries a row-level BCP-47 tag
  that attaches to the row's primary `rdfs:label' (emitted as a
  cons entry `(\"rdfs:label\" (list LABEL LANG))').
- Headers of the form `label@TAG' (for example `label@ko',
  `label@en-GB') become additional `rdfs:label' attribute rows
  with LANG=TAG.  The first column remains the id and the
  second column the denormalised label in `entities.label'.

Columns with neither special name ingest as plain attribute
entries keyed by header name, as before.  Files with neither
convention ingest exactly as in earlier versions."
  (let ((result nil)
        (coding-system-for-read 'utf-8)
        headers idx-lang label-at-cols)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; First non-empty line is the header.
      (while (and (not headers) (not (eobp)))
        (let ((line (string-trim-right
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (if (string-empty-p line)
              (forward-line 1)
            (setq headers (elot-source--split-csv-line line sep))
            (forward-line 1))))
      ;; Identify special header columns.
      (cl-loop for h in headers
               for i from 0
               do (cond
                   ((and (>= i 2) (equal h "lang"))
                    (setq idx-lang i))
                   ((and (>= i 2)
                         (stringp h)
                         (string-match "\\`label@\\(.+\\)\\'" h))
                    (push (cons i (match-string 1 h)) label-at-cols))))
      (setq label-at-cols (nreverse label-at-cols))
      (while (not (eobp))
        (let ((line (string-trim-right
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (unless (string-empty-p line)
            (let* ((fields (elot-source--split-csv-line line sep))
                   (id     (nth 0 fields))
                   (label  (nth 1 fields))
                   (row-lang (and idx-lang (nth idx-lang fields)))
                   (plist  nil))
              (cl-loop for h in (nthcdr 2 headers)
                       for v in (nthcdr 2 fields)
                       for i from 2
                       when (and v (not (string-empty-p v))
                                 (not (eql i idx-lang))
                                 (not (assoc i label-at-cols))) do
                       (setq plist (nconc plist (list h v))))
              ;; Row-level lang attaches to the primary label.
              (when (and row-lang (not (string-empty-p row-lang))
                         label (not (string-empty-p label)))
                (setq plist (nconc plist
                                   (list "rdfs:label"
                                         (list label row-lang)))))
              ;; label@TAG columns become extra rdfs:label rows.
              (dolist (cell label-at-cols)
                (let ((v (nth (car cell) fields))
                      (tag (cdr cell)))
                  (when (and v (not (string-empty-p v)))
                    (setq plist (nconc plist
                                       (list "rdfs:label"
                                             (list v tag)))))))
              (when (and id (not (string-empty-p id)))
                (push (list id (or label "") plist) result)))))
        (forward-line 1)))
    (nreverse result)))

(defun elot-source-parse-csv (file)
  "Parse a CSV FILE into slurp entries.
Column 1 is ID, column 2 is label, further columns become
attribute plist entries keyed by header name.  Handles
double-quoted fields with embedded commas (RFC 4180).  Primary
UC3 path."
  (elot-source--parse-separated file ","))

(defun elot-source-parse-tsv (file)
  "Parse a TSV FILE into slurp entries.
Semantics are identical to `elot-source-parse-csv', with tab as
the field separator.  Primary UC3 path."
  (elot-source--parse-separated file "\t"))

;;;; ------------------------------------------------------------------
;;;; JSON
;;;; ------------------------------------------------------------------

(defun elot-source--json-read (file)
  "Read FILE as JSON and return it with string keys and list values.
Uses `json-parse-buffer' with `:object-type 'alist' so the dict
shape is a plain alist."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (goto-char (point-min))
    (json-parse-buffer :object-type 'alist :null-object nil :false-object nil)))

(defun elot-source-parse-json (file)
  "Parse a JSON FILE containing an id->label mapping into slurp entries.

Two accepted shapes:

- Flat:   {\"id1\": \"label1\", \"id2\": \"label2\", ...}
- Nested: {\"id1\": {\"label\": \"...\", \"otherprop\": \"...\"}, ...}

In the nested form, the value of the `label' key becomes the
entry's label, and all other keys become attribute plist entries
(values are coerced to strings).  UC3-oriented; covers the
common ad-hoc mapping shape found in config / i18n / dashboard
JSON."
  (let ((data (elot-source--json-read file))
        (result nil))
    (dolist (pair data)
      (let* ((id  (let ((k (car pair)))
                    (if (symbolp k) (symbol-name k) k)))
             (val (cdr pair)))
        (cond
         ((stringp val)
          (push (list id val nil) result))
         ((or (consp val) (vectorp val))
          ;; Nested dict: alist.
          (let ((label nil) (label-lang nil) (plist nil))
            (dolist (kv val)
              (let* ((k (car kv))
                     (ks (if (symbolp k) (symbol-name k) k))
                     (v (cdr kv))
                     (vs (cond ((stringp v) v)
                               ((numberp v) (number-to-string v))
                               ((null v) "")
                               (t (format "%s" v)))))
                (cond
                 ((equal ks "label") (setq label vs))
                 ((equal ks "lang")  (setq label-lang vs))
                 ((and (stringp ks)
                       (string-match "\\`label@\\(.+\\)\\'" ks))
                  (setq plist (nconc plist
                                     (list "rdfs:label"
                                           (list vs (match-string 1 ks))))))
                 (t
                  (setq plist (nconc plist (list ks vs)))))))
            ;; Row-level lang attaches to the primary label.
            (when (and label-lang (not (string-empty-p label-lang))
                       label (not (string-empty-p label)))
              (setq plist (nconc plist
                                 (list "rdfs:label"
                                       (list label label-lang)))))
            (push (list id (or label "") plist) result)))
         (t
          ;; Scalar non-string: coerce.
          (push (list id (format "%s" val) nil) result)))))
    (nreverse result)))

;;;; ------------------------------------------------------------------
;;;; Step 1.7.3 subtask A: prefix harvest for TTL / RQ
;;;; ------------------------------------------------------------------

(defun elot-source--harvest-prefixes (file)
  "Return an alist ((PREFIX . EXPANSION) ...) harvested from FILE.

Matches both Turtle-style declarations
  `@prefix NAME: <EXPANSION> .'
and SPARQL-style declarations
  `PREFIX NAME: <EXPANSION>'
at the start of a line (with optional leading whitespace).  The
empty prefix (`@prefix : <...>') is supported and stored with the
empty string as key.

Whitespace and alignment between tokens are tolerated; simple
full-line `#' comments are skipped implicitly because the regex
requires `@prefix' / `PREFIX' at the start of the matched line.
Duplicate prefix names: the first occurrence wins (matching the
semantics ROBOT and most readers apply)."
  (let ((result nil))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \t]*\\(?:@prefix\\|[Pp][Rr][Ee][Ff][Ii][Xx]\\)[ \t]+\\([A-Za-z_][A-Za-z0-9_.-]*\\)?[ \t]*:[ \t]*<\\([^>]*\\)>"
                nil t)
          (let ((prefix    (or (match-string-no-properties 1) ""))
                (expansion (match-string-no-properties 2)))
            (unless (assoc prefix result)
              (push (cons prefix expansion) result))))))
    (nreverse result)))

(defun elot-source--entries-and-prefixes (result)
  "Normalise a parser RESULT to a cons (ENTRIES . PREFIXES).

Parsers may return either the bare list of (ID LABEL PLIST)
entries (the original v1 contract), or an extended plist of the
form (:entries ENTRIES :prefixes PREFIXES).  Detection is by the
presence of a keyword as the first element: a bare entries list
always begins with either nil or a list (a triple)."
  (cond
   ((and (consp result) (keywordp (car result)))
    (cons (plist-get result :entries)
          (plist-get result :prefixes)))
   (t
    (cons result nil))))

;;;; ------------------------------------------------------------------
;;;; Turtle
;;;; ------------------------------------------------------------------

(defcustom elot-source-ttl-label-query
  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?id ?label WHERE {
  ?id rdfs:label ?label .
}"
  "SPARQL query used by `elot-source-parse-ttl' to extract id/label pairs.
Must project at least ?id and ?label.  Additional projected
variables become attribute plist entries (column name -> key)."
  :type 'string
  :group 'elot-sources)

(defun elot-source-parse-ttl (file)
  "Parse a Turtle FILE by running ROBOT with a label-extraction SPARQL query.
Writes `elot-source-ttl-label-query' to a temp file, invokes
`elot-source--robot-run' with CSV output, then pipes the result
through the CSV parser.  Requires ROBOT; see
`elot-robot-available-p'.

Also harvests `@prefix' declarations from FILE and returns them
as part of an extended result shape
`(:entries ENTRIES :prefixes PREFIXES)' so
`elot-label-register-source' can thread them through to
`elot-db-add-prefix'.  This is what lets
`elot-global-label-display-mode' match CURIE-form tokens in a
buffer whose ids are stored as full IRIs (Step 1.7.3)."
  (unless (elot-robot-available-p)
    (user-error
     "elot-sources: ROBOT not available; set `elot-robot-jar-path'"))
  (let* ((query-tmp  (make-temp-file "elot-ttl-query-" nil ".rq"))
         (output-tmp (make-temp-file "elot-ttl-out-"   nil ".csv")))
    (unwind-protect
        (progn
          (with-temp-file query-tmp
            (insert elot-source-ttl-label-query))
          (elot-source--robot-run query-tmp file output-tmp)
          (list :entries  (elot-source-parse-csv output-tmp)
                :prefixes (elot-source--harvest-prefixes file)))
      (ignore-errors (delete-file query-tmp))
      (ignore-errors (delete-file output-tmp)))))

;;;; ------------------------------------------------------------------
;;;; SPARQL .rq
;;;; ------------------------------------------------------------------

(defcustom elot-source-rq-cache-dir-name ".elot-cache"
  "Directory name (relative to the project root) for cached .rq results."
  :type 'string
  :group 'elot-sources)

(defun elot-source--project-root (file)
  "Return the project root for FILE.
Prefers `project-current', falls back to `locate-dominating-file'
looking for common markers, and finally to FILE's own directory."
  (let ((dir (file-name-directory (expand-file-name file))))
    (or (and (fboundp 'project-current)
             (let ((default-directory dir))
               (when-let ((proj (project-current nil)))
                 (expand-file-name (if (fboundp 'project-root)
                                       (project-root proj)
                                     (car (with-no-warnings
                                            (project-roots proj))))))))
        (locate-dominating-file dir ".git")
        (locate-dominating-file dir ".elot-cache")
        dir)))

(defun elot-source-rq-cache-path (query-file data-source)
  "Return the absolute path to the cached CSV for (QUERY-FILE, DATA-SOURCE).
The filename is <query-basename>.<8-char-hash>.csv under
`elot-source-rq-cache-dir-name' at the project root."
  (let* ((root (elot-source--project-root query-file))
         (base (file-name-base query-file))
         (hash (substring
                (secure-hash 'sha1 (or data-source ""))
                0 8))
         (dir  (expand-file-name elot-source-rq-cache-dir-name root)))
    (expand-file-name (format "%s.%s.csv" base hash) dir)))

(defun elot-source--file-mtime (file)
  "Return FILE's mtime as a float, or nil if FILE does not exist."
  (let ((attrs (file-attributes file)))
    (and attrs (float-time (file-attribute-modification-time attrs)))))

(defun elot-source--rq-cache-fresh-p (cache query-file data-source)
  "Return non-nil if CACHE exists and is newer than QUERY-FILE and DATA-SOURCE.
Remote (http/https) data sources are not mtime-checked; only the
query file's mtime is considered in that case."
  (let ((c (elot-source--file-mtime cache))
        (q (elot-source--file-mtime query-file))
        (d (and data-source
                (not (string-match-p "\\`https?://" data-source))
                (elot-source--file-mtime data-source))))
    (and c q
         (>= c q)
         (or (null d) (>= c d)))))

(defun elot-source--rq-execute (query-file data-source output-file)
  "Run QUERY-FILE against DATA-SOURCE via ROBOT, writing CSV to OUTPUT-FILE.
For local-file data sources ROBOT is invoked with --input; for
http(s) endpoints, a dummy empty ontology is synthesised as input
and the query's own SERVICE clause is expected to carry the
endpoint.  Requires ROBOT; see `elot-robot-available-p'."
  (unless (elot-robot-available-p)
    (user-error
     "elot-sources: ROBOT not available; set `elot-robot-jar-path'"))
  (cond
   ((and data-source (string-match-p "\\`https?://" data-source))
    (let ((dummy-ttl (make-temp-file "elot-rq-dummy-" nil ".ttl")))
      (unwind-protect
          (progn
            ;; ROBOT requires a non-empty ontology; emit a bare prefix decl.
            (with-temp-file dummy-ttl
              (insert "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n"))
            (elot-source--robot-run query-file dummy-ttl output-file))
        (ignore-errors (delete-file dummy-ttl)))))
   (data-source
    (elot-source--robot-run query-file data-source output-file))
   (t
    (user-error "elot-sources: .rq source requires a data-source"))))

(defun elot-source-parse-rq (query-file &optional data-source)
  "Parse SPARQL QUERY-FILE against DATA-SOURCE into slurp entries.
DATA-SOURCE is a local RDF file or an http(s) endpoint URL.  If
the cached CSV is newer than both the query and (if local) the
data source, the cache is used directly and ROBOT is not
invoked.  On empty results or executor failure, the existing
cache is preserved.

Returns the extended result shape `(:entries ENTRIES :prefixes
PREFIXES)' (see Step 1.7.3).  Prefixes are harvested from
QUERY-FILE's own `PREFIX' lines, since the cached CSV does not
carry the source TTL's declarations."
  (let* ((cache (elot-source-rq-cache-path query-file data-source))
         (dir   (file-name-directory cache)))
    (unless (elot-source--rq-cache-fresh-p cache query-file data-source)
      (unless (file-directory-p dir)
        (make-directory dir t))
      (let ((tmp (concat cache ".tmp")))
        (condition-case err
            (progn
              (elot-source--rq-execute query-file data-source tmp)
              ;; Only replace cache if result is non-empty.
              (if (and (file-exists-p tmp)
                       (> (file-attribute-size (file-attributes tmp)) 0)
                       ;; At least a header + one data row.
                       (with-temp-buffer
                         (insert-file-contents tmp)
                         (goto-char (point-min))
                         (forward-line 1)
                         (not (eobp))))
                  (rename-file tmp cache t)
                (ignore-errors (delete-file tmp))))
          (error
           (ignore-errors (delete-file tmp))
           (unless (file-exists-p cache)
             (signal (car err) (cdr err)))))))
    (list :entries  (if (file-exists-p cache)
                        (elot-source-parse-csv cache)
                      nil)
          :prefixes (elot-source--harvest-prefixes query-file))))

;;;; ------------------------------------------------------------------
;;;; Step 1.4 - Source registration UI
;;;; ------------------------------------------------------------------

;; Thin glue layer over `elot-source-parse' (Step 1.3) and
;; `elot-db-*' (Steps 1.1 / 1.1.1 / 1.2).  Provides user-facing
;; commands for managing the contents of the label cache.  No new
;; persistent state is introduced here; everything is derived from
;; the DB.

(require 'tabulated-list)
(require 'elot-db)

(declare-function elot-db-init               "elot-db" (&optional path))
(declare-function elot-db-update-source      "elot-db"
                  (source data-source type data &optional file-mtime))
(declare-function elot-db-remove-source      "elot-db" (source &optional data-source))
(declare-function elot-db-list-sources       "elot-db" ())
(declare-function elot-db-source-exists-p    "elot-db" (source &optional data-source))
(declare-function elot-db-source-needs-update-p "elot-db" (file &optional data-source))
(declare-function elot-db-source-entity-count   "elot-db" (source &optional data-source))

(defvar elot-db)

(defun elot-source--ensure-db ()
  "Open the ELOT DB if not already open."
  (unless (and (boundp 'elot-db) elot-db (sqlitep elot-db))
    (elot-db-init)))

(defun elot-source--file-mtime-or-now (file)
  "Return FILE's mtime as a float, or current time if FILE does not exist."
  (or (elot-source--file-mtime file) (float-time)))

(defun elot-source--normalize-data-source (data-source)
  "Return DATA-SOURCE as a non-empty string or nil (empty string -> nil)."
  (cond
   ((null data-source) nil)
   ((and (stringp data-source) (string-empty-p data-source)) nil)
   (t data-source)))

(defun elot-source--type-for (source)
  "Return the type tag (e.g. \"csv\", \"ttl\") for SOURCE, or nil."
  (let ((ext (elot-source--extension source)))
    (and ext
         (cdr (assoc ext elot-source-supported-extensions))
         ext)))

(defun elot-source--registered-completions ()
  "Return a list of (LABEL . (SOURCE . DATA-SOURCE)) for registered sources.
LABEL is `SOURCE' when DATA-SOURCE is empty, else `SOURCE -> DATA-SOURCE'."
  (mapcar
   (lambda (row)
     (let* ((src (nth 0 row))
            (ds  (nth 1 row))
            (ds-clean (elot-source--normalize-data-source ds))
            (label (if ds-clean
                       (format "%s -> %s" src ds-clean)
                     src)))
       (cons label (cons src ds-clean))))
   (elot-db-list-sources)))

(defun elot-source--read-registered-source (prompt)
  "Prompt via `completing-read' for a registered (SOURCE . DATA-SOURCE) pair.
Returns the cons cell, or signals `user-error' when no sources are
registered."
  (let* ((cands (elot-source--registered-completions)))
    (unless cands
      (user-error "elot-sources: no sources registered in the DB"))
    (let* ((choice (completing-read prompt (mapcar #'car cands) nil t)))
      (or (cdr (assoc choice cands))
          (user-error "elot-sources: no such registered source: %s" choice)))))

(defun elot-source--read-source-for-registration ()
  "Interactive helper: read a source file path, and for `.rq' a data-source.
Returns a list (SOURCE DATA-SOURCE)."
  (let* ((source (expand-file-name
                  (read-file-name "Label source: " nil nil t
                                  (and buffer-file-name
                                       (file-name-nondirectory buffer-file-name)))))
         (ext    (elot-source--extension source))
         (ds     (when (equal ext "rq")
                   (let ((ans (read-string
                               "Data-source (local file or http(s) URL): ")))
                     (elot-source--normalize-data-source ans)))))
    (list source ds)))

(defun elot-source--do-register (source data-source &optional force)
  "Parse SOURCE against DATA-SOURCE and write results to the DB.
When FORCE is nil, returns `:skipped' (without reading the file)
if `elot-db-source-needs-update-p' reports the source is
up-to-date.  Returns `:written' with the entity count on a
successful ingest.  Signals on parser / DB errors.

If the parser returns the extended shape
`(:entries ENTRIES :prefixes PREFIXES)', the PREFIXES alist is
written to the `prefixes' table via `elot-db-add-prefix' scoped
to (SOURCE, DATA-SOURCE).  Introduced in Step 1.7.3."
  (elot-source--ensure-db)
  (let* ((ds-norm (elot-source--normalize-data-source data-source))
         (fresh   (and (not force)
                       (elot-db-source-exists-p source ds-norm)
                       (not (elot-db-source-needs-update-p source ds-norm)))))
    (if fresh
        (list :skipped source ds-norm)
      (let* ((type     (or (elot-source--type-for source)
                           (user-error
                            "elot-sources: no parser for %s" source)))
             (raw      (if (equal type "rq")
                           (elot-source-parse source ds-norm)
                         (elot-source-parse source)))
             (pair     (elot-source--entries-and-prefixes raw))
             (entries  (car pair))
             (prefixes (cdr pair))
             (mtime    (elot-source--file-mtime-or-now source))
             (n        (elot-db-update-source
                        source ds-norm type entries mtime)))
        (dolist (p prefixes)
          (ignore-errors
            (elot-db-add-prefix source ds-norm (car p) (cdr p))))
        (list :written source ds-norm n)))))

;;;###autoload
(defun elot-label-register-source (source &optional data-source)
  "Register SOURCE (a file path) in the ELOT label cache.
For `.rq' sources DATA-SOURCE is required and must be either a
local RDF file path or an http(s) endpoint URL.

If the source already exists in the DB and its file mtime has not
advanced since the last ingest, this is a no-op; use
`elot-label-refresh-source' to force a re-parse."
  (interactive (elot-source--read-source-for-registration))
  (let* ((result (elot-source--do-register source data-source nil)))
    (pcase (car result)
      (:skipped
       (message "elot-sources: %s is up-to-date; skipped" source))
      (:written
       (message "elot-sources: registered %s (%d entries)"
                source (nth 3 result))))
    result))

;;;###autoload
(defun elot-label-register-current-buffer ()
  "Register the file visited by the current buffer as a label source.
For `.rq' buffers the data-source is prompted for."
  (interactive)
  (unless buffer-file-name
    (user-error "elot-sources: current buffer is not visiting a file"))
  (let* ((source buffer-file-name)
         (ds     (when (equal (elot-source--extension source) "rq")
                   (elot-source--normalize-data-source
                    (read-string
                     "Data-source (local file or http(s) URL): ")))))
    (elot-label-register-source source ds)))

;;;###autoload
(defun elot-label-unregister-source (source &optional data-source)
  "Remove SOURCE (optionally scoped to DATA-SOURCE) from the DB.
Interactively, completes over registered sources."
  (interactive
   (let ((pair (elot-source--read-registered-source
                "Unregister source: ")))
     (list (car pair) (cdr pair))))
  (elot-source--ensure-db)
  (let ((removed (elot-db-remove-source source data-source)))
    (if removed
        (message "elot-sources: unregistered %s%s"
                 source
                 (if data-source (format " -> %s" data-source) ""))
      (message "elot-sources: no such source: %s" source))
    removed))

;;;###autoload
(defun elot-label-refresh-source (source &optional data-source)
  "Force a re-parse of SOURCE (optionally scoped to DATA-SOURCE).
Unlike `elot-label-register-source', the mtime freshness check is
bypassed; the file is always read and the DB rows replaced."
  (interactive
   (let ((pair (elot-source--read-registered-source
                "Refresh source: ")))
     (list (car pair) (cdr pair))))
  (let ((result (elot-source--do-register source data-source t)))
    (message "elot-sources: refreshed %s (%d entries)"
             source (nth 3 result))
    result))

;;;###autoload
(defun elot-label-refresh-all-sources ()
  "Refresh every registered source, continuing past individual failures.
Reports the count of successes and failures at the end; failures
include the error message for each failing source."
  (interactive)
  (elot-source--ensure-db)
  (let ((ok 0) (fail 0) (errors nil))
    (dolist (row (elot-db-list-sources))
      (let* ((src (nth 0 row))
             (ds  (elot-source--normalize-data-source (nth 1 row))))
        (condition-case err
            (progn
              (elot-source--do-register src ds t)
              (cl-incf ok))
          (error
           (cl-incf fail)
           (push (cons (if ds (format "%s -> %s" src ds) src)
                       (error-message-string err))
                 errors)))))
    (if (zerop fail)
        (message "elot-sources: refreshed %d source(s)" ok)
      (message "elot-sources: refreshed %d, %d failed: %s"
               ok fail
               (mapconcat (lambda (e) (format "%s (%s)" (car e) (cdr e)))
                          (nreverse errors) "; ")))
    (list :ok ok :failed fail :errors (nreverse errors))))

;;;; Tabulated source listing

(defvar elot-label-sources-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'elot-label-list-sources-refresh)
    (define-key map (kbd "r") #'elot-label-list-sources-refresh-entry)
    (define-key map (kbd "d") #'elot-label-list-sources-unregister-entry)
    (define-key map (kbd "+") #'elot-label-register-source)
    map)
  "Keymap for `elot-label-sources-mode'.")

(define-derived-mode elot-label-sources-mode tabulated-list-mode
  "ELOT-Sources"
  "Major mode for listing registered ELOT label sources."
  (setq tabulated-list-format
        [("Source"        40 t)
         ("Data-source"   30 t)
         ("Type"           6 t)
         ("Entities"       8
          (lambda (a b)
            (< (string-to-number (elt (cadr a) 3))
               (string-to-number (elt (cadr b) 3)))))
         ("Last updated"  20 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Source" nil))
  (tabulated-list-init-header))

(defun elot-label--format-time (t)
  "Format a float-time T for display in the source list."
  (if (and t (numberp t) (> t 0))
      (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time t))
    "-"))

(defun elot-label--build-entries ()
  "Build the `tabulated-list-entries' value from `elot-db-list-sources'."
  (elot-source--ensure-db)
  (mapcar
   (lambda (row)
     (let* ((src (nth 0 row))
            (ds  (nth 1 row))
            (typ (or (nth 2 row) ""))
            (lu  (nth 4 row))
            (n   (or (elot-db-source-entity-count src ds) 0))
            (key (cons src ds)))
       (list key
             (vector src
                     (if (and ds (not (string-empty-p ds))) ds "")
                     typ
                     (number-to-string n)
                     (elot-label--format-time lu)))))
   (elot-db-list-sources)))

;;;###autoload
(defun elot-label-list-sources ()
  "Display a tabulated list of all registered ELOT label sources."
  (interactive)
  (elot-source--ensure-db)
  (let ((buf (get-buffer-create "*ELOT Label Sources*")))
    (with-current-buffer buf
      (elot-label-sources-mode)
      (setq tabulated-list-entries (elot-label--build-entries))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(defun elot-label-list-sources-refresh ()
  "Rebuild the *ELOT Label Sources* buffer from the DB."
  (interactive)
  (setq tabulated-list-entries (elot-label--build-entries))
  (tabulated-list-print t))

(defun elot-label--current-entry ()
  "Return the (SOURCE . DATA-SOURCE) pair at point in the sources buffer.
Signals `user-error' if point is not on an entry."
  (let ((id (tabulated-list-get-id)))
    (unless (consp id)
      (user-error "No source on this line"))
    id))

(defun elot-label-list-sources-refresh-entry ()
  "Force-refresh the source at point."
  (interactive)
  (let* ((entry (elot-label--current-entry))
         (src   (car entry))
         (ds    (elot-source--normalize-data-source (cdr entry))))
    (elot-label-refresh-source src ds)
    (elot-label-list-sources-refresh)))

(defun elot-label-list-sources-unregister-entry ()
  "Unregister the source at point."
  (interactive)
  (let* ((entry (elot-label--current-entry))
         (src   (car entry))
         (ds    (elot-source--normalize-data-source (cdr entry))))
    (when (yes-or-no-p (format "Unregister %s? " src))
      (elot-label-unregister-source src ds)
      (elot-label-list-sources-refresh))))


;;;; Step 1.5 - per-project active-sources configuration

;; The buffer-local `elot-active-label-sources' variable and its
;; safe-local-variable predicate are defined in elot-db.el (so that
;; the Step 1.2 read primitives have a stable default).  The commands
;; below manage that list and, optionally, persist changes to
;; `.dir-locals.el' via `add-dir-local-variable'.

(defun elot-label--normalize-entry (entry)
  "Return ENTRY as a two-element list (SOURCE DATA-SOURCE).
DATA-SOURCE is coerced to nil when empty or missing."
  (unless (and (listp entry) (stringp (nth 0 entry)))
    (error "Invalid active-source entry: %S" entry))
  (let ((src (nth 0 entry))
        (ds  (nth 1 entry)))
    (list src
          (if (and (stringp ds) (not (string-empty-p ds))) ds nil))))

(defun elot-label--entries-equal-p (a b)
  "Non-nil if entries A and B refer to the same (SOURCE DATA-SOURCE)."
  (let ((na (elot-label--normalize-entry a))
        (nb (elot-label--normalize-entry b)))
    (and (equal (nth 0 na) (nth 0 nb))
         (equal (nth 1 na) (nth 1 nb)))))

(defun elot-label--find-entry (entry list)
  "Return the first element of LIST that matches ENTRY, or nil."
  (cl-find-if (lambda (e) (elot-label--entries-equal-p e entry)) list))

(defun elot-label--remove-entry (entry list)
  "Return LIST with every occurrence of ENTRY removed."
  (cl-remove-if (lambda (e) (elot-label--entries-equal-p e entry)) list))

(defun elot-label--project-root ()
  "Return the project root directory for the current buffer, or nil.
Tries `project-current' first, then `vc-root-dir'."
  (or (and (fboundp 'project-current)
           (let ((proj (project-current nil)))
             (when proj
               (cond
                ((fboundp 'project-root) (project-root proj))
                ((fboundp 'project-roots) (car (project-roots proj)))))))
      (and (fboundp 'vc-root-dir) (vc-root-dir))))

(defun elot-label--persist-to-dir-locals (value)
  "Persist VALUE as the `elot-active-label-sources' dir-local, if possible.
No-op when no file is associated with the current buffer or when
`add-dir-local-variable' is unavailable.  Returns non-nil on success.

The target directory is the project root (detected via
`project-current' with `vc-root-dir' as fallback), falling back to
`default-directory' if no project is identified.  To force
placement at that directory even when an ancestor already holds a
`.dir-locals.el', an empty file is created at the target first so
`add-dir-local-variable's upward walk terminates there.

`add-dir-local-variable' visits (or creates) the `.dir-locals.el'
file but leaves it modified and unsaved; we locate the visiting
buffer and save it unconditionally."
  (when (and (buffer-file-name)
             (fboundp 'add-dir-local-variable))
    (let* ((root    (elot-label--project-root))
           (dir     (file-name-as-directory
                     (expand-file-name (or root default-directory))))
           (dl-path (expand-file-name ".dir-locals.el" dir)))
      (unless (file-exists-p dl-path)
        (with-temp-file dl-path
          (insert ";;; Directory Local Variables            -*- no-byte-compile: t -*-\n"
                  ";;; For more information see (info \"(emacs) Directory Variables\")\n\n"
                  "()\n")))
      (let ((default-directory dir))
        (save-window-excursion
          (save-excursion
            (add-dir-local-variable nil 'elot-active-label-sources value))))
      (let ((dl-buffer (find-buffer-visiting dl-path)))
        (when (and dl-buffer (buffer-live-p dl-buffer))
          (with-current-buffer dl-buffer
            (when (buffer-modified-p)
              (let ((save-silently t))
                (save-buffer))))))
      (file-exists-p dl-path))))

(defvar elot-active-label-sources-change-hook nil
  "Abnormal hook run (with no args) after `elot-active-label-sources' is mutated.
Buffer-local.  `elot-global-label-display-mode' adds
`elot-global-label-display-setup' to this hook locally, so reorder
/ activate / deactivate commands automatically rebuild the
font-lock regex.  Other consumers may hook in similarly.")

(defun elot-label--dir-locals-has-entry-p ()
  "Non-nil if a `.dir-locals.el' in the ancestry binds `elot-active-label-sources'.
Looks upward from the buffer's `default-directory' (or project root
if detectable) for any `.dir-locals.el' and checks whether it
mentions `elot-active-label-sources'."
  (when-let* ((dir (or (elot-label--project-root) default-directory))
              (found (locate-dominating-file dir ".dir-locals.el"))
              (path (expand-file-name ".dir-locals.el" found)))
    (and (file-readable-p path)
         (with-temp-buffer
           (insert-file-contents path)
           (goto-char (point-min))
           (re-search-forward "elot-active-label-sources" nil t)))))

(defun elot-label--after-change (&optional value)
  "Run the change hook and, if dir-locals already has our entry, re-persist VALUE.
If VALUE is nil, uses the current `elot-active-label-sources'."
  (let ((v (or value elot-active-label-sources)))
    (when (elot-label--dir-locals-has-entry-p)
      (ignore-errors (elot-label--persist-to-dir-locals v))))
  (run-hooks 'elot-active-label-sources-change-hook))

;;;###autoload
(defun elot-label-set-active-sources (entries &optional persist)
  "Replace `elot-active-label-sources' with ENTRIES.
ENTRIES is an ordered list of (SOURCE DATA-SOURCE) two-element lists;
list order defines priority (index 0 is highest).  When PERSIST is
non-nil (interactively, with a prefix argument), write the list to
`.dir-locals.el' via `add-dir-local-variable'."
  (interactive
   (list (read (read-string "Active sources (Lisp list): "
                            (prin1-to-string elot-active-label-sources)))
         current-prefix-arg))
  (let ((norm (mapcar #'elot-label--normalize-entry entries)))
    (setq-local elot-active-label-sources norm)
    (when persist (elot-label--persist-to-dir-locals norm))
    (elot-label--after-change norm)
    norm))

;;;###autoload
(defun elot-label-activate-source (source &optional data-source position persist)
  "Activate (SOURCE DATA-SOURCE) in `elot-active-label-sources'.
POSITION is an integer index to insert at (default: end of list, i.e.
lowest priority).  If the entry is already present, it is moved to
POSITION.  With PERSIST non-nil (interactively, prefix arg), also
write the updated list to `.dir-locals.el'."
  (interactive
   (let* ((persist?    current-prefix-arg)  ; capture BEFORE any minibuffer call
          (completions (elot-source--registered-completions))
          (pick        (if completions
                           (completing-read "Activate source: "
                                            (mapcar #'car completions)
                                            nil t)
                         (read-file-name "Activate source file: " nil nil t)))
          (match       (assoc pick completions))
          (ds          (cdr match)))
     (list pick ds
           (when persist?
             (read-number "Position (0 = highest priority): " 0))
           persist?)))
  (let* ((entry (elot-label--normalize-entry (list source data-source)))
         (rest  (elot-label--remove-entry entry elot-active-label-sources))
         (idx   (cond ((null position) (length rest))
                      ((< position 0) 0)
                      ((> position (length rest)) (length rest))
                      (t position)))
         (new   (append (cl-subseq rest 0 idx)
                        (list entry)
                        (cl-subseq rest idx))))
    (setq-local elot-active-label-sources new)
    (when persist (elot-label--persist-to-dir-locals new))
    (elot-label--after-change new)
    new))

;;;###autoload
(defun elot-label-deactivate-source (source &optional data-source persist)
  "Remove (SOURCE DATA-SOURCE) from `elot-active-label-sources'.
With PERSIST non-nil (interactively, prefix arg), also write the
updated list to `.dir-locals.el'."
  (interactive
   (let* ((persist? current-prefix-arg)  ; capture BEFORE any minibuffer call
          (items (mapcar (lambda (e)
                           (cons (if (nth 1 e)
                                     (format "%s -> %s" (nth 0 e) (nth 1 e))
                                   (nth 0 e))
                                 e))
                         elot-active-label-sources))
          (pick  (completing-read "Deactivate source: "
                                  (mapcar #'car items) nil t))
          (entry (cdr (assoc pick items))))
     (list (nth 0 entry) (nth 1 entry) persist?)))
  (let* ((entry (elot-label--normalize-entry (list source data-source)))
         (new   (elot-label--remove-entry entry elot-active-label-sources)))
    (setq-local elot-active-label-sources new)
    (when persist (elot-label--persist-to-dir-locals new))
    (elot-label--after-change new)
    new))

;;;; ------------------------------------------------------------------
;;;; Step 1.7.2 - Active-sources priority UI
;;;; ------------------------------------------------------------------

(defun elot-label--move-entry (entry delta)
  "Move ENTRY by DELTA positions in `elot-active-label-sources'.
Negative DELTA moves toward higher priority (toward index 0).
Returns the new list.  No-op if ENTRY is absent or already at the
end in the requested direction."
  (let* ((list  elot-active-label-sources)
         (pos   (cl-position entry list
                             :test #'elot-label--entries-equal-p)))
    (if (null pos)
        list
      (let* ((len    (length list))
             (target (max 0 (min (1- len) (+ pos delta)))))
        (if (= target pos)
            list
          (let* ((without (elot-label--remove-entry entry list))
                 (new     (append (cl-subseq without 0 target)
                                  (list (elot-label--normalize-entry entry))
                                  (cl-subseq without target))))
            (setq-local elot-active-label-sources new)
            (elot-label--after-change new)
            new))))))

(defun elot-label--active-completions ()
  "Return an alist (LABEL . ENTRY) for `elot-active-label-sources'."
  (mapcar (lambda (e)
            (let* ((src (nth 0 e))
                   (ds  (nth 1 e))
                   (lab (if ds (format "%s -> %s" src ds) src)))
              (cons lab e)))
          elot-active-label-sources))

(defun elot-label--read-active-entry (prompt)
  "Prompt for an entry from `elot-active-label-sources'.  Return the entry."
  (let ((cands (elot-label--active-completions)))
    (unless cands (user-error "No active label sources"))
    (cdr (assoc (completing-read prompt (mapcar #'car cands) nil t)
                cands))))

;;;###autoload
(defun elot-label-move-source-up (source &optional data-source)
  "Move (SOURCE DATA-SOURCE) one position up (toward higher priority)."
  (interactive (let ((e (elot-label--read-active-entry "Move up: ")))
                 (list (nth 0 e) (nth 1 e))))
  (elot-label--move-entry (list source data-source) -1))

;;;###autoload
(defun elot-label-move-source-down (source &optional data-source)
  "Move (SOURCE DATA-SOURCE) one position down (toward lower priority)."
  (interactive (let ((e (elot-label--read-active-entry "Move down: ")))
                 (list (nth 0 e) (nth 1 e))))
  (elot-label--move-entry (list source data-source) 1))

;;;; Tabulated UI

(defvar-local elot-label--active-sources-origin nil
  "Buffer whose `elot-active-label-sources' the tabulated UI is editing.")

(defvar elot-label-active-sources-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-<up>")   #'elot-label-active-sources-move-up)
    (define-key map (kbd "M-<down>") #'elot-label-active-sources-move-down)
    (define-key map (kbd "d")        #'elot-label-active-sources-remove)
    (define-key map (kbd "+")        #'elot-label-active-sources-add)
    (define-key map (kbd "P")        #'elot-label-active-sources-persist)
    (define-key map (kbd "g")        #'elot-label-active-sources-refresh)
    map)
  "Keymap for `elot-label-active-sources-mode'.")

(define-derived-mode elot-label-active-sources-mode tabulated-list-mode
  "ELOT-Active"
  "Major mode for reordering `elot-active-label-sources'.

Key bindings:
  \\<elot-label-active-sources-mode-map>\
\\[elot-label-active-sources-move-up] / \
\\[elot-label-active-sources-move-down]  move entry up / down
  \\[elot-label-active-sources-remove]  remove entry
  \\[elot-label-active-sources-add]  add an active source
  \\[elot-label-active-sources-persist]  persist to .dir-locals.el
  \\[elot-label-active-sources-refresh]  refresh view
  \\[quit-window]  quit"
  (setq tabulated-list-format
        [("#"           3 nil)
         ("Source"     60 nil)
         ("Data-source" 40 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq header-line-format
        "  M-up/M-down move   d remove   + add   P persist   g refresh   q quit")
  (tabulated-list-init-header)
  ;; Echo-area fallback for configurations (e.g. Nano) that repurpose
  ;; `header-line-format' and thus hide the legend above.
  (run-at-time
   0 nil
   (lambda (buf)
     (when (buffer-live-p buf)
       (with-current-buffer buf
         (message
          "ELOT active sources: M-up/M-down move  d remove  + add  P persist  g refresh  q quit"))))
   (current-buffer)))

(defun elot-label--active-build-entries (entries)
  "Build `tabulated-list-entries' from the ordered ENTRIES list."
  (let ((i 0) rows)
    (dolist (e entries)
      (let* ((src (nth 0 e))
             (ds  (or (nth 1 e) "")))
        (push (list (elot-label--normalize-entry e)
                    (vector (number-to-string i) src ds))
              rows))
      (setq i (1+ i)))
    (nreverse rows)))

(defun elot-label--active-render ()
  "Populate the tabulated buffer from the origin buffer's active sources."
  (let ((entries (with-current-buffer
                     (or elot-label--active-sources-origin (current-buffer))
                   elot-active-label-sources)))
    (setq tabulated-list-entries
          (elot-label--active-build-entries entries))
    (tabulated-list-print t)))

;;;###autoload
(defun elot-label-list-active-sources ()
  "Display a tabulated buffer for reordering `elot-active-label-sources'.
Mutations performed in the buffer act on the originating buffer
(the one current when this command was invoked), so that the
buffer-local active-sources list and any buffer-local change hooks
resolve correctly."
  (interactive)
  (let ((origin (current-buffer))
        (buf (get-buffer-create "*ELOT Active Sources*")))
    (with-current-buffer buf
      (elot-label-active-sources-mode)
      (setq elot-label--active-sources-origin origin)
      (elot-label--active-render))
    (pop-to-buffer buf)))

(defun elot-label--active-entry-at-point ()
  "Return the entry at point in the active-sources buffer, or error."
  (let ((id (tabulated-list-get-id)))
    (unless (consp id) (user-error "No entry on this line"))
    id))

(defun elot-label--active-with-origin (fn)
  "Call FN (a thunk) in the origin buffer; then re-render the list.
The current line number is preserved across the refresh so point
stays on the moved/edited row."
  (let ((origin elot-label--active-sources-origin)
        (line   (line-number-at-pos)))
    (unless (and origin (buffer-live-p origin))
      (user-error "Origin buffer is gone; reopen with M-x elot-label-list-active-sources"))
    (with-current-buffer origin (funcall fn))
    (elot-label--active-render)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun elot-label-active-sources-move-up ()
  "Move the entry at point one position up (toward higher priority)."
  (interactive)
  (let ((entry (elot-label--active-entry-at-point)))
    (elot-label--active-with-origin
     (lambda () (elot-label--move-entry entry -1)))))

(defun elot-label-active-sources-move-down ()
  "Move the entry at point one position down (toward lower priority)."
  (interactive)
  (let ((entry (elot-label--active-entry-at-point)))
    (elot-label--active-with-origin
     (lambda () (elot-label--move-entry entry 1)))))

(defun elot-label-active-sources-remove ()
  "Remove the entry at point from `elot-active-label-sources'."
  (interactive)
  (let ((entry (elot-label--active-entry-at-point)))
    (elot-label--active-with-origin
     (lambda ()
       (elot-label-deactivate-source (nth 0 entry) (nth 1 entry))))))

(defun elot-label-active-sources-add ()
  "Activate a registered source in the origin buffer."
  (interactive)
  (let* ((pair (elot-source--read-registered-source "Activate source: ")))
    (elot-label--active-with-origin
     (lambda ()
       (elot-label-activate-source (car pair) (cdr pair))))))

(defun elot-label-active-sources-persist ()
  "Persist the origin buffer's `elot-active-label-sources' to `.dir-locals.el'."
  (interactive)
  (let ((origin elot-label--active-sources-origin))
    (unless (and origin (buffer-live-p origin))
      (user-error "Origin buffer is gone"))
    (with-current-buffer origin
      (if (elot-label--persist-to-dir-locals elot-active-label-sources)
          (message "elot-sources: persisted active sources to .dir-locals.el")
        (message "elot-sources: could not persist (no file buffer?)")))))

(defun elot-label-active-sources-refresh ()
  "Re-read the origin buffer's active sources and redraw."
  (interactive)
  (elot-label--active-render))

(provide 'elot-sources)

;;; elot-sources.el ends here
