;;; elot-label-display.el --- Emacs Literate Ontology Tool (ELOT): Label display   -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0

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

;; This file allows for displaying rdfs:label strings instead of
;; resource identifiers while editing an ontology with ELOT.

;; By default for ELOT, switching between display of identifiers and
;; labels is assigned to the F5 key.

;; By default, labels are updated when your file is saved to disk.
;; You can also update labels with M-x `elot-label-display-setup'.

;;; Code:

;; No external dependencies -- ht/dash removed; using built-in gethash.
(require 'elot-tangle)

(defvar org-link-abbrev-alist-local)

(defun elot-org-link-search (&rest strings)
  "Search for an :ID: heading in current buffer.
The concatenation of STRINGS is searched.  If found, move point there
and return position.  If not found, return nil and leave point unchanged."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (re-search-forward (concat ":ID:\\s-*"
                                          (apply #'concat strings)
                                          "\\s-*$")
                                  nil :noerror))))
    (when pos
      (goto-char pos))
    pos))

(defvar-local elot-label-display 'off
  "Value says `off' or `on' to showing labels for RDF resources.")

(defun elot--strip-lang-tag (s)
  "Strip quotes and language/datatype tags from string S like \"abc\"@en."
  (if (and (stringp s) (string-prefix-p "\"" s))
      (replace-regexp-in-string "^\"\\([^\"]+\\)\".*" "\\1" s)
    s))
(defun elot-codelist-id-label (idstring)
  "Given curie IDSTRING, return label if found.
Two-tier lookup: local `elot-codelist-ht' first, then the ELOT label
DB (`elot-db-get-label-any') if available and not found locally."
  (or (and (hash-table-p elot-codelist-ht)
           (elot--strip-lang-tag (gethash idstring elot-codelist-ht)))
      (and (fboundp 'elot-db-get-label-any)
           (fboundp 'sqlite-open)
           (ignore-errors (elot-db-get-label-any idstring)))))
(defun elot-attriblist-label-value (idstring prop)
  "Given label IDSTRING and PROP, return value if found.
Two-tier: local `elot-attriblist-ht' first, then `elot-db-get-attr'
if available."
  (or (and (hash-table-p elot-attriblist-ht)
           (plist-get (gethash idstring elot-attriblist-ht) prop 'equal))
      (and (fboundp 'elot-db-get-attr)
           (fboundp 'sqlite-open)
           (ignore-errors (elot-db-get-attr idstring prop)))))

(defvar elot-codelist-fontify-regexp
  "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9.-]*\\)\\>"
  "A regular expression used to match identifiers, for use with label-display.")

(defun elot-update-codelist-fontify-regexp ()
  "Populate `elot-codelist-fontify-regexp'.

• First try to build a tight union-of-identifiers regexp with
  `regexp-opt' and *eagerly* compile it once.
• If compilation signals “regular expression too big” (or any other
  error), fall back to a generic CURIE pattern:

  \"\\\\<\\\\([-a-z_A-Z0-9]*\\\\):\\\\([a-z_A-Z0-9.-]*\\\\)\\\\>\"

This guarantees that the value stored is always usable during
font-lock and export."
  (let ((fallback "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9.-]*\\)\\>"))
    (condition-case err
        (progn
          (unless (listp elot-slurp)
            (error "List of resources `elot-slurp' is missing"))

          ;; Build the precise regexp …
          (let* ((ids (flatten-tree (mapcar #'car elot-slurp)))
                 (precise (regexp-opt ids)))

            ;; … and *force* Emacs to compile it once.  Any size error
            ;; happens right here.
            (string-match-p precise "")

            ;; If we got here, it’s safe to use.
            (setq elot-codelist-fontify-regexp precise)))

      (error
       (message "elot: %s – using generic CURIE regexp." (error-message-string err))
       (setq elot-codelist-fontify-regexp fallback)))))

(defvar elot-fontify-keyword nil
  "Variable holding font-lock pattern.")

(defface elot-label-face
  '((t :inherit italic))
  "Default face used to fontify labels in ELOT overlays."
  :group 'elot)

(defcustom elot-label-display-face 'elot-label-face
  "Customizable face used for ELOT label fontification.
You can choose any face (e.g. `italic', `shadow', `underline')"
  :group 'elot
  :version "29.2"
  :type 'face)

(defun elot-update-fontify-keyword ()
  "Update `elot-fontify-keyword' from collected identifier-label pairs."
  (setq elot-fontify-keyword
        `((,elot-codelist-fontify-regexp
           (0
            (unless (memq (get-char-property (match-beginning 0) 'face)
                          org-level-faces)
              ;; bind once per match — match-string and both ht lookups
              (let* ((id    (match-string 0))
                     (label (elot-codelist-id-label id))
                     (rtype (elot-attriblist-label-value label "rdf:type")))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'help-echo
                                   (concat id "  " label "  (" rtype ")"))
                (when (eq elot-label-display 'on)
                  (put-text-property (match-beginning 0) (match-end 0)
                                     'elot-label-display label)
                  (put-text-property (match-beginning 0) (match-end 0)
                                     'face elot-label-display-face)))))))))

(defun elot-add-label-fontification ()
 "Add label fontification to the font-lock list of keywords, then fontify.
The list of keywords is in `elot-fontify-keyword'."
 (progn
   (with-silent-modifications ;; don't mark as edited
     (font-lock-add-keywords
      nil  ; current buffer
      elot-fontify-keyword 'append))
   (font-lock-flush)))

(defun elot-remove-prop-display ()
  "Remove fontification added by `elot-label-display'."
  (remove-text-properties (point-min) (point-max) '(elot-label-display nil)))

(defun elot-label-attribs-query (&optional filter limit)
  "SPARQL query to retrieve (id, label, list of relationships).
Query resources, optionally with FILTER and LIMIT merged into the query."
  (concat
   (elot-prefix-block-from-alist org-link-abbrev-alist-local 'sparql)
   "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl:  <http://www.w3.org/2002/07/owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX iof-av: <https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary/>
select distinct ?id ?label ?plist
{ ?id rdfs:label ?label
  filter(lang(?label) = \"\" || lang(?label) = \"en\")    # language should be a user option
  { select ?id
    (concat( group_concat(distinct concat(str(?p), \";;\", str(?o)); separator=\";;\") ) as ?plist)
    where { ?id rdfs:label ?label .
            optional {
              values ?p { rdf:type rdfs:label
                          iof-av:naturalLanguageDefinition dcterms:description skos:definition rdfs:comment }
              ?id ?p ?o .
              filter(!(isBlank(?o))) }
            FILTER isIRI(?id)
            "
   (if filter (concat filter "\n"))
   " } group by ?id ?label }
}"
  (if limit (concat "\nlimit "
                    (if (stringp limit) limit (number-to-string limit) )))))

(defun elot-retrieve-prefixes (uri)
  "Query with SPARQL and return the prefixes.
Prefixes in the query result are returned as a list of (uri, prefix) pairs.
URI is a SPARQL endpoint URL or ontology filename."
  (let ((empty-construct-qry "construct where {?x ?y ?z} limit 0")
        (format ""))
    (with-temp-buffer
      ;; reusing from ELOT customized org-babel-execute:sparql
      (if (string-match-p "^http" uri)  ;; querying an endpoint, or a file?
          (sparql-execute-query empty-construct-qry uri format t)
        (elot-robot-execute-query empty-construct-qry uri 'ttl))
      (mapcar
       (lambda (x)
         (string-match "^\\([^ ]*:\\).*<\\([^>]+\\)>" x)
         (cons (match-string 2 x) (match-string 1 x)))
           (cl-remove ""
                  (split-string
                      (buffer-string)
                      "@prefix +")
                  :test #'equal)))))

(defun elot-replace-strings (str pairs)
  "PAIRS is a list of pairs of strings to replace in string STR."
  (seq-reduce
   (lambda (s pair)
     (string-replace (car pair) (cdr pair) s))
   pairs
   str))

(defun elot-retrieve-labels-plist (url out-file &optional filter limit)
  "Query URL with SPARQL for labels and attributes, optionally FILTER and LIMIT.
Output to OUT-FILE as an elisp list."
  (let ((labels-qry (elot-label-attribs-query filter limit))
        (format "application/sparql-results+json"))
    (with-temp-buffer
      ;; reusing from ELOT customized org-babel-execute:sparql
      (if (string-match-p "^http" url)  ;; querying an endpoint, or a file?
          (sparql-execute-query labels-qry url format t)
        (error "ROBOT ontology-file query not implemented yet for elot labels query"))
        ;; (elot-robot-execute-query labels-qry url 'json)) ; can't output json format
      (let* ((prefixes (elot-retrieve-prefixes url))
             (data-puri (elot-replace-strings (buffer-string) prefixes))
             (bindings (cdr (cadadr (json-read-from-string data-puri)))))
        (with-temp-file (expand-file-name out-file)
          (insert (pp-to-string
           (mapcar (lambda (x)
            (list
             (alist-get 'value (alist-get 'id x))
             (alist-get 'value (alist-get 'label x))
             (string-split
              (alist-get 'value (alist-get 'plist x))
              ";;" t)))
          bindings))))))))

;;(elot-retrieve-labels-plist "http://localhost:3030/bfo-core/query" "~/tmp/bfotest.el")
;;(elot-retrieve-labels-plist "https://www.qudt.org/fuseki/qudt/sparql" "~/tmp/qudttest.el")

(defun elot-read-slurp-global (&rest file-l)
  "FILE-L is a list of files holding elisp lists for label-display."
  (let ((out))
    (cl-loop for l in file-l do
             (setq out
              (append out
               (with-temp-buffer
                 (insert-file-contents (expand-file-name l))
                 (read (buffer-string))))))
    (setq elot-slurp-global out)))

(defun elot-label-display-setup ()
  "Read identifier-label pairs and initialise label-display."
  (interactive)
  (progn
    (elot-update-headline-hierarchy)
    (elot-slurp-to-vars)
    (elot-update-codelist-fontify-regexp)
    (elot-update-fontify-keyword)
    (unless (rassoc '(elot-label-display) char-property-alias-alist)
      (push '(display elot-label-display)
            char-property-alias-alist))
    (elot-add-label-fontification)
    (make-local-variable 'elot-label-display)
    (setq elot-label-display 'on)
    ;; use minibuffer to display info about identifier at point.
    ;; set, then activate.
    (setq help-at-pt-display-when-idle t)
    (help-at-pt-set-timer)))

(defun elot-toggle-label-display ()
  "Toggle between showing identifier or rdfs:label, using `elot-label-display'."
  (interactive)
  (with-silent-modifications
    (if (local-variable-p 'elot-label-display)
        (if (eq elot-label-display 'on)
            (progn (elot-remove-prop-display)
                   (setq elot-label-display 'off)
                   (message "ELOT label-display turned off"))
          (progn (font-lock-flush)
                 (setq elot-label-display 'on)
                 (message "ELOT label-display turned on")))
      ;; not active yet, add fontification
      (elot-add-label-fontification))))

(defvar elot-label-lookup-tmp-attriblist-ht nil
  "Temporary storage for attribute list during label lookup.")

(declare-function elot-db-all-active-labels "elot-db" (&optional active-sources))
(declare-function elot-db-get-all-attrs     "elot-db" (id &optional active-sources))
(declare-function elot-db-contract-uri      "elot-db" (uri &optional active-sources))
(declare-function elot-db--looks-like-uri-p "elot-db" (s))
(declare-function elot-db-label-variants    "elot-db" (id &optional active-sources))
(declare-function elot-db--select-by-language "elot-db" (rows &optional prefs))

(defun elot-label-lookup--lang-suffix-for-id (label id)
  "Return \"@LANG\" when ID has multiple `rdfs:label' language variants.
Returns the empty string when ID has at most one variant, or when
the winning variant (per `elot-preferred-languages') carries no
language tag, or when LABEL does not match the winning variant's
VALUE (defensive: avoids claiming a language for an unrelated
label).  Step 1.16.8 cosmetic: makes it visible that a singleton
stage-1 entry is one of several language variants in the source."
  (when (and (fboundp 'elot-db-label-variants)
             (fboundp 'elot-db--select-by-language))
    (let ((variants (elot-db-label-variants id)))
      (if (or (null variants) (<= (length variants) 1))
          ""
        (let* ((winner (elot-db--select-by-language variants))
               (wval   (car winner))
               (wlang  (cdr winner)))
          (if (and wval (equal wval label) wlang (> (length wlang) 0))
              (concat "@" wlang)
            ""))))))

;; Shared annotation formatter (Item A.3)
(defun elot-label-lookup--format-annotation (label prefix rdf-type definition)
  "Return a padded annotation string for completion display.
LABEL is the completion candidate; PREFIX, RDF-TYPE, and DEFINITION
are the annotation fields.  All arguments are strings (possibly empty)."
  (concat
   ;; pad annotations to col 35
   (make-string (max (- 35 (length label)) 0) 32)
   "  "
   (or prefix "")
   (make-string (max (- 10 (length (or prefix ""))) 0) 32)
   (or rdf-type "")
   (make-string (max (- 24 (length (or rdf-type ""))) 0) 32)
   (or definition "")))

(defun elot-label-lookup-annotations (label)
  "Annotation function for `elot-label-lookup' using `elot-label-lookup-tmp-attriblist-ht'.
Provides a preview string for LABEL during completing-read."
  (let* ((attrib-plist (gethash label elot-label-lookup-tmp-attriblist-ht))
         (rdf-type   (plist-get attrib-plist "rdf:type" 'string=))
         (prefix     (car (split-string (or (plist-get attrib-plist "puri" 'string=) "") ":")))
         (definition (string-replace
                      "\n" " "
                      (string-limit
                       (or (plist-get attrib-plist "iof-av:naturalLanguageDefinition" 'string=)
                           (plist-get attrib-plist "skos:definition" 'string=)
                           (plist-get attrib-plist "dcterms:description" 'string=)
                           (plist-get attrib-plist "rdfs:comment" 'string=)
                           "")
                       120))))
    (elot-label-lookup--format-annotation label prefix rdf-type definition)))

(defun elot-label-lookup--collect-attriblist ()
  "Return (COLLECTION . ANNOTATOR) for the buffer-local slurp path.
COLLECTION is `elot-attriblist-ht' (hashtable, label -> attr-plist).
ANNOTATOR is the symbol `elot-label-lookup-annotations'.  Returns
nil when the slurp hash is not populated."
  (when (and (bound-and-true-p elot-attriblist-ht)
             (hash-table-p elot-attriblist-ht)
             (> (hash-table-count elot-attriblist-ht) 0))
    (cons elot-attriblist-ht 'elot-label-lookup-annotations)))

(defun elot-label-lookup--from-attriblist ()
  "Run interactive label lookup backed by the buffer-local slurp hash tables."
  (let* ((pair (elot-label-lookup--collect-attriblist))
         (coll (car pair))
         (ann  (cdr pair))
         (completion-extra-properties
          (append completion-extra-properties
                  (list :annotation-function ann))))
    (setq elot-label-lookup-tmp-attriblist-ht coll)
    (let ((selected-label
           (completing-read "Label: " coll)))
      (when selected-label
        (insert " ")     ;; Insert a space to avoid getting stuck under the text property
        (backward-char 1)
        (insert (elot-attriblist-label-value selected-label "puri"))
        (forward-char 1)))))

(defun elot-label-lookup--id-token (id)
  "Return the insertion token for ID: a CURIE if possible, else ID."
  (if (and id
           (fboundp 'elot-db--looks-like-uri-p)
           (elot-db--looks-like-uri-p id)
           (fboundp 'elot-db-contract-uri))
      (or (car (elot-db-contract-uri id elot-active-label-sources))
          id)
    id))

(defun elot-label-lookup--id-suffix (id)
  "Return a short disambiguation suffix for ID (CURIE or IRI tail)."
  (or (elot-label-lookup--id-token id)
      (and id (if (string-match "[#/]\\([^#/]+\\)\\'" id)
                  (match-string 1 id)
                id))
      ""))

(defun elot-label-lookup--db-annotations (label)
  "Annotation function for `elot-label-lookup--from-db' (stage 1).
Looks up LABEL in `elot-label-lookup-tmp-attriblist-ht' whose
values are plists (:ids IDS :count N).  For singletons, fetches
attributes for the sole id; for groups, shows `N matches'.

Step 1.16.8: for singletons whose id has multiple `rdfs:label'
language variants, append an `@LANG' marker after the definition
so the user can see that one of several variants was picked."
  (let* ((entry (gethash label elot-label-lookup-tmp-attriblist-ht))
         (ids   (plist-get entry :ids))
         (count (or (plist-get entry :count) (length ids))))
    (if (and (integerp count) (> count 1))
        (elot-label-lookup--format-annotation
         label "" "" (format "%d matches" count))
      (let* ((id (car ids))
             (attrib-plist (and id
                                (fboundp 'elot-db-get-all-attrs)
                                (elot-db-get-all-attrs id)))
             (rdf-type   (plist-get attrib-plist "rdf:type" 'string=))
             (prefix     (when id
                           (cond
                            ((and (fboundp 'elot-db--looks-like-uri-p)
                                  (elot-db--looks-like-uri-p id)
                                  (fboundp 'elot-db-contract-uri))
                             (let ((curie (car (elot-db-contract-uri
                                                id elot-active-label-sources))))
                               (when curie (car (split-string curie ":")))))
                            ((string-match "\\`\\([^:]+\\):" id)
                             (match-string 1 id))
                            (t nil))))
             (definition (string-replace
                          "\n" " "
                          (string-limit
                           (or (plist-get attrib-plist "iof-av:naturalLanguageDefinition" 'string=)
                               (plist-get attrib-plist "skos:definition" 'string=)
                               (plist-get attrib-plist "dcterms:description" 'string=)
                               (plist-get attrib-plist "rdfs:comment" 'string=)
                               "")
                           120)))
             (lang-suffix (elot-label-lookup--lang-suffix-for-id label id))
             (definition+lang (if (> (length lang-suffix) 0)
                                  (if (> (length definition) 0)
                                      (concat definition "  " lang-suffix)
                                    lang-suffix)
                                definition)))
        (elot-label-lookup--format-annotation label prefix rdf-type definition+lang)))))

(defvar elot-label-lookup--tmp-stage2-ht nil
  "Temporary DISPLAY -> id hash used by the stage-2 annotator.")

(defun elot-label-lookup--stage2-annotations (display)
  "Annotation function for stage-2 id picker.
DISPLAY is a key in `elot-label-lookup--tmp-stage2-ht'."
  (let* ((id (gethash display elot-label-lookup--tmp-stage2-ht))
         (attrs (and id
                     (fboundp 'elot-db-get-all-attrs)
                     (elot-db-get-all-attrs id)))
         (rdf-type (or (plist-get attrs "rdf:type" 'string=) ""))
         (definition (string-replace
                      "\n" " "
                      (string-limit
                       (or (plist-get attrs "iof-av:naturalLanguageDefinition" 'string=)
                           (plist-get attrs "skos:definition" 'string=)
                           (plist-get attrs "dcterms:description" 'string=)
                           (plist-get attrs "rdfs:comment" 'string=)
                           "")
                       120)))
         (notation (or (plist-get attrs "skos:notation" 'string=) "")))
    (elot-label-lookup--format-annotation
     display "" rdf-type
     (if (> (length notation) 0)
         (format "%s  %s" notation definition)
       definition))))

(defun elot-label-lookup--pick-id-stage2 (label ids)
  "Run stage-2 completing-read over IDS for LABEL; return chosen id.
Quits cleanly on C-g without re-entering stage 1."
  (let* ((sorted (sort (copy-sequence ids) #'string-lessp))
         (ht (make-hash-table :test 'equal)))
    (dolist (id sorted)
      (puthash (elot-label-lookup--id-suffix id) id ht))
    (setq elot-label-lookup--tmp-stage2-ht ht)
    (let* ((completion-extra-properties
            (append completion-extra-properties
                    '(:annotation-function
                      elot-label-lookup--stage2-annotations)))
           (display (completing-read
                     (format "Pick id for %S: " label) ht nil t)))
      (and display (gethash display ht)))))

(defun elot-label-lookup--collect-db ()
  "Return (COLLECTION . ANNOTATOR) for the DB path.
COLLECTION is a hashtable mapping label -> plist (:ids :count) over
the active sources (see `elot-db-all-active-labels').  ANNOTATOR is
`elot-label-lookup--db-annotations'.  Returns nil when there are no
active sources or no labelled entities."
  (when (and (bound-and-true-p elot-active-label-sources)
             (fboundp 'elot-db-all-active-labels))
    (let ((raw (elot-db-all-active-labels elot-active-label-sources)))
      (when (and (hash-table-p raw) (> (hash-table-count raw) 0))
        (let ((ht (make-hash-table :test 'equal)))
          (maphash
           (lambda (label ids)
             (let ((lst (if (listp ids) ids (list ids))))
               (puthash label
                        (list :ids lst :count (length lst))
                        ht)))
           raw)
          (cons ht 'elot-label-lookup--db-annotations))))))

(defun elot-label-lookup--from-db (&optional flat)
  "Run interactive label lookup backed by the ELOT DB active sources.

Two-stage flow: stage 1 is a `completing-read' over unique labels;
when the chosen label carries more than one id, stage 2 is a
second `completing-read' over those ids with an attribute-rich
annotator.  When FLAT is non-nil, all ids are flattened into the
stage-1 collection with CURIE/IRI-tail suffixes and stage 2 is
suppressed."
  (let* ((pair (elot-label-lookup--collect-db)))
    (unless pair
      (user-error "No labelled entities in active ELOT DB sources"))
    (if flat
        (elot-label-lookup--from-db-flat (car pair))
      (let* ((ht  (car pair))
             (ann (cdr pair))
             (completion-extra-properties
              (append completion-extra-properties
                      (list :annotation-function ann))))
        (setq elot-label-lookup-tmp-attriblist-ht ht)
        (let ((selected-label (completing-read "Label: " ht)))
          (when selected-label
            (let* ((entry (gethash selected-label ht))
                   (ids   (and entry (plist-get entry :ids)))
                   (count (and entry (plist-get entry :count)))
                   (id    (cond
                           ((null ids) nil)
                           ((or (not (integerp count)) (= count 1))
                            (car ids))
                           (t (elot-label-lookup--pick-id-stage2
                               selected-label ids))))
                   (token (elot-label-lookup--id-token id)))
              (when token
                (insert " ")
                (backward-char 1)
                (insert token)
                (forward-char 1)))))))))

(defun elot-label-lookup--from-db-flat (ht)
  "Flat-presentation variant of `elot-label-lookup--from-db'.
HT is the label -> plist hash from `elot-label-lookup--collect-db'.
Every id becomes its own stage-1 entry with a CURIE/IRI-tail
suffix when its label is ambiguous; no stage 2."
  (let ((flat-ht (make-hash-table :test 'equal)))
    (maphash
     (lambda (label entry)
       (let* ((ids (plist-get entry :ids))
              (count (plist-get entry :count)))
         (if (or (not (integerp count)) (<= count 1))
             (puthash label (car ids) flat-ht)
           (dolist (id ids)
             (puthash (format "%s  [%s]" label
                              (elot-label-lookup--id-suffix id))
                      id flat-ht)))))
     ht)
    ;; Reuse stage-2 annotator: DISPLAY -> id, attributes from DB.
    (setq elot-label-lookup--tmp-stage2-ht flat-ht)
    (let* ((completion-extra-properties
            (append completion-extra-properties
                    '(:annotation-function
                      elot-label-lookup--stage2-annotations)))
           (display (completing-read "Label: " flat-ht)))
      (when display
        (let* ((id (gethash display flat-ht))
               (token (elot-label-lookup--id-token id)))
          (when token
            (insert " ")
            (backward-char 1)
            (insert token)
            (forward-char 1)))))))

;;;; -------------------------------------------------------------------
;;;; Step 1.13: union path (local + external)
;;;; -------------------------------------------------------------------

(defcustom elot-label-lookup-scope 'both
  "Which label sources `elot-label-lookup' consults.

- `local'    : the current buffer's slurp hash (`elot-attriblist-ht').
- `external' : labels indexed in the ELOT DB over the buffer's
               `elot-active-label-sources'.
- `both'     : union of the two, deduplicated by id.  Default.

The scope-aware command `elot-label-lookup' honours this
defcustom; `\[universal-argument]' overrides to `local' and
`\[universal-argument] \[universal-argument]' overrides to
`external' for a single invocation.  The sibling commands
`elot-label-lookup-local' and `elot-label-lookup-external' pin
the scope regardless of this value."
  :type '(choice (const :tag "Local buffer only"   local)
                 (const :tag "External (DB) only"  external)
                 (const :tag "Both (union)"        both))
  :group 'elot)

(defvar elot-label-lookup--tmp-union-ht nil
  "Temporary DISPLAY -> entry hash used by the union annotator.
An entry is a plist (:label L :source SYM :id ID :attrs PLIST),
where SYM is one of `local', `external', `both'.")

(defun elot-label-lookup--curie-suffix-for (entry)
  "Return a disambiguation suffix string for ENTRY, or empty string.
For an IRI id, tries `elot-db-contract-uri'; falls back to a short
tail of the IRI.  For a CURIE id, returns the id itself."
  (let ((id (plist-get entry :id)))
    (cond
     ((null id) "")
     ((and (fboundp 'elot-db--looks-like-uri-p)
           (elot-db--looks-like-uri-p id))
      (or (and (fboundp 'elot-db-contract-uri)
               (car (elot-db-contract-uri
                     id elot-active-label-sources)))
          ;; Fallback: last path/fragment segment.
          (let ((tail (if (string-match "[#/]\\([^#/]+\\)\\'" id)
                          (match-string 1 id)
                        id)))
            tail)))
     (t id))))

(defun elot-label-lookup--union-build ()
  "Build the union DISPLAY -> entry hash.
Merges local (slurp) and external (DB) collections, dedup'ing by id.
Returns the hash and also stores it in `elot-label-lookup--tmp-union-ht'."
  (let ((id->entry (make-hash-table :test 'equal))
        (label->entries (make-hash-table :test 'equal))
        (out (make-hash-table :test 'equal)))
    ;; 1) local entries keyed by id (puri).
    (when (and (bound-and-true-p elot-attriblist-ht)
               (hash-table-p elot-attriblist-ht))
      (maphash
       (lambda (label attrs)
         (let* ((id (plist-get attrs "puri" 'string=))
                (key (or id (format "__local:%s" label))))
           (puthash key
                    (list :label label :source 'local
                          :id id :attrs attrs)
                    id->entry)))
       elot-attriblist-ht))
    ;; 2) external entries keyed by id; if id already present, mark `both'.
    (let ((ext (and (bound-and-true-p elot-active-label-sources)
                    (fboundp 'elot-db-all-active-labels)
                    (elot-db-all-active-labels elot-active-label-sources))))
      (when (hash-table-p ext)
        (maphash
         (lambda (label ids)
           ;; Step 1.15: `ids' may be a list of ids (multi-id label);
           ;; fall back to treating it as a single id for legacy callers.
           (dolist (id (if (listp ids) ids (list ids)))
             (let* ((key (or id (format "__external:%s:%s" label id)))
                    (existing (gethash key id->entry)))
               (if existing
                   ;; Local wins; mark as both so annotator can show it.
                   (puthash key
                            (plist-put (copy-sequence existing)
                                       :source 'both)
                            id->entry)
                 (puthash key
                          (list :label label :source 'external
                                :id id :attrs nil)
                          id->entry)))))
         ext)))
    ;; 3) Group entries by label to detect collisions.
    (maphash
     (lambda (_id entry)
       (let* ((lbl (plist-get entry :label))
              (cur (gethash lbl label->entries)))
         (puthash lbl (cons entry cur) label->entries)))
     id->entry)
    ;; 4) Build DISPLAY -> entry with suffix disambiguation on collision.
    (maphash
     (lambda (lbl entries)
       (if (= 1 (length entries))
           (puthash lbl (car entries) out)
         (dolist (e entries)
           (let* ((suffix (elot-label-lookup--curie-suffix-for e))
                  (disp   (if (and suffix (> (length suffix) 0))
                              (format "%s  [%s]" lbl suffix)
                            ;; Last-resort: source tag so displays stay unique.
                            (format "%s  [%s]" lbl
                                    (symbol-name (plist-get e :source))))))
             (puthash disp e out)))))
     label->entries)
    (setq elot-label-lookup--tmp-union-ht out)
    out))

(defun elot-label-lookup--union-annotations (display)
  "Annotation function for the union completion collection.
DISPLAY is a key in `elot-label-lookup--tmp-union-ht'."
  (let* ((entry (gethash display elot-label-lookup--tmp-union-ht))
         (source (plist-get entry :source))
         (label  (or (plist-get entry :label) display))
         (id     (plist-get entry :id))
         ;; Attributes: prefer local plist when present, else DB fetch.
         (attrs  (or (plist-get entry :attrs)
                     (and id
                          (fboundp 'elot-db-get-all-attrs)
                          (elot-db-get-all-attrs id))))
         (rdf-type (plist-get attrs "rdf:type" 'string=))
         (prefix
          (cond
           ((eq source 'local)
            (car (split-string (or (plist-get attrs "puri" 'string=) "") ":")))
           ((and id (fboundp 'elot-db--looks-like-uri-p)
                 (elot-db--looks-like-uri-p id)
                 (fboundp 'elot-db-contract-uri))
            (let ((c (car (elot-db-contract-uri
                           id elot-active-label-sources))))
              (when c (car (split-string c ":")))))
           ((and id (string-match "\\`\\([^:]+\\):" id))
            (match-string 1 id))))
         (definition (string-replace
                      "\n" " "
                      (string-limit
                       (or (plist-get attrs "iof-av:naturalLanguageDefinition" 'string=)
                           (plist-get attrs "skos:definition" 'string=)
                           (plist-get attrs "dcterms:description" 'string=)
                           (plist-get attrs "rdfs:comment" 'string=)
                           "")
                       120)))
         (base (elot-label-lookup--format-annotation
                display prefix rdf-type definition)))
    (concat base
            (cond ((eq source 'both)     "  +external")
                  ((eq source 'external) "  [external]")
                  (t "")))))

(defun elot-label-lookup--from-union (&optional flat)
  "Run interactive label lookup over the union of local and external sources.
Delegates to `elot-label-lookup--from-attriblist' or
`elot-label-lookup--from-db' when only one scope has data.  When
FLAT is non-nil the external path uses flat presentation
(Step 1.15)."
  (let* ((local-pair    (elot-label-lookup--collect-attriblist))
         (external-pair (elot-label-lookup--collect-db)))
    (cond
     ((and local-pair (not external-pair))
      (elot-label-lookup--from-attriblist))
     ((and external-pair (not local-pair))
      (elot-label-lookup--from-db flat))
     ((and local-pair external-pair)
      (let* ((table (elot-label-lookup--union-build))
             (completion-extra-properties
              (append completion-extra-properties
                      '(:annotation-function
                        elot-label-lookup--union-annotations))))
        (let ((selected (completing-read "Label: " table)))
          (when selected
            (let* ((entry  (gethash selected table))
                   (source (plist-get entry :source))
                   (id     (plist-get entry :id))
                   (attrs  (plist-get entry :attrs))
                   (token
                    (cond
                     ;; Local / both: prefer the local puri.
                     ((memq source '(local both))
                      (or (plist-get attrs "puri" 'string=) id))
                     ;; External IRI: contract to CURIE if possible.
                     ((and id (fboundp 'elot-db--looks-like-uri-p)
                           (elot-db--looks-like-uri-p id)
                           (fboundp 'elot-db-contract-uri))
                      (or (car (elot-db-contract-uri
                                id elot-active-label-sources))
                          id))
                     (t id))))
              (when token
                (insert " ")
                (backward-char 1)
                (insert token)
                (forward-char 1)))))))
     (t
      (user-error
       "No ELOT labels available: no slurp data and no active DB sources")))))

;;;###autoload
(defun elot-label-lookup (&optional arg)
  "Interactive lookup of resource identifier, with completion.

The set of candidates is determined by `elot-label-lookup-scope'
(default `both', the union of local slurp data and the ELOT DB
active sources).  A single `\\[universal-argument]' prefix pins
the scope to `local' for this invocation only.  To pin scope to
`external', use the sibling command `elot-label-lookup-external'
(which is unambiguous, whereas a bare prefix-arg is overloaded).

A double `\\[universal-argument] \\[universal-argument]' prefix
forces /flat presentation/ (Step 1.15): when a label is borne by
many ids, all ids are shown in a single completing-read with
disambiguating CURIE/IRI-tail suffixes, rather than the default
two-stage picker.  Flat presentation adjusts /display/, not scope.

See also `elot-label-lookup-local' and `elot-label-lookup-external'."
  (interactive "P")
  (let* ((flat (equal arg '(16)))
         (elot-label-lookup-scope
          (cond
           ((equal arg '(4))  'local)
           (t                 elot-label-lookup-scope))))
    (pcase elot-label-lookup-scope
      ('local
       (if (elot-label-lookup--collect-attriblist)
           (elot-label-lookup--from-attriblist)
         (user-error
          "Scope `local' but no slurp data in this buffer")))
      ('external
       (if (elot-label-lookup--collect-db)
           (elot-label-lookup--from-db flat)
         (user-error
          "Scope `external' but no active ELOT DB sources")))
      (_ ; both
       (elot-label-lookup--from-union flat)))))

;;;###autoload
(defun elot-label-lookup-local ()
  "Run `elot-label-lookup' pinned to scope `local'.
Offers candidates from the buffer-local slurp hash only."
  (interactive)
  (let ((elot-label-lookup-scope 'local))
    (call-interactively #'elot-label-lookup)))

;;;###autoload
(defun elot-label-lookup-external ()
  "Run `elot-label-lookup' pinned to scope `external'.
Offers candidates from the ELOT DB active sources only."
  (interactive)
  (let ((elot-label-lookup-scope 'external))
    (call-interactively #'elot-label-lookup)))

;;;; -------------------------------------------------------------------
;;;; Step 1.14: Attribute-driven eldoc / hover (Tier 1 = eldoc backend)
;;;; -------------------------------------------------------------------
;;
;; The `elot-global-label-display-mode' label overlay answers the
;; question "what does this id mean" one word at a time.  Tier 1 of
;; Step 1.14 surfaces the /next/ level of detail (label, rdf:type,
;; definition, source provenance) without leaving the buffer, as a
;; one-line echo-area summary driven by eldoc.  The implementation is
;; deliberately zero-ceremony: it registers a buffer-local backend on
;; `eldoc-documentation-functions' and falls through on no match.
;;
;; In ELOT Org buffers, the slurp path (`elot-codelist-ht' /
;; `elot-attriblist-ht', set up by `elot-label-display-setup') is
;; preferred over the DB, mirroring the Step 1.12 dispatcher ordering.

(declare-function elot-db-expand-curie  "elot-db" (curie &optional active-sources))
(declare-function elot-db-get-label-any "elot-db" (token &optional active-sources))

(defcustom elot-global-label-display-eldoc t
  "Whether `elot-global-label-display-mode' registers an eldoc backend.
When non-nil (the default), activating the mode installs a
buffer-local function on `eldoc-documentation-functions' that
summarises the identifier under point using slurp data or the
ELOT DB.  Set to nil before enabling the mode to opt out."
  :type 'boolean
  :group 'elot)

(defcustom elot-global-label-display-show-source t
  "Whether the eldoc/hover summary carries a provenance marker.
When non-nil, the one-line summary produced by
`elot-global--eldoc-function' ends with `[src: <source>]' when
the winning row's source is known.  Set to nil to suppress."
  :type 'boolean
  :group 'elot)

(defun elot--id-at-point ()
  "Return the identifier under point as a string, or nil.
Recognises, in order: angle-bracketed IRIs (<http://...>), bare
absolute IRIs, CURIEs (prefix:localname), and plain bare ids.
The value returned is the identifier's textual form as it would
be stored in the DB's `entities.id' column or keyed in the slurp
hashtables; callers decide how to canonicalise."
  (or (let ((b (bounds-of-thing-at-point 'symbol)))
        (when b
          (let ((s (buffer-substring-no-properties (car b) (cdr b))))
            (when (and s (> (length s) 0))
              s))))
      ;; Angle-bracketed IRI: <http://...>
      (save-excursion
        (let ((p (point)))
          (save-match-data
            (when (re-search-backward "<" (line-beginning-position) t)
              (when (looking-at "<\\([^<>[:space:]]+\\)>")
                (when (and (<= (match-beginning 0) p)
                           (>= (match-end 0) p))
                  (match-string-no-properties 1)))))))))

(defun elot--format-attribute-summary (id label rdf-type definition source-name)
  "Compose a one-line summary for ID from LABEL, RDF-TYPE, DEFINITION, SOURCE-NAME.
Any of LABEL / RDF-TYPE / DEFINITION / SOURCE-NAME may be nil or
empty and is then omitted.  SOURCE-NAME is included only when
`elot-global-label-display-show-source' is non-nil.

Format:

  ID  [LABEL]  (RDF-TYPE)  -- DEFINITION   [src: SOURCE-NAME]"
  (let ((parts (list id)))
    (when (and label (> (length label) 0))
      (push (format "[%s]" label) parts))
    (when (and rdf-type (> (length rdf-type) 0))
      (push (format "(%s)" rdf-type) parts))
    (let ((s (mapconcat #'identity (nreverse parts) "  ")))
      (when (and definition (> (length definition) 0))
        (setq s (concat s "  -- "
                        (string-replace
                         "\n" " "
                         (string-limit definition 200)))))
      (when (and elot-global-label-display-show-source
                 source-name (> (length source-name) 0))
        (setq s (concat s "  [src: " source-name "]")))
      s)))

(defun elot--summary-from-slurp (id)
  "Compose an attribute summary for ID from slurp hashtables, or nil.
Consulted in ELOT Org buffers where `elot-codelist-ht' and
`elot-attriblist-ht' are populated by `elot-label-display-setup'."
  (when (and (boundp 'elot-codelist-ht)
             (hash-table-p elot-codelist-ht))
    (let ((raw (gethash id elot-codelist-ht)))
      (when raw
        (let* ((label (elot--strip-lang-tag raw))
               (rdf-type (and (boundp 'elot-attriblist-ht)
                              (hash-table-p elot-attriblist-ht)
                              (plist-get (gethash label elot-attriblist-ht)
                                         "rdf:type" 'equal)))
               (definition
                (and (boundp 'elot-attriblist-ht)
                     (hash-table-p elot-attriblist-ht)
                     (let ((p (gethash label elot-attriblist-ht)))
                       (or (plist-get p "iof-av:naturalLanguageDefinition" 'equal)
                           (plist-get p "skos:definition" 'equal)
                           (plist-get p "dcterms:description" 'equal)
                           (plist-get p "rdfs:comment" 'equal))))))
          (elot--format-attribute-summary
           id label rdf-type definition nil))))))

(defun elot--summary-from-db (id)
  "Compose an attribute summary for ID from the ELOT DB, or nil.
Tries ID as-is first; if the DB returns nothing and the token is
a CURIE, retries with the expanded IRI.  Uses the `:source-origin'
entry added by `elot-db-get-all-attrs' (Step 1.14) for the
provenance marker."
  (when (and (bound-and-true-p elot-active-label-sources)
             (fboundp 'elot-db-get-all-attrs))
    (let* ((attrs (ignore-errors
                    (elot-db-get-all-attrs
                     id elot-active-label-sources)))
           (attrs (or attrs
                      ;; CURIE -> IRI retry
                      (and (fboundp 'elot-db--looks-like-curie-p)
                           (elot-db--looks-like-curie-p id)
                           (fboundp 'elot-db-expand-curie)
                           (let ((iri (ignore-errors
                                        (elot-db-expand-curie
                                         id elot-active-label-sources))))
                             (and iri
                                  (ignore-errors
                                    (elot-db-get-all-attrs
                                     iri elot-active-label-sources)))))
                      ;; IRI -> CURIE retry
                      (and (fboundp 'elot-db--looks-like-uri-p)
                           (elot-db--looks-like-uri-p id)
                           (fboundp 'elot-db-contract-uri)
                           (let ((curies (ignore-errors
                                           (elot-db-contract-uri
                                            id elot-active-label-sources))))
                             (cl-some
                              (lambda (c)
                                (ignore-errors
                                  (elot-db-get-all-attrs
                                   c elot-active-label-sources)))
                              curies))))))
      (when attrs
        (let* ((label (or (plist-get attrs "rdfs:label" #'string=)
                          ;; Fallback: labels live in `entities.label',
                          ;; not in `attributes', unless the ingestor
                          ;; redundantly emitted `rdfs:label'.
                          (and (fboundp 'elot-db-get-label)
                               (ignore-errors
                                 (elot-db-get-label
                                  id elot-active-label-sources)))))
               (rdf-type (plist-get attrs "rdf:type" #'string=))
               (definition
                (or (plist-get attrs "iof-av:naturalLanguageDefinition" #'string=)
                    (plist-get attrs "skos:definition" #'string=)
                    (plist-get attrs "dcterms:description" #'string=)
                    (plist-get attrs "rdfs:comment" #'string=)))
               (origin (plist-get attrs :source-origin))
               (source-name (and (consp origin)
                                 (format "%s" (car origin)))))
          (elot--format-attribute-summary
           id (elot--strip-lang-tag label)
           rdf-type definition source-name))))))

(defun elot-global--eldoc-function (&optional _callback &rest _ignored)
  "ELOT eldoc backend for `elot-global-label-display-mode'.
Returns a one-line summary of the identifier under point, or nil
so the next eldoc backend on `eldoc-documentation-functions' can
have a turn.  In ELOT Org buffers the slurp path is consulted
first; the DB is consulted otherwise or as a fallback."
  (when elot-global-label-display-eldoc
    (let ((id (elot--id-at-point)))
      (when id
        (or (elot--summary-from-slurp id)
            (elot--summary-from-db id))))))

;;;; -------------------------------------------------------------------
;;;; Step 1.7: Activation for non-ELOT buffers
;;;; -------------------------------------------------------------------
;;
;; `elot-global-label-display-mode' is a general-purpose buffer-local
;; minor mode that decorates identifiers in *any* buffer (Turtle,
;; SPARQL, CSV, Python, Markdown, shell scripts, etc.) with labels
;; drawn from the active label sources recorded in the ELOT DB.
;;
;; It has no dependency on:
;;   - `elot-slurp' being populated in the buffer
;;   - prefix tables / CURIE syntax
;;   - the buffer's major mode
;;
;; The font-lock matcher is built from the DB: `regexp-opt' is
;; applied to the union of `entities.id' strings across the active
;; sources (`elot-db-all-active-ids'), producing a literal-token
;; matcher.  This is what lets UC3 buffers (e.g. a Python file
;; containing `EMP-12345') get labelled without any language-aware
;; tokenising.
;;
;; Scope limit (v1): matching is literal tokens only.  No capture
;; groups, no per-source regex templates, no partial-string
;; substitution inside larger tokens.  See the Decisions Log in
;; ELOT-DB-PLAN.org.

(declare-function elot-db-all-active-ids "elot-db"
                  (&optional active-sources include-curies))
(declare-function elot-db-init "elot-db" (&optional path))

(defcustom elot-global-label-display-max-ids 10000
  "Soft cap on the number of ids fed to `regexp-opt' by the global mode.
When `elot-db-all-active-ids' (augmented with CURIE contractions
in Step 1.7.3) exceeds this value, `elot-global--install' logs a
warning and installs no matcher; the mode itself stays enabled so
the toggle UX remains consistent.  Set to nil to disable the cap."
  :type '(choice (integer :tag "Maximum id count")
                 (const :tag "No cap" nil))
  :group 'elot)

(defvar-local elot-global--fontify-regexp nil
  "Buffer-local regexp used by `elot-global-label-display-mode'.
Built from `elot-db-all-active-ids' at activation time.")

(defvar-local elot-global--keywords nil
  "Buffer-local font-lock keywords installed by the global mode.")

(defconst elot-global--fallback-curie-regexp
  "\\_<\\([-a-z_A-Z0-9]+\\):\\([-a-z_A-Z0-9.]+\\)\\_>"
  "Generic CURIE-shape regexp used when `regexp-opt' produces a pattern
too large for the Emacs regex engine.  Matches `prefix:localname'
tokens; the font-lock keyword body then filters via
`elot-db-get-label-any', so non-entries are cheap no-ops.")

(defun elot-global--try-compile (regexp)
  "Return REGEXP if it compiles, nil otherwise.
Forces eager compilation via `string-match-p' so that
\"regular expression too big\" is caught here rather than later
inside font-lock."
  (and regexp
       (condition-case _err
           (progn (string-match-p regexp "") regexp)
         (error nil))))

(defun elot-global--build-regexp (ids)
  "Return a DB-driven font-lock regexp matching literal IDS.
IDS is a list of strings (entity identifiers), typically IRIs and
their CURIE contractions.  Tries three tiers:

  1. `regexp-opt' over IDS with symbol boundaries (best coverage).
  2. On compile failure, retry with CURIE-shape entries only
     (IRIs dropped; usually fits because CURIEs share prefixes).
  3. On still-too-big, fall back to a generic CURIE pattern
     (`elot-global--fallback-curie-regexp'); the matcher body
     filters via `elot-db-get-label-any' so false-positive CURIEs
     are no-ops.

Returns nil when IDS is empty."
  (when ids
    (or (elot-global--try-compile (regexp-opt ids 'symbols))
        (let ((curies (seq-filter
                       (lambda (s)
                         (and (string-match-p ":" s)
                              (not (string-match-p "://" s))))
                       ids)))
          (and curies
               (elot-global--try-compile (regexp-opt curies 'symbols))))
        (prog1 elot-global--fallback-curie-regexp
          (message
           "elot-global-label-display-mode: %d ids too large for regexp-opt; using generic CURIE fallback"
           (length ids))))))

(defun elot-global--help-echo (_window object pos)
  "Lazy `help-echo' callback for `elot-global-label-display-mode'.
Composes a rich one-line summary (id, label, rdf:type, definition,
source provenance) for the identifier at POS in OBJECT, by calling
`elot--summary-from-db'.  Falls back to a plain `id  label' string
if the DB lookup yields nothing.  Computed on demand so per-match
font-lock cost stays at zero."
  (let* ((id (cond
              ((bufferp object)
               (with-current-buffer object
                 (or (get-text-property pos 'elot-global--id)
                     (let ((b (bounds-of-thing-at-point 'symbol)))
                       (and b (buffer-substring-no-properties
                               (car b) (cdr b)))))))
              ((stringp object)
               (or (get-text-property pos 'elot-global--id object)
                   nil))
              (t nil))))
    (when id
      (or (ignore-errors (elot--summary-from-db id))
          (let ((label (and (fboundp 'elot-db-get-label-any)
                            (ignore-errors
                              (elot-db-get-label-any id)))))
            (if label (concat id "  " label) id))))))

(defun elot-global--build-keywords (regexp)
  "Return a font-lock keyword form decorating REGEXP with labels.
The matcher looks the matched id up via `elot-db-get-label-any'
and, on a hit, installs the `elot-label-display' text property
(aliased to `display' via `char-property-alias-alist').  The
property is only applied when the buffer-local toggle
`elot-label-display' is `on', so \[elot-toggle-label-display]
can hide labels without disabling the mode.

`help-echo' is installed as a *function* (`elot-global--help-echo')
so the rich summary (rdf:type, definition, source) is computed
lazily by `help-at-pt' or tooltip mouse-over rather than during
fontification.  The matched id is stashed in the
`elot-global--id' text property so the lazy callback can recover
it without re-tokenising the buffer.

The matcher body is wrapped in `condition-case' so a failing
lookup cannot silently disable font-lock for the whole buffer
(Step 1.7.3 safety net)."
  `((,regexp
     (0 (condition-case err
            ;; Capture match-beginning/end BEFORE calling lookup helpers,
            ;; which use `string-match' internally and would otherwise
            ;; clobber the global match data (Step 1.7.3 fix).
            (let* ((mb (match-beginning 0))
                   (me (match-end 0))
                   (id (match-string 0))
                   (label (and (fboundp 'elot-db-get-label-any)
                               (ignore-errors
                                 (elot-db-get-label-any id)))))
              (when label
                (put-text-property mb me 'elot-global--id id)
                (put-text-property mb me 'help-echo
                                   #'elot-global--help-echo)
                (when (eq elot-label-display 'on)
                  (put-text-property mb me 'elot-label-display label)
                  (put-text-property mb me 'face
                                     (if (boundp 'elot-label-display-face)
                                         elot-label-display-face
                                       'italic)))))
          (error
           (message "elot-global-label-display-mode: matcher error: %S" err)
           nil))))))

(defun elot-global--install ()
  "Build regexp + keywords from the DB and install them in this buffer."
  (when (and (fboundp 'elot-db-init)
             (or (not (boundp 'elot-db)) (null elot-db)))
    (ignore-errors (elot-db-init)))
  ;; Ensure the `display' alias is registered so that setting the
  ;; `elot-label-display' text property actually swaps the glyph.
  (unless (rassoc '(elot-label-display) char-property-alias-alist)
    (push '(display elot-label-display) char-property-alias-alist))
  (let* ((ids (and (fboundp 'elot-db-all-active-ids)
                   (ignore-errors (elot-db-all-active-ids nil t))))
         (capped (and elot-global-label-display-max-ids
                      ids
                      (> (length ids) elot-global-label-display-max-ids)))
         (regexp (and ids (not capped)
                      (elot-global--build-regexp ids))))
    (when capped
      (message
       "elot-global-label-display-mode: %d ids exceeds cap of %d; no matcher installed (customize `elot-global-label-display-max-ids')"
       (length ids) elot-global-label-display-max-ids))
    (setq elot-global--fontify-regexp regexp)
    (setq elot-global--keywords
          (and regexp (elot-global--build-keywords regexp)))
    (when elot-global--keywords
      (with-silent-modifications
        (font-lock-add-keywords nil elot-global--keywords 'append))
      (font-lock-flush))))

(defun elot-global--uninstall ()
  "Remove decorations and font-lock keywords installed by the mode."
  (when elot-global--keywords
    (with-silent-modifications
      (font-lock-remove-keywords nil elot-global--keywords)
      (remove-text-properties (point-min) (point-max)
                              '(elot-label-display nil
                                help-echo nil
                                elot-global--id nil))))
  (setq elot-global--keywords nil)
  (setq elot-global--fontify-regexp nil)
  (font-lock-flush))

(defvar-local elot-global--no-sources-warned nil
  "Non-nil once the no-active-sources hint has been emitted in this buffer.
Cleared by `elot-global-label-display-setup' when the active-sources
list becomes non-nil, so the hint is re-emitted if sources are later
removed again.")

(defun elot-global--maybe-warn-no-sources ()
  "Emit a one-shot hint when the mode is enabled without active sources.
Debounced via the buffer-local `elot-global--no-sources-warned' so
toggling the mode off and on in a buffer that has no sources does not
spam the echo area.  The flag is cleared by
`elot-global-label-display-setup' when sources eventually appear."
  (when (and (null elot-active-label-sources)
             (not elot-global--no-sources-warned))
    (setq elot-global--no-sources-warned t)
    (message
     "elot-global-label-display-mode: no active label sources in this buffer; use M-x elot-label-activate-source or set via .dir-locals.el")))

;;;###autoload
(define-minor-mode elot-global-label-display-mode
  "Display labels from ELOT active label sources in any buffer.

When enabled, identifiers present in the active sources
(`elot-active-label-sources', usually set via `.dir-locals.el' or
`M-x elot-label-activate-source') are decorated in place with
their labels.  Works in any major mode: the font-lock matcher is
built from the DB, not from a language grammar.

If the buffer has no active sources, the mode still turns on (so
the toggle state is honest) but nothing is decorated; a one-shot
hint is emitted in the echo area pointing at
`elot-label-activate-source'.  Adding a source afterwards triggers
an automatic refresh via `elot-active-label-sources-change-hook'.

Toggle with \\[elot-toggle-label-display] (F5) once enabled.  Use
\\[elot-global-label-display-setup] for a one-shot refresh after
registering or activating new sources."
  :lighter " ELOT-L"
  :keymap (make-sparse-keymap)
  (if elot-global-label-display-mode
      (progn
        (elot-global--install)
        (make-local-variable 'elot-label-display)
        (setq elot-label-display 'on)
        (local-set-key (kbd "<f5>") #'elot-toggle-label-display)
        (local-set-key (kbd "C-c C-x r") #'elot-label-lookup)
        (when elot-global-label-display-eldoc
          (add-hook 'eldoc-documentation-functions
                    #'elot-global--eldoc-function nil t)
          ;; Make sure eldoc is actually running in this buffer.
          (when (fboundp 'eldoc-mode)
            (eldoc-mode 1)))
        (when (boundp 'elot-active-label-sources-change-hook)
          (add-hook 'elot-active-label-sources-change-hook
                    #'elot-global-label-display-setup nil t))
        (elot-global--maybe-warn-no-sources))
    (elot-global--uninstall)
    (setq elot-label-display 'off)
    (remove-hook 'eldoc-documentation-functions
                 #'elot-global--eldoc-function t)
    (when (boundp 'elot-active-label-sources-change-hook)
      (remove-hook 'elot-active-label-sources-change-hook
                   #'elot-global-label-display-setup t))))

;;;###autoload
(defun elot-global-label-display-setup ()
  "One-shot: (re)build the DB-driven font-lock regexp in this buffer.
Use after registering or activating new label sources to refresh
decorations without toggling the minor mode off and on."
  (interactive)
  (elot-global--uninstall)
  (elot-global--install)
  (unless elot-global-label-display-mode
    (elot-global-label-display-mode 1))
  ;; If sources are now present, clear the one-shot warning flag so
  ;; that a later drop back to empty re-emits the hint.  If sources
  ;; are still nil, emit the hint (debounced).
  (if elot-active-label-sources
      (setq elot-global--no-sources-warned nil)
    (elot-global--maybe-warn-no-sources)))

;; If `elot-mode' is (or becomes) loaded, surface its ELOT menu in
;; buffers where only `elot-global-label-display-mode' is active.
;; The menu is bound with `:visible (not elot-mode)' so that it
;; disappears once elot-mode activates and adds its own copy via
;; elot-mode-map -- preventing duplicate ELOT menu entries.
;; The sibling snippet in elot-mode.el covers the opposite load order.
(with-eval-after-load 'elot-mode
  (when (and (boundp 'elot-global-label-display-mode-map)
             (boundp 'elot-menu))
    (define-key elot-global-label-display-mode-map
                [menu-bar ELOT]
                `(menu-item "ELOT" ,elot-menu
                            :visible (not (bound-and-true-p elot-mode))))))

(provide 'elot-label-display)
;;; elot-label-display.el ends here
