;;; elot-label-display.el --- Emacs Literate Ontology Tool (ELOT): Label display   -*- lexical-binding: t; no-native-compile: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot

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

(require 'elot)

(defun elot-entities-with-plist (subsection-descriptions &optional owl-type)
  "Return a list of URI, label, and plist of attributes for a resource.
Arguments are a list SUBSECTION-DESCRIPTIONS produced by
`elot-org-subsection-descriptions' and a string for OWL-TYPE."
  (mapcar (lambda (x)
            (let* ((owl-type (or owl-type "rdfs:Resource"))
                   (header (car x))
                   (annotations-plist (flatten-tree (cdr x)))
                   (puri (elot-entity-from-header header))
                   (label
                    (if (string-match "\\(.+\\) (.*)" header)
                        (match-string 1 header) puri)))
              (list
               puri label
               (append `("rdf:label" ,label "rdf:type" ,owl-type) annotations-plist))))
          subsection-descriptions))

(defun elot-org-link-search (&rest strings)
  "Search for a `custom_id' heading in current buffer.
The concatenation of STRINGS is searched.  If found, move point there
and return position.  If not found, return nil and leave point unchanged."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (re-search-forward (concat ":custom_id:\\s-*"
                                          (apply #'concat strings)
                                          "\\s-*$")
                                  nil :noerror))))
    (when pos
      (goto-char pos))
    pos))

(defun elot-slurp-entities ()
  "Return a list of lists (URI, label, plist of attributes).
Uses `elot-org-subsection-descriptions' to read class, property, and
individual sections from an ELOT buffer.  If not in an ELOT buffer,
read using `elot-slurp-global'"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward ":ELOT-context-type: ontology" nil :noerror)
        (let ((context (elot-context-localname)))
          (append
           (when (elot-org-link-search context "-datatypes")                     (elot-entities-with-plist (elot-org-subsection-descriptions) "rdfs:Datatype"))
           (when (elot-org-link-search context "-class-hierarchy")               (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:Class"))
           (when (elot-org-link-search context "-object-property-hierarchy")     (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:ObjectProperty"))
           (when (elot-org-link-search context "-data-property-hierarchy")       (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:DatatypeProperty"))
           (when (elot-org-link-search context "-annotation-property-hierarchy") (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:AnnotationProperty"))
           (when (elot-org-link-search context "-individuals")                   (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:NamedIndividual"))))
      '())))

(defun elot-codelist-from-slurp (slurp)
  "Return a plist of the first two entries of each member of SLURP.
SLURP is a list of lists made with `elot-slurp-entities'.
The result is a plist of pairs of identifiers and labels to display."
  (flatten-tree
   (mapcar (lambda (row) (take 2 row)) slurp)))

(defun elot-attriblist-from-slurp (slurp)
  "Return a plist of the second and third entries of each member of SLURP.
SLURP is a list of lists made with `elot-slurp-entities'.  The result is
a plist of pairs of labels and plists of predicate--value pairs.
The identifier puri of the resource is added to the plist with key `\"puri\"'."
   (mapcar (lambda (row) (cons (cadr row)
                               (append `("puri" ,(car row))
                                       (caddr row))))
           slurp))

(defvar-local elot-slurp nil
  "List of resources declared in an ELOT buffer.
Each member is a list of curie, label, and plist of attributes.")
(defvar elot-slurp-global nil
  "List of resources retrieved from SPARQL endpoints.")
(defvar-local elot-codelist-ht nil
  "Hashtable holding pairs of curie and label for ELOT label-display.")
(defvar-local elot-attriblist-ht nil
  "Hashtable holding pairs of curie and attribute plist for ELOT label-display.")
(defvar-local elot-label-display 'no
  "Value says `no' or `yes' to showing labels for RDF resources.")

(defun elot-slurp-to-vars ()
  "Read resources declared in ELOT buffer into local variables.
The variables are ELOT-SLURP (plist) and ELOT-CODELIST-HT,
ELOT-ATTRIBLIST-HT (hashtable).  Outside ELOT buffers, use ELOT-SLURP-GLOBAL."
  (let ((slurp (elot-slurp-entities)))
    (setq elot-slurp (or slurp elot-slurp-global))
    (setq elot-codelist-ht
          (ht<-plist (elot-codelist-from-slurp
                      ;; only fontify what's locally declared
                      elot-slurp)))
    (setq elot-attriblist-ht
          (ht<-alist (elot-attriblist-from-slurp
                      ;; lookup includes the global list
                      (append slurp elot-slurp-global))))))

(defun elot--strip-lang-tag (s)
  "Strip quotes and language/datatype tags from string S like \"abc\"@en."
  (if (and (stringp s) (string-prefix-p "\"" s))
      (replace-regexp-in-string "^\"\\([^\"]+\\)\".*" "\\1" s)
    s))
(defun elot-codelist-id-label (idstring)
  "Given curie IDSTRING, return label if found."
  (elot--strip-lang-tag (ht-get elot-codelist-ht idstring)))
(defun elot-attriblist-label-value (idstring prop)
  "Given label IDSTRING and PROP, return puri if found."
  (plist-get (ht-get elot-attriblist-ht idstring) prop 'equal))

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
           (0 ;; all of the match
            ;; if on a headline, don't fontify
            (unless (memq (get-char-property (match-beginning 0) 'face) org-level-faces)
              ;; add tooltip
              (put-text-property (match-beginning 0) (match-end 0)
                                 'help-echo (concat (match-string 0) "  "
                                                    (elot-codelist-id-label (match-string 0)) "  ("
                                                    (elot-attriblist-label-value
                                                     (elot-codelist-id-label (match-string 0)) "rdf:type")
                                                    ")"))
              (if (eq elot-label-display 'on)
                  (progn
                    ;; label in text property, using 'elot-label-display as alias for 'display
                    (put-text-property (match-beginning 0) (match-end 0)
                                       'elot-label-display (elot-codelist-id-label (match-string 0)))
                    ;; use customizable face
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

(add-hook 'after-save-hook #'elot-slurp-to-vars nil :local)

(defvar elot-label-lookup-tmp-attriblist-ht nil
  "Temporary storage for attribute list during label lookup.")

(defun elot-label-lookup-annotations (label)
  "Helper function for `elot-label-lookup' provides preview string for LABEL."
  (let* ((attrib-plist (ht-get elot-label-lookup-tmp-attriblist-ht label))
         (rdf-type (plist-get attrib-plist "rdf:type" 'string=))
         (prefix (car (split-string (plist-get attrib-plist "puri" 'string=) ":")))
         (definition (string-replace "\n" " " (string-limit
                      (or (plist-get attrib-plist "iof-av:naturalLanguageDefinition" 'string=)
                          (plist-get attrib-plist "skos:definition" 'string=)
                          (plist-get attrib-plist "dcterms:description" 'string=)
                          (plist-get attrib-plist "rdfs:comment" 'string=)
                          "")
                      120))))
    (concat
     ;; pad annotations to col 35
     (make-string (max (- 35 (length label)) 0) 32)
     "  "
     prefix
     (make-string (max (- 10 (length prefix)) 0) 32)
     rdf-type
     (make-string (max (- 24 (length rdf-type)) 0) 32)
     definition)))

(defun elot-label-lookup ()
  "Interactive lookup of resource identifier, with completion."
  (interactive)
  (let ((completion-extra-properties
         (append completion-extra-properties
                 '(:annotation-function elot-label-lookup-annotations))))
    ;; Store the attriblist globally so annotation function can access it
    (setq elot-label-lookup-tmp-attriblist-ht elot-attriblist-ht)
    (let ((selected-label
           (completing-read
            "Label: " elot-attriblist-ht)))
      (when selected-label
        (insert " ")     ;; Insert a space to avoid getting stuck under the text property
        (backward-char 1)
        (insert (elot-attriblist-label-value selected-label "puri"))
        (forward-char 1)))))

(provide 'elot-label-display)
;;; elot-label-display.el ends here
