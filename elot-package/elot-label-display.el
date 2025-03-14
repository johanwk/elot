(require 'ht)

(defun elot-entities-with-plist (subsection-descriptions &optional owl-type)
  "With a list SUBSECTION-DESCRIPTIONS produced by
`elot-org-subsection-descriptions' and a string for OWL-TYPE,
return a list of URI, label, and plist of attributes."
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

(defun elot-slurp-entities ()
  "Read the class, property, and individual sections with `elot-org-subsection-descriptions'
and return a list of (URI, label, plist of attributes). Unless not in an ELOT buffer,
if so use `elot-slurp-global'"
  (save-excursion
    (beginning-of-buffer)
    (if (search-forward ":ELOT-context-type: ontology" nil :noerror)
        (let ((context (elot-context-localname)))
          (append
           (org-id-goto (concat context "-class-hierarchy")) (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:Class")
           (org-id-goto (concat context "-object-property-hierarchy")) (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:ObjectProperty")
           (org-id-goto (concat context "-data-property-hierarchy")) (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:DatatypeProperty")
           (org-id-goto (concat context "-annotation-property-hierarchy")) (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:AnnotationProperty")
           (org-id-goto (concat context "-individuals")) (elot-entities-with-plist (elot-org-subsection-descriptions) "owl:NamedIndividual")))
      '())))

(defun elot-codelist-from-slurp (slurp)
  "SLURP is a list of lists made with `elot-slurp-entities'.
Return a plist of the first two entries of each member, i.e.,
pairs of identifiers and labels to display."
  (flatten-tree 
   (mapcar (lambda (row) (take 2 row)) slurp)))

(defun elot-attriblist-from-slurp (slurp)
  "SLURP is a list of lists made with `elot-slurp-entities'.
Return a plist of the second and third entries of each member:
pairs of labels and plists of predicate--value pairs. The puri
of the resource is added to the plist with key `\"puri\"'."
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
  "Value says `no' or `yes' to showing labels for RDF resources")

(defun elot-slurp-to-vars ()
  "Read resources declared in ELOT buffer into local variables
`elot-slurp' (plist) and `elot-codelist-ht', `elot-attriblist-ht' (hashtable).
If not in an ELOT buffer, use `elot-slurp-global'"
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

(defun elot-codelist-id-label (idstring)
  "Given curie `idstring`, return label if found"
  (ht-get elot-codelist-ht idstring))
(defun elot-attriblist-label-value (idstring prop)
  "Given `label' and `prop', return puri if found"
  (plist-get (ht-get elot-attriblist-ht idstring) prop 'equal))

(defvar elot-codelist-fontify-regexp
  "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9.-]*\\)\\>"
  "A regular expression used to match identifiers, for use with label-display.")

(defun elot-update-codelist-fontify-regexp ()
  (if (listp elot-slurp)
      (setq elot-codelist-fontify-regexp
            (regexp-opt
             (flatten-tree 
              (mapcar (lambda (row) (car row)) elot-slurp))))
    (error "List of resources `elot-slurp` is missing, can't make regexp")))

(defvar elot-fontify-keyword nil
  "Variable holding font-lock pattern.")

(defun elot-update-fontify-keyword ()
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
                    ;; use italic shape
                    (put-text-property (match-beginning 0) (match-end 0)
                                       'face 'italic)))))))))

(defun elot-add-label-fontification ()
 "Add label fontification (as given in elot-fontify-keyword) 
to the font-lock list of keywords, then fontify."
 (progn
   (with-silent-modifications ;; don't mark as edited
     (font-lock-add-keywords 
      nil  ; current buffer
      elot-fontify-keyword 'append))
   (font-lock-fontify-buffer)))

(defun elot-remove-prop-display () 
  (remove-text-properties (point-min) (point-max) '(elot-label-display nil)))

(defun elot-label-attribs-query (&optional filter limit)
  "SPARQL query to retrieve (id, label, list of relationships)
for all resources. Optional FILTER and LIMIT is merged into the query."
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

(defun elot-retrieve-prefixes (url)
  "URI is a SPARQL endpoint URL or ontology filename.  Return the prefixes 
in the query result as a list of (uri, prefix) pairs."
  (let ((empty-construct-qry "construct where {?x ?y ?z} limit 0")
        (format ""))
    (with-temp-buffer
      ;; reusing from ELOT customized org-babel-execute:sparql
      (if (string-match-p "^http" url)  ;; querying an endpoint, or a file?
          (sparql-execute-query empty-construct-qry url format t)
        (elot-robot-execute-query empty-construct-qry url 'ttl))
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
  "Query URL with SPARQL for labels and attributes, optionally with FILTER and LIMIT. 
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
    (local-set-key (kbd "<f5>") #'elot-toggle-label-display)
    ;; use minibuffer to display info about identifier at point.
    ;; set, then activate.
    (setq help-at-pt-display-when-idle t)
    (help-at-pt-set-timer)))

(defun elot-toggle-label-display ()
  "Toggle between showing curie or rdfs:label, using `elot-label-display'."
  (interactive)
  (with-silent-modifications
    (if (local-variable-p 'elot-label-display)
        (if (eq elot-label-display 'on)
            (progn (elot-remove-prop-display)
                   (setq elot-label-display 'off)
                   (message "ELOT label-display turned off"))
          (progn (font-lock-fontify-buffer)
                 (setq elot-label-display 'on)
                 (message "ELOT label-display turned on")))
      ;; not active yet, add fontification
      (elot-add-label-fontification))))

(add-hook 'after-save-hook #'elot-slurp-to-vars nil :local)

(defun elot-label-lookup-annotations (label)
    (let* ((attrib-plist (ht-get tmp-elot-attriblist-ht label))
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
  (interactive)
  (let ((completion-extra-properties 
         (append completion-extra-properties 
                 '(:annotation-function elot-label-lookup-annotations)))
        ;; make a tmp copy since elot-attriblist-ht is a local variable
        (tmp-elot-attriblist-ht elot-attriblist-ht))
    (let ((selected-label
           (completing-read 
            "Label: " elot-attriblist-ht)))
      (if selected-label
          (insert (elot-attriblist-label-value selected-label "puri"))))))
