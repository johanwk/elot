(require 'ht)

(defun elot-entities-with-plist (subsection-descriptions &optional owl-type)
  "With a list `subsection-descriptions` produced by
`org-subsection-descriptions` and a string for `owl-type`,
return a list of uri, label, and plist of attributes."
  (mapcar (lambda (x)
            (let* ((owl-type (or owl-type "rdfs:Resource"))
                   (header (car x))
                   (annotations-plist (flatten-tree (cdr x)))
                   (puri (entity-from-header header))
                   (label 
                    (if (string-match "\\(.+\\) (.*)" header)
                        (match-string 1 header) puri)))
              (list
               puri label 
               (append `("rdf:type" ,owl-type) annotations-plist))))
          subsection-descriptions))

(defun elot-slurp-entities ()
  "Read the class, property, and individual sections with `org-subsection-descriptions`
and return a list of (uri, label, plist of attributes)"
  (save-excursion
    (beginning-of-buffer)
    (search-forward ":ELOT-context-type: ontology")
    (let ((context (elot-context-localname)))
      (append
       (org-id-goto (concat context "-class-hierarchy")) (elot-entities-with-plist (org-subsection-descriptions) "owl:Class")
       (org-id-goto (concat context "-object-property-hierarchy")) (elot-entities-with-plist (org-subsection-descriptions) "owl:ObjectProperty")
       (org-id-goto (concat context "-annotation-property-hierarchy")) (elot-entities-with-plist (org-subsection-descriptions) "owl:AnnotationProperty")
       (org-id-goto (concat context "-individuals")) (elot-entities-with-plist (org-subsection-descriptions) "owl:NamedIndividual")))))

(defun elot-codelist-from-slurp (slurp)
  "`slurp` is a list of lists made with `elot-slurp-entities`.
Return a plist of the first two entries of each member, i.e.,
pairs of identifiers and labels to display."
  (flatten-tree 
   (mapcar (lambda (row) (take 2 row)) slurp)))

(defvar-local elot-slurp nil
  "List of resources declared in an ELOT buffer. 
Each member is a list of curie, label, and plist of attributes.")
(defvar-local elot-codelist-ht nil
  "Hashtable holding pairs of curie and label for ELOT label-display.")

(defun elot-slurp-to-vars ()
  "Read resources declared in ELOT buffer into local variables
`elot-slurp` (plist) and `elot-codelist-ht` (hashtable)"
  (setq elot-slurp (elot-slurp-entities))
  (setq elot-codelist-ht
        (ht<-plist (elot-codelist-from-slurp elot-slurp))))

(defun elot-codelist-id-label (idstring)
  "Given curie `idstring`, return label if found"
  (ht-get elot-codelist-ht idstring))

(defvar elot-codelist-fontify-regexp
  "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9-.]*\\)\\>"
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
                                 'help-echo (concat (match-string 0) " "
                                                    (elot-codelist-id-label (match-string 0))))
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
  "Toggle between showing curie or `rdfs:label`, using
`elot-label-display`"
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
    (let* ((resource (car (seq-filter (lambda (r) 
                                        (equal (nth 1 r) label))
                                      tmp-elot-slurp)))
           (attrib-plist (nth 2 resource))
           (rdf-type (plist-get attrib-plist "rdf:type" 'string=))
           (prefix (car (split-string (car resource) ":")))
           (definition (string-limit
                        (or (plist-get attrib-plist "iof-av:naturalLanguageDefinition" 'string=)
                            (plist-get attrib-plist "skos:definition" 'string=)
                            (plist-get attrib-plist "rdfs:comment" 'string=))
                        120))
           )
      (concat 
       ;; pad annotations to col 30
       (make-string (max (- 30 (length label)) 0) 32)
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
        (tmp-elot-slurp elot-slurp))
    (let ((selected-label
           (completing-read 
            "Label: "
            (mapcar (lambda (resource) (nth 1 resource)) elot-slurp))))
      (if selected-label
          (insert (caar (seq-filter (lambda (r) (equal (nth 1 r) selected-label)) elot-slurp)))))))
