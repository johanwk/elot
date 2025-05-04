;;; elot-lint.el  -*- lexical-binding: t; -*-
(require 'org-element)
(require 'org-lint)
(require 'ox)

(defun elot--resourcedefs-here-p ()
  "Return t if headline at point sets :resourcedefs: yes."
  (string-equal (org-entry-get nil "resourcedefs") "yes"))

(defun elot--inside-resourcedefs-p ()
  "Return t when point is inside any :resourcedefs: yes section."
  (string-equal (org-entry-get-with-inheritance "resourcedefs") "yes"))

(declare-function elot-entity-from-header "elot")
(declare-function elot-unprefix-uri "elot")
(declare-function elot-context-type "elot")
(declare-function elot-context-localname "elot")
(declare-function elot-default-prefix "elot")

(defvar elot-slurp)
(defvar elot-omn-all-keywords)
(defvar elot-owl-builtin-resources)

(defun elot-check-nodeclare-id-prefix-label (tree)
  "ELOT rule: check ID, prefix, and label format under :resourcedefs:."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (elot--inside-resourcedefs-p)
                   (not (elot--resourcedefs-here-p)))
          (let* ((title (org-get-heading nil t))
                 (tags  (org-element-property :tags hl))
                 (nodeclare (member "nodeclare" tags)))
            (unless nodeclare
              (let* ((entity (condition-case nil
                                 (elot-entity-from-header title 'noerror)
                               (error nil)))
                     (label (if (and title (string-match "\\`\\(.*\\) (\\([^()]+\\))\\'" title))
                                 (match-string 1 title)
                               title)))
                (cond
                 ((not entity)
                  (push (list (point)
                              (propertize "ERROR: No identifier found – add CURIE/URI or tag :nodeclare:"
                                          'face 'error))
                        issues))
                 ((not (stringp entity))
                  (push (list (point)
                              (propertize "ERROR: Malformed identifier value"
                                          'face 'error))
                        issues))
                 ((null (elot-unprefix-uri (substring-no-properties entity) org-link-abbrev-alist-local 'noerror))
                  (push (list (point)
                              (propertize "ERROR: Unknown prefix in identifier"
                                          'face 'error))
                        issues))
                 ((not (or (not (string-match-p "\"" label))
                           (string-match-p "^\"[^\"]+\"@[[:alpha:]-]+$" label)))
                  (push (list (point)
                              (propertize "WARNING: Label should have no quotes or be a \"text\"@lang string"
                                          'face 'warning))
                        issues)))))))
        nil)
      tree)
    issues))

(defun elot--check-omn-args (omn-args point issues)
  "Check :header-args:omn string OMN-ARGS for valid :tangle file and :noweb yes.
Add warnings or errors to ISSUES at POINT."
  ;; Check for :tangle filename
  (if (and (string-match ":tangle[ \t]+\\([^ \t:]+\\.omn\\)" omn-args)
         (match-string 1 omn-args))
    (let ((file (match-string 1 omn-args)))
      (condition-case nil
          (expand-file-name file)
        (error
         (push (list point
                     (propertize (format "ERROR: Invalid tangle file path: %s" file)
                                 'face 'error))
               issues))))
  (push (list point
              (propertize "ERROR: :tangle missing or invalid in :header-args:omn"
                          'face 'error))
        issues))
  ;; Check for :noweb yes followed by whitespace or end of string
  (unless (string-match ":noweb[ \t]+yes\\(\\s-\\|\\'\\)" omn-args)
    (push (list point
                (propertize "ERROR: :noweb yes missing or malformed in :header-args:omn"
                            'face 'error))
          issues))
  issues)


(defun elot-check-ontology-header (tree)
  "ELOT rule: check top-level ontology header properties."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (= (org-element-property :level hl) 1)
                   (string= (elot-context-type) "ontology"))
          (let* ((id (org-entry-get nil "ID"))
                 (localname (elot-context-localname))
                 (prefix (elot-default-prefix))
                 (omn-args (org-entry-get nil "header-args:omn")))
            (when (or (null id) (string= id ""))
              (push (list (point)
                          (propertize "ERROR: Top-level heading missing :ID:" 'face 'error))
                    issues))
            (when (and id localname (not (string= id localname)))
              (push (list (point)
                          (propertize "WARNING: :ELOT-context-localname: should match :ID:" 'face 'warning))
                    issues))
            (when (null (assoc prefix org-link-abbrev-alist-local))
              (push (list (point)
                          (propertize "WARNING: :ELOT-default-prefix: is not defined in org-link-abbrev-alist-local"
                                      'face 'warning))
                    issues))
            ;; Run helper check on :header-args:omn
            (setq issues (elot--check-omn-args omn-args (point) issues)))))
      tree)
    issues))

(org-lint-add-checker
 'elot/nodeclare-id-prefix-label
 "ELOT: heading must declare ID with known prefix and valid label, or carry :nodeclare:"
 #'elot-check-nodeclare-id-prefix-label
 :categories '(default elot)
 :trust 'high)

(org-lint-add-checker
 'elot/ontology-header
 "ELOT: top-level ontology heading must have required properties"
 #'elot-check-ontology-header
 :categories '(default elot)
 :trust 'high)

(defun elot-check-prefix-table (tree)
  "ELOT rule: ensure `elot-update-link-abbrev` sets useful abbrevs."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (and (= (org-element-property :level hl) 1)
                   (string= (elot-context-type) "ontology"))
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (let ((result (elot-update-link-abbrev)))
              (when (or (null org-link-abbrev-alist-local)
                        (equal org-link-abbrev-alist-local '(("prefix" . "uri"))))
                (push (list (point)
                            (propertize "ERROR: prefix-table is missing or malformed"
                                        'face 'error))
                      issues))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/prefix-table
 "ELOT: ontology section must contain #+name: prefix-table that updates abbrevs"
 #'elot-check-prefix-table
 :categories '(default elot)
 :trust 'high)

(defun elot-check-ontology-presence (tree)
  "ELOT rule: ensure there is at least one top-level ontology section."
  (let ((found nil)
        issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) 1)
          (goto-char (org-element-property :begin hl))
          (when (string= (org-entry-get nil "ELOT-context-type") "ontology")
            (setq found t))))
      tree)
    (unless found
      (push (list 1  ;; top of buffer
                  (propertize "ERROR: No top-level heading with :ELOT-context-type: ontology"
                              'face 'error))
            issues))
    issues))


(org-lint-add-checker
 'elot/ontology-presence
 "ELOT: file must contain at least one ontology top-level heading"
 #'elot-check-ontology-presence
 :categories '(default elot)
 :trust 'high)


(defun elot-check-required-sections (tree)
  "ELOT rule: check all required section headers for ontology."
  (let (localname issues)
    ;; Find top-level ontology headline to get localname
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (= (org-element-property :level hl) 1)
                   (string= (org-entry-get nil "ELOT-context-type") "ontology"))
          (setq localname (org-entry-get nil "ELOT-context-localname"))))
      nil t) ;; stop after first match

    (when localname
      (let* ((suffixes '("-ontology-declaration"
                         ;; "-datatypes"
                         "-class-hierarchy"
                         "-object-property-hierarchy"
                         "-data-property-hierarchy"
                         "-annotation-property-hierarchy"
                         "-individuals"))
             (required-ids (mapcar (lambda (suffix) (concat localname suffix)) suffixes))
             (headline-alist '()))
        ;; Build alist with ID as (custom_id resourcedefs begin)
        (org-element-map tree 'headline
          (lambda (hl)
            (goto-char (org-element-property :begin hl))
            (let ((id (org-entry-get nil "ID"))
                  (custom-id (org-entry-get nil "custom_id"))
                  (resourcedefs (org-entry-get nil "resourcedefs"))
                  (pos (point)))
              (when id
                (push (cons id (list custom-id resourcedefs pos))
                      headline-alist)))))

        ;; Check required sections
        (dolist (req-id required-ids)
          (let ((entry (assoc req-id headline-alist)))
            (if (null entry)
                (push (list (point-min)
                            (propertize (format "WARNING: Missing section with ID %s" req-id)
                                        'face 'warning))
                      issues)
              (let* ((custom-id (nth 0 (cdr entry)))
                     (resourcedefs (nth 1 (cdr entry)))
                     (pos (nth 2 (cdr entry))))
                (when (or (null custom-id) (not (string= custom-id req-id)))
                  (push (list pos
                              (propertize (format "ERROR: Section %s has missing or incorrect custom_id" req-id)
                                          'face 'error))
                        issues))
                (when (not (string= resourcedefs "yes"))
                  (push (list pos
                              (propertize (format "WARNING: Section %s should have :resourcedefs: yes" req-id)
                                          'face 'warning))
                        issues))))))))
    issues))


(org-lint-add-checker
 'elot/required-sections
 "ELOT: ontology must have required resource sections with proper properties"
 #'elot-check-required-sections
 :categories '(default elot)
 :trust 'high)


(defconst elot-known-annotation-properties
  '("rdfs:label" "rdfs:comment" "rdfs:seeAlso" "rdfs:isDefinedBy")
  "List of annotation properties allowed in description lists without declaration.")

(defun elot-check-description-list-curies (tree)
  "Check that CURIE terms in description lists exist as annotation properties in `elot-slurp` or are allowed exceptions."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (let* ((tag (org-element-property :tag item))
                   (term (org-element-interpret-data tag)))
              (when (and (stringp term)
                         (string-match "\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'" term))
                (unless (or
                         ;; allowed exceptions
                         (member term elot-known-annotation-properties)
                         ;; declared in elot-slurp as AnnotationProperty
                         (cl-find term elot-slurp
                                  :key #'car
                                  :test #'string=
                                  :if (lambda (x)
                                        (let ((plist (nth 2 x)))
                                          (and (plist-get plist "rdf:type")
                                               (string= (plist-get plist "rdf:type")
                                                        "owl:AnnotationProperty"))))))
                  (push (list (org-element-property :begin item)
                              (propertize (format "WARNING: Unknown or invalid annotation property: %s" term)
                                          'face 'warning))
                        issues)))))))
      tree)
    issues))



(org-lint-add-checker
 'elot/description-list-curies
 "ELOT: check that CURIE terms in description lists are declared"
 #'elot-check-description-list-curies
 :categories '(default elot)
 :trust 'high)


(defun elot-string-balanced-parentheses-p (str)
  "Return t if STR has balanced parentheses, nil otherwise."
  (let ((count 0)
        (balanced t)
        (i 0))
    (while (and balanced (< i (length str)))
      (let ((ch (aref str i)))
        (cond
         ((= ch ?\() (setq count (1+ count)))
         ((= ch ?\)) (setq count (1- count))
          (when (< count 0)
            (setq balanced nil)))))
      (setq i (1+ i)))
    (and balanced (= count 0))))

(defun elot-check-axiom-value-curies (tree)
  "Check that CURIEs in the value of axioms (Manchester syntax) are declared and not annotation properties, and that parentheses are balanced."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent))
               (hierarchy-id
                (save-excursion
                  (goto-char (org-element-property :begin item))
                  (elot-governing-hierarchy)))
               (in-annotation-section
                (and hierarchy-id
                     (string-match-p "annotation-property-hierarchy$" hierarchy-id))))
          (when (eq type 'descriptive)
            (let* ((tag (org-element-property :tag item))
                   (term (org-element-interpret-data tag))
                   (contents (org-element-interpret-data (org-element-contents item))))
              ;; Only apply check if term is a Manchester keyword
              (when (member term elot-omn-all-keywords)
                ;; Check CURIEs
                (let ((curies (seq-filter (lambda (word)
                                            (and (string-match "\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'" word)
                                                 (not (string-match "\\`https?://" word))))
                                          (split-string contents "[ \n\t]+" t))))
                  (dolist (curie curies)
                    (let ((entry (cl-find curie elot-slurp :key #'car :test #'string=)))
                      (cond
                       ((or (member curie elot-owl-builtin-resources)
                            entry)
                        ;; Built-in or known: only warn if it’s an annotation property
                        (when (and entry
                                   (not in-annotation-section)
                                   (string= (plist-get (nth 2 entry) "rdf:type" #'equal)
                                            "owl:AnnotationProperty"))
                          (push (list (org-element-property :begin item)
                                      (propertize (format "WARNING: Annotation property used in axiom: %s" curie)
                                                  'face 'warning))
                                issues)))
                       (t
                        (push (list (org-element-property :begin item)
                                    (propertize (format "WARNING: Unknown CURIE in axiom: %s" curie)
                                                'face 'warning))
                              issues))))))
                ;; Check balanced parentheses
                (unless (elot-string-balanced-parentheses-p contents)
                  (push (list (org-element-property :begin item)
                              (propertize "WARNING: Unbalanced parentheses in axiom value"
                                          'face 'warning))
                        issues)))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/axiom-value-curies
 "ELOT: Check CURIEs in axiom description values are defined and not annotation properties"
 #'elot-check-axiom-value-curies
 :categories '(default elot)
 :trust 'high)



(provide 'elot-lint)
;;; elot-lint.el ends here
