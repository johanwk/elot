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
                     (label (if (and title (string-match "\\`\\(.+?\\) (.*)" title))
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
 "ELOT: ontology section must contain #+name: prefix-table with valid abbrevs"
 #'elot-check-prefix-table
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

(provide 'elot-lint)
;;; elot-lint.el ends here
