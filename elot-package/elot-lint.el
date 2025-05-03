;;; elot-lint.el  -*- lexical-binding: t; -*-
(require 'org-element)
(require 'org-lint)
(require 'ox)                         ; for org-entry-get-with-inheritance

;; ---------------------------------------------------------------------
;;  Helpers
;; ---------------------------------------------------------------------

(defun elot--resourcedefs-here-p ()
  "Return t if headline at point sets :resourcedefs: yes."
  (string-equal (org-entry-get nil "resourcedefs") "yes"))

(defun elot--inside-resourcedefs-p ()
  "Return t when point is inside any :resourcedefs: yes section."
  (string-equal (org-entry-get-with-inheritance "resourcedefs") "yes"))

(declare-function elot-entity-from-header "elot")
(declare-function elot-unprefix-uri "elot")

;; ---------------------------------------------------------------------
;;  Combined checker with prefix validation
;; ---------------------------------------------------------------------

(defun elot-check-nodeclare-id-prefix (tree)
  "ELOT rule: every headline under :resourcedefs: yes must either
have a valid identifier or be tagged :nodeclare:, and must use a known prefix."
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
              (let ((entity (condition-case nil
                               (elot-entity-from-header title 'noerror)
                             (error nil))))
                (cond
                 ((not entity)
                  (push (list (point)
                              (propertize "ERROR: No identifier found – add CURIE/URI or tag :nodeclare:"
                                          'face 'error))
                        issues))
                 ((null (elot-unprefix-uri entity org-link-abbrev-alist-local 'noerror))
                  (push (list (point)
                              (propertize "ERROR: Unknown prefix in identifier"
                                          'face 'error))
                        issues)))))))
        nil)
      tree)
    issues))

(org-lint-add-checker
 'elot/nodeclare-id-prefix
 "ELOT: resource heading must declare ID with known prefix or carry :nodeclare:"
 #'elot-check-nodeclare-id-prefix
 :categories '(default elot)
 :trust 'high)

;; ---------------------------------------------------------------------
(provide 'elot-lint)
