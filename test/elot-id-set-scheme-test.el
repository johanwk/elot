;;; elot-id-set-scheme-test.el --- Tests for elot-id-set-scheme  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-id-set-scheme-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 10 Step 10.7 -- tests for the
;; per-ontology scheme picker `elot-id-set-scheme' and the
;; companion read-out `elot-id-show-scheme'.  Interactive prompts
;; (`completing-read', `read-string', `y-or-n-p') are stubbed via
;; `cl-letf'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst elot-id-set-scheme-test--repo-root
  (let ((this (or load-file-name buffer-file-name
                  (locate-library "elot-id-set-scheme-test"))))
    (unless this
      (error "elot-id-set-scheme-test: cannot determine source location"))
    (file-name-directory
     (directory-file-name
      (file-name-directory (file-truename this))))))

(add-to-list 'load-path
             (expand-file-name "elot-package" elot-id-set-scheme-test--repo-root))

(require 'elot-id)
(require 'elot-id-insert)


;;; ---------------------------------------------------------------------------
;;; Fixtures
;;; ---------------------------------------------------------------------------

(defun elot-id-set-scheme-test--fixture (&optional scheme)
  "Return an ELOT-shaped Org buffer with one ontology.
When SCHEME is non-nil, declare it via :ELOT-id-scheme:."
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   (if scheme (format ":ELOT-id-scheme: %s\n" scheme) "")
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID: my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"))

(defun elot-id-set-scheme-test--multi-fixture ()
  "Return an Org buffer with TWO ontologies.
Pre-heading filler so point-min sits above both ontologies (otherwise
point would already be on the `* first-ont' heading line and the
multi-ontology picker would never run)."
  (concat
   "#+title: multi-fixture\n"
   "\n"
   "Pre-heading filler.\n"
   "\n"
   "* first-ont\n"
   ":PROPERTIES:\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-context-localname: first-ont\n"
   ":ELOT-default-prefix: a\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n:resourcedefs: yes\n:END:\n"
   "* second-ont\n"
   ":PROPERTIES:\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-context-localname: second-ont\n"
   ":ELOT-default-prefix: b\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n:resourcedefs: yes\n:END:\n"))

(defmacro elot-id-set-scheme-test--with (fixture point-re &rest body)
  "Insert FIXTURE in a temp Org buffer, move point to POINT-RE, run BODY."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,fixture)
     (org-mode)
     (goto-char (point-min))
     (when ,point-re
       (re-search-forward ,point-re nil t)
       (beginning-of-line))
     ,@body))

(defun elot-id-set-scheme-test--stub-completing (choice)
  "Return a lambda usable as `completing-read' stub returning CHOICE."
  (lambda (&rest _) choice))

(defun elot-id-set-scheme-test--stub-string (value)
  "Return a `read-string' stub that always returns VALUE."
  (lambda (&rest _) value))


;;; ---------------------------------------------------------------------------
;;; Bare-scheme writes (no params)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-set-scheme-test-write-uuid ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "uuid")))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "uuid"))))))

(ert-deftest elot-id-set-scheme-test-write-slug ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "slug")))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "slug"))))))


;;; ---------------------------------------------------------------------------
;;; Scheme-specific second prompts
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-set-scheme-test-counter-with-template ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "counter"))
              ((symbol-function 'read-string)
               (elot-id-set-scheme-test--stub-string "GO_0000000")))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme")
                         "counter GO_0000000"))))))

(ert-deftest elot-id-set-scheme-test-acme-with-slug-yes ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "acme"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme")
                         "acme slug:t"))))))

(ert-deftest elot-id-set-scheme-test-acme-with-slug-no ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "acme"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "acme"))))))


;;; ---------------------------------------------------------------------------
;;; Overwrite confirmation
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-set-scheme-test-overwrite-confirmed ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture "slug") "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "uuid"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "uuid"))))))

(ert-deftest elot-id-set-scheme-test-overwrite-declined ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture "slug") "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "uuid"))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (elot-id-set-scheme) :type 'user-error)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "slug"))))))

(ert-deftest elot-id-set-scheme-test-same-value-no-prompt ()
  "Choosing the same value does NOT trigger the overwrite prompt."
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture "slug") "^\\*\\*\\* Animal"
    (cl-letf (((symbol-function 'completing-read)
               (elot-id-set-scheme-test--stub-completing "slug"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (error "y-or-n-p should not be called"))))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* my-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "slug"))))))


;;; ---------------------------------------------------------------------------
;;; Multi-ontology + refusal
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-set-scheme-test-multi-ontology-pick ()
  "Outside any subtree, user picks the ontology by localname."
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--multi-fixture) nil
    (goto-char (point-min)) ;; on the buffer start, above all ontologies
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (cond
                  ((string-match-p "ontology" prompt) "second-ont")
                  ((string-match-p "Scheme" prompt) "uuid")
                  (t (car collection))))))
      (elot-id-set-scheme)
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^\\* second-ont")
        (should (string= (org-entry-get nil "ELOT-id-scheme") "uuid"))
        ;; first-ont untouched
        (goto-char (point-min))
        (re-search-forward "^\\* first-ont")
        (should (null (org-entry-get nil "ELOT-id-scheme")))))))

(ert-deftest elot-id-set-scheme-test-refusal-no-ontology ()
  (with-temp-buffer
    (insert "* just a heading\n")
    (org-mode)
    (goto-char (point-min))
    (should-error (elot-id-set-scheme) :type 'user-error)))


;;; ---------------------------------------------------------------------------
;;; Show
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-show-scheme-test-reports-spec ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture "counter GO_0000000")
      "^\\*\\*\\* Animal"
    (should (string= (elot-id-show-scheme) "counter GO_0000000"))))

(ert-deftest elot-id-show-scheme-test-unset ()
  (elot-id-set-scheme-test--with
      (elot-id-set-scheme-test--fixture) "^\\*\\*\\* Animal"
    (should (null (elot-id-show-scheme)))))


(provide 'elot-id-set-scheme-test)
;;; elot-id-set-scheme-test.el ends here
