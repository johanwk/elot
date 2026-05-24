;;; elot-gptel-db-shims-test.el --- Tests for M6.4 Wave A shims  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-shims-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-shims-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.4 (Wave A): tests for the
;; three convenience shims layered over `elot-db':
;;   `elot-gptel-tool-db-get-label'
;;   `elot-gptel-tool-db-list-sources'
;;   `elot-gptel-tool-db-expand-curie'
;;
;; Pure-Elisp; no ROBOT required.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-gptel)

;;;; Harness

(defvar elot-gptel-db-shims-test--tmpfile nil)

(defun elot-gptel-db-shims-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-shims-test--tmpfile
             (file-exists-p elot-gptel-db-shims-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-shims-test--tmpfile)))
  (setq elot-gptel-db-shims-test--tmpfile
        (make-temp-file "elot-gptel-db-shims-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-shims-test--tmpfile))
  (elot-db-init elot-gptel-db-shims-test--tmpfile))

(defun elot-gptel-db-shims-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-shims-test--tmpfile
             (file-exists-p elot-gptel-db-shims-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-shims-test--tmpfile)))
  (setq elot-gptel-db-shims-test--tmpfile nil))

(defmacro elot-gptel-db-shims-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-shims-test--fresh-db) ,@body)
     (elot-gptel-db-shims-test--teardown)))

(defun elot-gptel-db-shims-test--seed-pets ()
  "Populate the fresh DB with a tiny pets-like fixture."
  (elot-db-update-source
   "pets" nil "org"
   '(("ex:dog"    "Dog"    ("rdf:type" "owl:Class"))
     ("ex:cat"    "Cat"    ("rdf:type" "owl:Class"))
     ("ex:animal" "Animal" ("rdf:type" "owl:Class"))))
  (elot-db-add-prefix "pets" nil "ex" "http://example.org/"))

;;;; Tests -- elot_db_get_label

(ert-deftest test-elot-gptel-tool-db-get-label-curie ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((elot-active-label-sources '(("pets" nil))))
      (should (equal "Dog"
                     (elot-gptel-tool-db-get-label "ex:dog"))))))

(ert-deftest test-elot-gptel-tool-db-get-label-bare-iri ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((elot-active-label-sources '(("pets" nil))))
      (should (equal "Dog"
                     (elot-gptel-tool-db-get-label
                      "http://example.org/dog"))))))

(ert-deftest test-elot-gptel-tool-db-get-label-bracketed-iri ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((elot-active-label-sources '(("pets" nil))))
      (should (equal "Dog"
                     (elot-gptel-tool-db-get-label
                      "<http://example.org/dog>"))))))

(ert-deftest test-elot-gptel-tool-db-get-label-unknown ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((elot-active-label-sources '(("pets" nil))))
      (should (equal "(no label)"
                     (elot-gptel-tool-db-get-label "ex:nope"))))))

(ert-deftest test-elot-gptel-tool-db-get-label-rejects-empty ()
  (elot-gptel-db-shims-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-get-label "")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

;;;; Tests -- elot_db_list_sources

(ert-deftest test-elot-gptel-tool-db-list-sources-empty ()
  (elot-gptel-db-shims-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-list-sources)))
      (should (stringp out))
      (should (string-match-p "no sources registered" out)))))

(ert-deftest test-elot-gptel-tool-db-list-sources-tsv ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((out (elot-gptel-tool-db-list-sources)))
      (should (stringp out))
      ;; Header line is present.
      (should (string-match-p
               "^source\tdata_source\ttype\tlast_modified\tlast_updated"
               out))
      ;; The seeded source name appears on a data row.
      (should (string-match-p "^pets\t" out)))))

;;;; Tests -- elot_db_expand_curie

(ert-deftest test-elot-gptel-tool-db-expand-curie-ok ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let ((elot-active-label-sources '(("pets" nil))))
      (should (equal "http://example.org/dog"
                     (elot-gptel-tool-db-expand-curie "ex:dog"))))))

(ert-deftest test-elot-gptel-tool-db-expand-curie-unknown-prefix ()
  (elot-gptel-db-shims-test--with-fresh-db
    (elot-gptel-db-shims-test--seed-pets)
    (let* ((elot-active-label-sources '(("pets" nil)))
           (out (elot-gptel-tool-db-expand-curie "zz:foo")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest test-elot-gptel-tool-db-expand-curie-rejects-empty ()
  (elot-gptel-db-shims-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-expand-curie "")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(provide 'elot-gptel-db-shims-test)

;;; elot-gptel-db-shims-test.el ends here
