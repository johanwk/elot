;;; elot-gptel-db-query-test.el --- Tests for the M6.2 elot_db_query tool  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-query-tool-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-query-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.2: tests for
;; `elot-gptel-tool-db-query' (and the helpers
;; `elot-gptel--db-cell-to-string',
;; `elot-gptel--db-format-rows').
;;
;; Pure tests for the formatter; integration tests against a
;; seeded SQLite cache for the end-to-end tool path.

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

(defvar elot-gptel-db-q-test--tmpfile nil)

(defun elot-gptel-db-q-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-q-test--tmpfile
             (file-exists-p elot-gptel-db-q-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-q-test--tmpfile)))
  (setq elot-gptel-db-q-test--tmpfile
        (make-temp-file "elot-gptel-db-q-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-q-test--tmpfile))
  (elot-db-init elot-gptel-db-q-test--tmpfile)
  (elot-db-update-source
   "s1" nil "org"
   '(("e1" "Entity One"
      ("rdf:type" "owl:Class" "skos:definition" "the one"))
     ("e2" "Entity Two"
      ("rdf:type" "owl:Class"))
     ("e3" "Entity Three"
      ("rdf:type" "owl:Class")))
   123.0))

(defun elot-gptel-db-q-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-q-test--tmpfile
             (file-exists-p elot-gptel-db-q-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-q-test--tmpfile)))
  (setq elot-gptel-db-q-test--tmpfile nil))

(defmacro elot-gptel-db-q-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-q-test--fresh-db) ,@body)
     (elot-gptel-db-q-test--teardown)))

;;;; Pure helpers

(ert-deftest test-elot-gptel-db-cell-to-string-nil ()
  (should (equal "" (elot-gptel--db-cell-to-string nil))))

(ert-deftest test-elot-gptel-db-cell-to-string-string ()
  (should (equal "abc" (elot-gptel--db-cell-to-string "abc"))))

(ert-deftest test-elot-gptel-db-cell-to-string-number ()
  (should (equal "42" (elot-gptel--db-cell-to-string 42))))

(ert-deftest test-elot-gptel-db-cell-to-string-escapes-tab ()
  (should (equal "a b" (elot-gptel--db-cell-to-string "a\tb"))))

(ert-deftest test-elot-gptel-db-cell-to-string-escapes-newline ()
  (should (equal "a / b" (elot-gptel--db-cell-to-string "a\nb"))))

(ert-deftest test-elot-gptel-db-format-rows-with-header ()
  (let ((out (elot-gptel--db-format-rows
              '("id" "label")
              '(("e1" "Entity One") ("e2" "Entity Two"))
              200)))
    (should (equal "id\tlabel\ne1\tEntity One\ne2\tEntity Two" out))))

(ert-deftest test-elot-gptel-db-format-rows-no-header ()
  (let ((out (elot-gptel--db-format-rows
              nil
              '(("e1" "Entity One"))
              200)))
    (should (equal "e1\tEntity One" out))))

(ert-deftest test-elot-gptel-db-format-rows-truncates ()
  (let ((out (elot-gptel--db-format-rows
              '("id")
              '(("e1") ("e2") ("e3") ("e4"))
              2)))
    (should (string-match-p "id\ne1\ne2\n\\.\\.\\. 2 more rows omitted"
                            out))))

(ert-deftest test-elot-gptel-db-format-rows-truncates-singular ()
  (let ((out (elot-gptel--db-format-rows
              '("id")
              '(("e1") ("e2") ("e3"))
              2)))
    (should (string-match-p "\\.\\.\\. 1 more row omitted" out))
    (should-not (string-match-p "1 more rows omitted" out))))

(ert-deftest test-elot-gptel-db-format-rows-nil-cell ()
  (let ((out (elot-gptel--db-format-rows
              '("id" "label")
              '(("e1" nil))
              200)))
    (should (equal "id\tlabel\ne1\t" out))))

;;;; Tool: refusal paths (gate)

(ert-deftest test-elot-gptel-tool-db-query-refuses-empty ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query "")))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest test-elot-gptel-tool-db-query-refuses-delete ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query "DELETE FROM entities")))
      (should (string-prefix-p "ERROR:" out)))
    ;; The DELETE must not have run.
    (should (= 3 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities"))))))

(ert-deftest test-elot-gptel-tool-db-query-refuses-pragma ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query "PRAGMA user_version = 7")))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest test-elot-gptel-tool-db-query-refuses-bad-limit ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query
                "SELECT id FROM entities" -1)))
      (should (string-prefix-p "ERROR:" out)))))

;;;; Tool: integration

(ert-deftest test-elot-gptel-tool-db-query-select ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query
                "SELECT id, label FROM entities ORDER BY id")))
      (should (string-match-p "^id\tlabel\n" out))
      (should (string-match-p "^e1\tEntity One$" out))
      (should (string-match-p "^e2\tEntity Two$" out))
      (should (string-match-p "^e3\tEntity Three$" out)))))

(ert-deftest test-elot-gptel-tool-db-query-with ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query
                "WITH x AS (SELECT id FROM entities)
                 SELECT COUNT(*) FROM x")))
      (should (string-match-p "3" out)))))

(ert-deftest test-elot-gptel-tool-db-query-truncates ()
  (elot-gptel-db-q-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-query
                "SELECT id FROM entities ORDER BY id" 2)))
      (should (string-match-p "\\.\\.\\. 1 more row omitted" out))
      (should (string-match-p "^e1$" out))
      (should (string-match-p "^e2$" out))
      (should-not (string-match-p "^e3$" out)))))

(ert-deftest test-elot-gptel-tool-db-query-restores-write-capability ()
  "After a successful query, direct sqlite-execute writes still work."
  (elot-gptel-db-q-test--with-fresh-db
    (elot-gptel-tool-db-query "SELECT id FROM entities")
    (should (sqlite-execute
             elot-db
             "UPDATE entities SET label = 'X' WHERE id = 'e1'"))))

(provide 'elot-gptel-db-query-test)

;;; elot-gptel-db-query-test.el ends here
