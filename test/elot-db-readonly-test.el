;;; elot-db-readonly-test.el --- Tests for the M6.1 read-only SQL gate  -*- lexical-binding: t; -*-

;; Usage:  make -C test readonly-test  (or)
;;         cd test && emacs --batch -l elot-db-readonly-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.1: tests for
;; `elot-db-execute-readonly' and its helpers
;; `elot-db--sql-strip-comments' / `elot-db--sql-first-token'.
;;
;; The gate is the single SQL entry point used by the upcoming
;; LLM-facing tools (`elot_db_query', `elot_db_search_label',
;; `elot_db_borrow_term').  It refuses any statement whose first
;; non-comment token is not SELECT or WITH, /and/ pins
;; `PRAGMA query_only = 1' for the duration of the call as a
;; belt-and-braces safety net at the SQLite level.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

;;;; Harness

(defvar elot-db-ro-test--tmpfile nil)

(defun elot-db-ro-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-ro-test--tmpfile
             (file-exists-p elot-db-ro-test--tmpfile))
    (ignore-errors (delete-file elot-db-ro-test--tmpfile)))
  (setq elot-db-ro-test--tmpfile
        (make-temp-file "elot-db-ro-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-ro-test--tmpfile))
  (elot-db-init elot-db-ro-test--tmpfile)
  ;; Seed one source + two entities so SELECTs have something to return.
  (elot-db-update-source
   "s1" nil "org"
   '(("e1" "Entity One"
      ("rdf:type" "owl:Class" "skos:definition" "the one"))
     ("e2" "Entity Two"
      ("rdf:type" "owl:Class")))
   123.0))

(defun elot-db-ro-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-ro-test--tmpfile
             (file-exists-p elot-db-ro-test--tmpfile))
    (ignore-errors (delete-file elot-db-ro-test--tmpfile)))
  (setq elot-db-ro-test--tmpfile nil))

(defmacro elot-db-ro-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-db-ro-test--fresh-db) ,@body)
     (elot-db-ro-test--teardown)))

;;;; Pure helpers: comment stripping and first-token detection

(ert-deftest test-elot-db-sql-strip-comments-line ()
  (should (equal (elot-db--sql-strip-comments
                  "-- leading comment\nSELECT 1")
                 "\nSELECT 1"))
  (should (equal (elot-db--sql-strip-comments
                  "SELECT 1 -- trailing\nFROM t")
                 "SELECT 1 \nFROM t")))

(ert-deftest test-elot-db-sql-strip-comments-block ()
  (should (equal (elot-db--sql-strip-comments
                  "/* hidden DELETE */ SELECT 1")
                 " SELECT 1"))
  (should (equal (elot-db--sql-strip-comments
                  "SELECT /* between\nlines */ 1")
                 "SELECT  1")))

(ert-deftest test-elot-db-sql-strip-comments-preserves-strings ()
  "Comment markers inside single-quoted literals are not comments."
  (let* ((sql "SELECT '-- not a comment', '/* also not */' FROM t")
         (stripped (elot-db--sql-strip-comments sql)))
    (should (equal stripped sql))))

(ert-deftest test-elot-db-sql-strip-comments-doubled-quote ()
  "Doubled '' inside a literal does not end the literal."
  (let* ((sql "SELECT 'it''s -- fine' FROM t")
         (stripped (elot-db--sql-strip-comments sql)))
    (should (equal stripped sql))))

(ert-deftest test-elot-db-sql-first-token ()
  (should (equal "SELECT" (elot-db--sql-first-token "SELECT 1")))
  (should (equal "SELECT" (elot-db--sql-first-token "  select 1 ")))
  (should (equal "WITH"   (elot-db--sql-first-token
                           "-- foo\n  WITH x AS (SELECT 1) SELECT * FROM x")))
  (should (equal "DELETE" (elot-db--sql-first-token "DELETE FROM t")))
  (should (equal "SELECT"
                 (elot-db--sql-first-token
                  "/* DELETE FROM t */ SELECT 1")))
  (should (null (elot-db--sql-first-token "")))
  (should (null (elot-db--sql-first-token "   "))))

;;;; Read-only gate: refusals

(ert-deftest test-elot-db-execute-readonly-empty ()
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly ""))
    (should-error (elot-db-execute-readonly "   \n\t"))))

(ert-deftest test-elot-db-execute-readonly-refuses-insert ()
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly
                   "INSERT INTO sources (source) VALUES ('x')"))))

(ert-deftest test-elot-db-execute-readonly-refuses-delete ()
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly
                   "DELETE FROM entities"))
    ;; The DELETE must not have run.
    (should (= 2 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities"))))))

(ert-deftest test-elot-db-execute-readonly-refuses-update ()
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly
                   "UPDATE entities SET label = 'x'"))))

(ert-deftest test-elot-db-execute-readonly-refuses-pragma ()
  "PRAGMA is not SELECT/WITH and must be refused."
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly "PRAGMA user_version = 7"))))

(ert-deftest test-elot-db-execute-readonly-refuses-drop ()
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly "DROP TABLE entities"))
    (should (= 2 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities"))))))

(ert-deftest test-elot-db-execute-readonly-refuses-mutating-in-comment ()
  "A leading comment that hides a SELECT-ish keyword is still SELECT/WITH;
but a leading DELETE that hides a SELECT comment must be refused."
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly
                   "-- SELECT 1\nDELETE FROM entities"))
    (should (= 2 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM entities"))))))

;;;; Read-only gate: acceptance

(ert-deftest test-elot-db-execute-readonly-accepts-select ()
  (elot-db-ro-test--with-fresh-db
    (let ((rows (elot-db-execute-readonly
                 "SELECT id, label FROM entities ORDER BY id")))
      (should (equal rows '(("e1" "Entity One")
                            ("e2" "Entity Two")))))))

(ert-deftest test-elot-db-execute-readonly-accepts-select-lowercase ()
  (elot-db-ro-test--with-fresh-db
    (let ((rows (elot-db-execute-readonly
                 "  select count(*) from entities")))
      (should (equal rows '((2)))))))

(ert-deftest test-elot-db-execute-readonly-accepts-with ()
  (elot-db-ro-test--with-fresh-db
    (let ((rows (elot-db-execute-readonly
                 "WITH x AS (SELECT id FROM entities)
                  SELECT COUNT(*) FROM x")))
      (should (equal rows '((2)))))))

(ert-deftest test-elot-db-execute-readonly-accepts-select-after-comment ()
  (elot-db-ro-test--with-fresh-db
    (let ((rows (elot-db-execute-readonly
                 "-- leading comment\n/* block */ SELECT 1")))
      (should (equal rows '((1)))))))

(ert-deftest test-elot-db-execute-readonly-accepts-bind-params ()
  (elot-db-ro-test--with-fresh-db
    (let ((rows (elot-db-execute-readonly
                 "SELECT label FROM entities WHERE id = ?"
                 (list "e2"))))
      (should (equal rows '(("Entity Two")))))))

;;;; PRAGMA query_only restoration

(ert-deftest test-elot-db-execute-readonly-restores-pragma ()
  "After a successful read-only call, the connection accepts writes again."
  (elot-db-ro-test--with-fresh-db
    (elot-db-execute-readonly "SELECT 1")
    ;; A direct write through the unguarded API must still work.
    (should (sqlite-execute
             elot-db
             "UPDATE entities SET label = 'updated' WHERE id = 'e1'"))
    (should (equal '(("updated"))
                   (sqlite-select
                    elot-db
                    "SELECT label FROM entities WHERE id = 'e1'")))))

(ert-deftest test-elot-db-execute-readonly-restores-pragma-on-error ()
  "Even when the underlying SELECT signals (e.g. SQL syntax error), the
PRAGMA must be restored to query_only = 0."
  (elot-db-ro-test--with-fresh-db
    (should-error (elot-db-execute-readonly
                   "SELECT * FROM no_such_table"))
    (should (sqlite-execute
             elot-db
             "UPDATE entities SET label = 'x' WHERE id = 'e1'"))))

(ert-deftest test-elot-db-execute-readonly-no-connection ()
  "Without an open connection, the gate signals user-error."
  (ignore-errors (elot-db-close))
  (let ((elot-db nil))
    (should-error (elot-db-execute-readonly "SELECT 1"))))

(provide 'elot-db-readonly-test)

;;; elot-db-readonly-test.el ends here
