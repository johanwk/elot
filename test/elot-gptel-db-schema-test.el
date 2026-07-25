;;; elot-gptel-db-schema-test.el --- Tests for the M6.3 elot_db_schema tool  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-schema-tool-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-schema-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.3: tests for
;; `elot-gptel-tool-db-schema'.  Pure-Elisp; no ROBOT needed.
;;
;; Verifies the tool returns the canonical schema.sql DDL fenced
;; as a ```sql``` block, alongside the code-side
;; `elot-db-schema-version' constant and the stored
;; `schema_version' row pulled through the read-only gate.

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

(defvar elot-gptel-db-schema-test--tmpfile nil)

(defun elot-gptel-db-schema-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-schema-test--tmpfile
             (file-exists-p elot-gptel-db-schema-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-schema-test--tmpfile)))
  (setq elot-gptel-db-schema-test--tmpfile
        (make-temp-file "elot-gptel-db-schema-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-schema-test--tmpfile))
  (elot-db-init elot-gptel-db-schema-test--tmpfile))

(defun elot-gptel-db-schema-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-schema-test--tmpfile
             (file-exists-p elot-gptel-db-schema-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-schema-test--tmpfile)))
  (setq elot-gptel-db-schema-test--tmpfile nil))

(defmacro elot-gptel-db-schema-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-schema-test--fresh-db) ,@body)
     (elot-gptel-db-schema-test--teardown)))

;;;; Tests

(ert-deftest test-elot-gptel-tool-db-schema-fenced-sql ()
  "Output contains a fenced ```sql``` code block carrying the DDL."
  (elot-gptel-db-schema-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-schema)))
      (should (stringp out))
      (should (string-match-p "```sql" out))
      (should (string-match-p "```\\'" out))
      ;; Some core schema markers from schema.sql:
      (should (string-match-p "CREATE TABLE IF NOT EXISTS schema_version"
                              out))
      (should (string-match-p "CREATE TABLE IF NOT EXISTS sources" out))
      (should (string-match-p "CREATE TABLE IF NOT EXISTS entities" out))
      (should (string-match-p "CREATE TABLE IF NOT EXISTS attributes" out))
      (should (string-match-p "CREATE TABLE IF NOT EXISTS prefixes" out)))))

(ert-deftest test-elot-gptel-tool-db-schema-reports-versions ()
  "Output reports both code-side and stored schema versions."
  (elot-gptel-db-schema-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-schema)))
      (should (string-match-p "schema_version (code):" out))
      (should (string-match-p "schema_version (stored):" out))
      (should (string-match-p
               (format "schema_version (code):[ \t]+%d"
                       elot-db-schema-version)
               out))
      (should (string-match-p
               (format "schema_version (stored):[ \t]+%d"
                       elot-db-schema-version)
               out)))))

(ert-deftest test-elot-gptel-tool-db-schema-matches-canonical ()
  "Output's fenced block matches the canonical schema.sql contents."
  (elot-gptel-db-schema-test--with-fresh-db
    (let* ((out (elot-gptel-tool-db-schema))
           (ddl (elot-db--read-schema-sql)))
      (should (stringp ddl))
      ;; The DDL appears verbatim inside the fenced block.
      (should (string-match-p
               (regexp-quote (string-trim-right ddl))
               out)))))

(provide 'elot-gptel-db-schema-test)

;;; elot-gptel-db-schema-test.el ends here
