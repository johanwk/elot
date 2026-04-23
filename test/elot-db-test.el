;;; elot-db-test.el --- ERT tests for elot-db  -*- lexical-binding: t; -*-

;; Usage:  make -C test          (or)
;;         cd test && emacs --batch -l elot-db-test.el -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.1 tests for the SQLite DB layer.  Each test uses a fresh
;; temp-file DB created via `make-temp-file' and passed explicitly to
;; `elot-db-init', avoiding any dependency on or mutation of the real
;; user cache at `elot-db-file'.

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

(defvar elot-db-test--tmpfile nil)

(defun elot-db-test--fresh-db ()
  "Close any open DB, create a new temp file, and init on it."
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
    (ignore-errors (delete-file elot-db-test--tmpfile)))
  (setq elot-db-test--tmpfile
        (make-temp-file "elot-db-test-" nil ".sqlite"))
  ;; sqlite-open wants to create the file itself; remove the empty
  ;; temp file so the DB is born fresh.
  (ignore-errors (delete-file elot-db-test--tmpfile))
  (elot-db-init elot-db-test--tmpfile))

(defun elot-db-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
    ;; On Windows, a lingering SQLite handle would keep the file
    ;; locked; ignore-errors keeps teardown from masking the real
    ;; test failure.
    (ignore-errors (delete-file elot-db-test--tmpfile)))
  (setq elot-db-test--tmpfile nil))

(defmacro elot-db-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-db-test--fresh-db) ,@body)
     (elot-db-test--teardown)))

(defun elot-db-test--table-names (db)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name FROM sqlite_master WHERE type='table' \
ORDER BY name")))

(defun elot-db-test--index-names (db)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name FROM sqlite_master WHERE type='index' \
AND name NOT LIKE 'sqlite_%' ORDER BY name")))

;;;; Tests

(ert-deftest test-elot-db-init-creates-tables ()
  "After init, all schema tables and indexes exist."
  (elot-db-test--with-fresh-db
    (let ((tables  (elot-db-test--table-names elot-db))
          (indexes (elot-db-test--index-names elot-db)))
      (dolist (tbl '("schema_version" "sources" "entities" "attributes"))
        (should (member tbl tables)))
      (dolist (idx '("idx_attrs_id_source" "idx_entities_id"))
        (should (member idx indexes))))))

(ert-deftest test-elot-db-init-idempotent ()
  "Calling `elot-db-init' twice on the same file does not error and
leaves schema_version at exactly one row."
  (elot-db-test--with-fresh-db
    (let ((path elot-db-test--tmpfile))
      (elot-db-close)
      (should (elot-db-init path))
      (let ((rows (sqlite-select elot-db
                                 "SELECT version FROM schema_version")))
        (should (equal rows (list (list elot-db-schema-version))))))))

(ert-deftest test-elot-db-schema-version-seeded ()
  "A fresh init seeds schema_version with exactly one row equal to
`elot-db-schema-version'."
  (elot-db-test--with-fresh-db
    (let ((rows (sqlite-select elot-db
                               "SELECT version FROM schema_version")))
      (should (equal rows (list (list elot-db-schema-version)))))))

(ert-deftest test-elot-db-cascade-delete ()
  "Deleting a sources row cascades through entities and attributes."
  ;; NB: all three tables use a *composite* FK (source, data_source).
  ;; SQLite treats composite FKs as MATCH SIMPLE: if any key column is
  ;; NULL the child is considered to reference no parent, so
  ;; ON DELETE CASCADE would silently skip it.  The schema defends
  ;; against this by declaring `data_source' NOT NULL DEFAULT '';
  ;; callers with no sub-dataset should use the empty-string sentinel.
  ;; Here we use a real non-empty value for clarity.
  (elot-db-test--with-fresh-db
    ;; Seed one source, one entity, two attributes.
    (sqlite-execute elot-db
                    "INSERT INTO sources \
(source, data_source, type, last_modified, last_updated) \
VALUES (?, ?, 'org', 0.0, 0.0)"
                    (list "s1" "d1"))
    (sqlite-execute elot-db
                    "INSERT INTO entities (id, label, source, data_source) \
VALUES (?, ?, ?, ?)"
                    (list "e1" "Entity One" "s1" "d1"))
    (sqlite-execute elot-db
                    "INSERT INTO attributes \
(id, source, data_source, prop, value) VALUES (?, ?, ?, ?, ?)"
                    (list "e1" "s1" "d1" "rdf:type" "owl:Class"))
    (sqlite-execute elot-db
                    "INSERT INTO attributes \
(id, source, data_source, prop, value) VALUES (?, ?, ?, ?, ?)"
                    (list "e1" "s1" "d1" "skos:definition" "an entity"))
    ;; Sanity pre-check.
    (should (= 1 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM entities"))))
    (should (= 2 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM attributes"))))
    ;; Delete the parent source.
    (sqlite-execute elot-db
                    "DELETE FROM sources WHERE source = ?" (list "s1"))
    ;; Entities and attributes should have cascaded away.
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM entities"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM attributes"))))))

(ert-deftest test-elot-db-data-source-defaults-to-empty-string ()
  "Omitting `data_source' on insert yields '' (not NULL), and the
composite FK cascade works across rows that use the sentinel."
  (elot-db-test--with-fresh-db
    ;; Omit data_source entirely -- should default to ''.
    (sqlite-execute elot-db
                    "INSERT INTO sources \
(source, type, last_modified, last_updated) \
VALUES (?, 'org', 0.0, 0.0)"
                    (list "s1"))
    (sqlite-execute elot-db
                    "INSERT INTO entities (id, label, source) \
VALUES (?, ?, ?)"
                    (list "e1" "Entity One" "s1"))
    (sqlite-execute elot-db
                    "INSERT INTO attributes (id, source, prop, value) \
VALUES (?, ?, ?, ?)"
                    (list "e1" "s1" "rdf:type" "owl:Class"))
    ;; Every data_source column should be the empty string, never NULL.
    (dolist (tbl '("sources" "entities" "attributes"))
      (should (equal '((""))
                     (sqlite-select
                      elot-db
                      (format "SELECT DISTINCT data_source FROM %s" tbl)))))
    ;; And the cascade still fires across the sentinel.
    (sqlite-execute elot-db
                    "DELETE FROM sources WHERE source = ?" (list "s1"))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM entities"))))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM attributes"))))))

(ert-deftest test-elot-db-migrate-mismatch-errors ()
  "Opening a DB whose stored schema_version differs from the code
constant signals an error via `elot-db-migrate'."
  (elot-db-test--with-fresh-db
    (let ((path elot-db-test--tmpfile))
      ;; Tamper: set stored version to something impossible.
      (sqlite-execute elot-db "DELETE FROM schema_version")
      (sqlite-execute elot-db
                      "INSERT INTO schema_version (version) VALUES (?)"
                      (list (+ elot-db-schema-version 999)))
      (elot-db-close)
      (should-error (elot-db-init path) :type 'user-error))))

(provide 'elot-db-test)

;;; elot-db-test.el ends here
