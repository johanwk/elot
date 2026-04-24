;;; elot-db-schema-v3-test.el --- Step 1.16.2 schema migration tests -*- lexical-binding: t; -*-

;; Usage:  make -C test step-1-16-2-test

;;; Commentary:

;; Step 1.16.2 tests: the additive v2 -> v3 migration that introduces
;; the `lang' column on the `attributes' table and the
;; `idx_attrs_prop_lang' index.  No reader or writer consumes the
;; column yet -- these tests prove additivity (fresh DBs have the
;; column; a hand-built v2 DB migrates cleanly; pre-existing rows
;; receive lang='').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sqlite)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

;;;; Harness

(defvar elot-v3-test--tmpfile nil)

(defun elot-v3-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-v3-test--tmpfile
             (file-exists-p elot-v3-test--tmpfile))
    (ignore-errors (delete-file elot-v3-test--tmpfile)))
  (setq elot-v3-test--tmpfile nil))

(defun elot-v3-test--fresh-path ()
  (elot-v3-test--teardown)
  (setq elot-v3-test--tmpfile
        (make-temp-file "elot-v3-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-v3-test--tmpfile))
  elot-v3-test--tmpfile)

(defun elot-v3-test--cols (db table)
  "Column names of TABLE in DB."
  (mapcar (lambda (r) (nth 1 r))
          (sqlite-select db (format "PRAGMA table_info(%s)" table))))

(defun elot-v3-test--indexes (db)
  (mapcar #'car
          (sqlite-select
           db
           "SELECT name FROM sqlite_master WHERE type='index' \
AND name NOT LIKE 'sqlite_%' ORDER BY name")))

(defun elot-v3-test--build-v2-db (path)
  "Create a v2-shaped DB at PATH: attributes lacks `lang', stored
schema_version is 2, and the v3-only index is absent.  Used to
exercise the v2 -> v3 migration path."
  (let ((db (sqlite-open path)))
    (unwind-protect
        (progn
          (sqlite-pragma db "foreign_keys = ON")
          (dolist (stmt
                   '("CREATE TABLE schema_version (version INTEGER PRIMARY KEY)"
                     "CREATE TABLE sources (
                        source        TEXT NOT NULL,
                        data_source   TEXT NOT NULL DEFAULT '',
                        type          TEXT,
                        last_modified REAL,
                        last_updated  REAL,
                        PRIMARY KEY (source, data_source))"
                     "CREATE TABLE entities (
                        id          TEXT,
                        label       TEXT,
                        source      TEXT NOT NULL,
                        data_source TEXT NOT NULL DEFAULT '',
                        kind        TEXT NOT NULL DEFAULT 'unknown',
                        PRIMARY KEY (id, source, data_source),
                        FOREIGN KEY (source, data_source)
                          REFERENCES sources(source, data_source) ON DELETE CASCADE)"
                     "CREATE TABLE attributes (
                        id          TEXT,
                        source      TEXT NOT NULL,
                        data_source TEXT NOT NULL DEFAULT '',
                        prop        TEXT,
                        value       TEXT,
                        FOREIGN KEY (id, source, data_source)
                          REFERENCES entities(id, source, data_source) ON DELETE CASCADE)"
                     "CREATE TABLE prefixes (
                        source        TEXT NOT NULL,
                        data_source   TEXT NOT NULL DEFAULT '',
                        prefix        TEXT NOT NULL,
                        expansion     TEXT NOT NULL,
                        PRIMARY KEY (source, data_source, prefix),
                        FOREIGN KEY (source, data_source)
                          REFERENCES sources(source, data_source) ON DELETE CASCADE)"
                     "CREATE TABLE global_prefixes (
                        prefix      TEXT PRIMARY KEY,
                        expansion   TEXT NOT NULL,
                        trust       INTEGER NOT NULL DEFAULT 1)"
                     "CREATE INDEX idx_attrs_id_source ON attributes(id, source, data_source)"
                     "CREATE INDEX idx_entities_id ON entities(id)"
                     "CREATE INDEX idx_prefixes_expansion ON prefixes(expansion)"
                     "INSERT INTO schema_version (version) VALUES (2)"
                     "INSERT INTO sources (source, data_source, type, last_modified, last_updated) \
VALUES ('s1', '', 'csv', 0.0, 0.0)"
                     "INSERT INTO entities (id, label, source, data_source, kind) \
VALUES ('e1', 'Entity One', 's1', '', 'Class')"
                     "INSERT INTO attributes (id, source, data_source, prop, value) \
VALUES ('e1', 's1', '', 'rdf:type', 'owl:Class')"))
            (sqlite-execute db stmt)))
      (sqlite-close db))))

;;;; Tests

(ert-deftest test-fresh-db-has-lang-column ()
  "A freshly-initialised DB has the `lang' column on `attributes'
and the `idx_attrs_prop_lang' index, and reports schema_version = 3."
  (unwind-protect
      (let ((path (elot-v3-test--fresh-path)))
        (elot-db-init path)
        (should (member "lang" (elot-v3-test--cols elot-db "attributes")))
        (should (member "idx_attrs_prop_lang" (elot-v3-test--indexes elot-db)))
        (should (equal 3 elot-db-schema-version))
        (should (equal (list (list 3))
                       (sqlite-select
                        elot-db "SELECT version FROM schema_version"))))
    (elot-v3-test--teardown)))

(ert-deftest test-migrate-v2-to-v3-adds-lang-column ()
  "Opening a hand-built v2 DB migrates to v3: adds `lang' column
(existing rows carry lang = ''), creates `idx_attrs_prop_lang', and
bumps schema_version to 3."
  (unwind-protect
      (let ((path (elot-v3-test--fresh-path)))
        ;; Build a v2 DB, close it.
        (elot-v3-test--build-v2-db path)
        ;; Sanity: the DB is genuinely v2.
        (let ((db (sqlite-open path)))
          (unwind-protect
              (progn
                (should-not (member "lang" (elot-v3-test--cols db "attributes")))
                (should (equal (list (list 2))
                               (sqlite-select
                                db "SELECT version FROM schema_version"))))
            (sqlite-close db)))
        ;; Open via elot-db-init -> triggers migration.
        (elot-db-init path)
        (should (member "lang" (elot-v3-test--cols elot-db "attributes")))
        (should (member "idx_attrs_prop_lang" (elot-v3-test--indexes elot-db)))
        (should (equal (list (list 3))
                       (sqlite-select
                        elot-db "SELECT version FROM schema_version")))
        ;; Pre-existing attribute row carries lang = ''.
        (should (equal (list (list "e1" "rdf:type" "owl:Class" ""))
                       (sqlite-select
                        elot-db
                        "SELECT id, prop, value, lang FROM attributes"))))
    (elot-v3-test--teardown)))

(ert-deftest test-migrate-v2-to-v3-idempotent ()
  "Re-opening a DB that has already been migrated to v3 is a no-op."
  (unwind-protect
      (let ((path (elot-v3-test--fresh-path)))
        (elot-v3-test--build-v2-db path)
        (elot-db-init path)       ; migrates
        (elot-db-close)
        (elot-db-init path)       ; must not error
        (should (equal (list (list 3))
                       (sqlite-select
                        elot-db "SELECT version FROM schema_version")))
        (should (member "lang" (elot-v3-test--cols elot-db "attributes"))))
    (elot-v3-test--teardown)))

(provide 'elot-db-schema-v3-test)

;;; elot-db-schema-v3-test.el ends here
