;;; elot-db-prefixes-test.el --- ERT tests for elot-db schema v2 (Step 1.1.1)  -*- lexical-binding: t; -*-

;; Usage:  make -C test prefix-test  (or)
;;         cd test && emacs --batch -l elot-db-prefixes-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.1.1 tests: prefix tables, global-prefix seed, v1->v2
;; migration, and the two-pass `elot-db-get-label-any' lookup.

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
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
    (ignore-errors (delete-file elot-db-test--tmpfile)))
  (setq elot-db-test--tmpfile
        (make-temp-file "elot-db-prefixes-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-test--tmpfile))
  (elot-db-init elot-db-test--tmpfile))

(defun elot-db-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-test--tmpfile
             (file-exists-p elot-db-test--tmpfile))
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

(defun elot-db-test--seed-source (src &optional ds)
  "Helper: insert a sources row plus, when needed, an entity row."
  (sqlite-execute
   elot-db
   "INSERT INTO sources (source, data_source, type, last_modified, last_updated)
    VALUES (?, ?, 'csv', 0.0, 0.0)"
   (list src (or ds ""))))

(defun elot-db-test--seed-entity (id label src &optional ds kind)
  (elot-db-test--seed-source src ds)
  (sqlite-execute
   elot-db
   "INSERT OR IGNORE INTO entities (id, label, source, data_source, kind)
    VALUES (?, ?, ?, ?, ?)"
   (list id label src (or ds "") (or kind "unknown"))))

;;;; Subtask A: schema + migration

(ert-deftest test-elot-db-prefixes-table-exists ()
  "Fresh init creates the prefixes / global_prefixes tables and the
idx_prefixes_expansion index."
  (elot-db-test--with-fresh-db
    (let ((tables  (elot-db-test--table-names elot-db))
          (indexes (elot-db-test--index-names elot-db)))
      (should (member "prefixes" tables))
      (should (member "global_prefixes" tables))
      (should (member "idx_prefixes_expansion" indexes)))))

(ert-deftest test-elot-db-entities-has-kind-column ()
  "Fresh init adds the `kind' column to `entities' with default
'unknown' for rows inserted without it."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "s1")
    (sqlite-execute
     elot-db
     "INSERT INTO entities (id, label, source, data_source) VALUES (?, ?, ?, ?)"
     (list "e1" "Entity One" "s1" ""))
    (should (equal "unknown"
                   (caar (sqlite-select
                          elot-db
                          "SELECT kind FROM entities WHERE id = 'e1'"))))))

(ert-deftest test-elot-db-global-prefixes-seeded ()
  "Fresh init populates global_prefixes from
`elot-db-default-global-prefixes'."
  (elot-db-test--with-fresh-db
    (let ((count (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM global_prefixes"))))
      (should (= count (length elot-db-default-global-prefixes))))
    ;; Spot-check a couple of rows.
    (should (equal "http://www.w3.org/2000/01/rdf-schema#"
                   (caar (sqlite-select
                          elot-db
                          "SELECT expansion FROM global_prefixes \
WHERE prefix = 'rdfs'"))))
    (should (equal "http://purl.obolibrary.org/obo/"
                   (caar (sqlite-select
                          elot-db
                          "SELECT expansion FROM global_prefixes \
WHERE prefix = 'obo'"))))))

(ert-deftest test-elot-db-global-prefixes-seed-preserves-user-edits ()
  "Re-running init does not overwrite a user-modified
global_prefixes row (INSERT OR IGNORE, not OR REPLACE)."
  (elot-db-test--with-fresh-db
    (sqlite-execute elot-db
                    "UPDATE global_prefixes SET expansion = ? WHERE prefix = 'obo'"
                    (list "http://my.custom.obo/"))
    (let ((path elot-db-test--tmpfile))
      (elot-db-close)
      (elot-db-init path)
      (should (equal "http://my.custom.obo/"
                     (caar (sqlite-select
                            elot-db
                            "SELECT expansion FROM global_prefixes \
WHERE prefix = 'obo'")))))))

(ert-deftest test-elot-db-migrate-v1-to-v2 ()
  "Open a DB that looks like v1 (no kind column, no prefix tables,
schema_version=1) and verify `elot-db-migrate' upgrades it in place."
  (elot-db-test--with-fresh-db
    (let ((path elot-db-test--tmpfile))
      ;; Tear down cleanly so we can build a v1 shape from scratch.
      (elot-db-close)
      (ignore-errors (delete-file path))
      ;; Hand-craft a v1 database.
      (let ((db (sqlite-open path)))
        (sqlite-pragma db "foreign_keys = ON")
        (sqlite-execute db "CREATE TABLE schema_version (version INTEGER PRIMARY KEY)")
        (sqlite-execute db "CREATE TABLE sources (
          source TEXT NOT NULL,
          data_source TEXT NOT NULL DEFAULT '',
          type TEXT, last_modified REAL, last_updated REAL,
          PRIMARY KEY (source, data_source))")
        (sqlite-execute db "CREATE TABLE entities (
          id TEXT, label TEXT,
          source TEXT NOT NULL, data_source TEXT NOT NULL DEFAULT '',
          PRIMARY KEY (id, source, data_source),
          FOREIGN KEY (source, data_source)
            REFERENCES sources(source, data_source) ON DELETE CASCADE)")
        (sqlite-execute db "CREATE TABLE attributes (
          id TEXT, source TEXT NOT NULL, data_source TEXT NOT NULL DEFAULT '',
          prop TEXT, value TEXT,
          FOREIGN KEY (id, source, data_source)
            REFERENCES entities(id, source, data_source) ON DELETE CASCADE)")
        (sqlite-execute db "INSERT INTO schema_version (version) VALUES (1)")
        (sqlite-execute
         db
         "INSERT INTO sources (source, data_source, type, last_modified, last_updated)
          VALUES ('v1src', '', 'csv', 0.0, 0.0)")
        (sqlite-execute
         db
         "INSERT INTO entities (id, label, source, data_source)
          VALUES ('v1e', 'v1 entity', 'v1src', '')")
        (sqlite-close db))
      ;; Now open through the regular init; migration must fire.
      (elot-db-init path)
      ;; Version bumped.
      (should (equal elot-db-schema-version
                     (caar (sqlite-select
                            elot-db "SELECT version FROM schema_version"))))
      ;; New tables present.
      (let ((tables (elot-db-test--table-names elot-db)))
        (should (member "prefixes" tables))
        (should (member "global_prefixes" tables)))
      ;; kind column present, pre-existing row carries default.
      (should (equal "unknown"
                     (caar (sqlite-select
                            elot-db
                            "SELECT kind FROM entities WHERE id = 'v1e'"))))
      ;; Old data still queryable.
      (should (equal "v1 entity"
                     (caar (sqlite-select
                            elot-db
                            "SELECT label FROM entities WHERE id = 'v1e'"))))
      ;; Global prefixes seeded as part of init.
      (should (> (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM global_prefixes"))
                 0)))))

(ert-deftest test-elot-db-prefixes-cascade-on-source-delete ()
  "Deleting a source cascades through prefixes."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "s1")
    (elot-db-add-prefix "s1" nil "ex" "http://example.org/")
    (should (= 1 (caar (sqlite-select
                        elot-db
                        "SELECT COUNT(*) FROM prefixes WHERE source = 's1'"))))
    (sqlite-execute elot-db "DELETE FROM sources WHERE source = ?" (list "s1"))
    (should (= 0 (caar (sqlite-select
                        elot-db "SELECT COUNT(*) FROM prefixes"))))))

;;;; Subtask B: prefix API

(ert-deftest test-elot-db-expand-curie-source-scoped ()
  "Same prefix bound differently in two sources resolves per
active-source priority order."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "A")
    (elot-db-test--seed-source "B")
    (elot-db-add-prefix "A" nil "ex" "http://a.example/")
    (elot-db-add-prefix "B" nil "ex" "http://b.example/")
    (should (equal "http://a.example/hello"
                   (elot-db-expand-curie "ex:hello" '(("A" nil) ("B" nil)))))
    (should (equal "http://b.example/hello"
                   (elot-db-expand-curie "ex:hello" '(("B" nil) ("A" nil)))))))

(ert-deftest test-elot-db-expand-curie-global-fallback ()
  "A prefix not in any active source falls back to global_prefixes."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "A")
    ;; `rdfs' is a seeded global prefix and is absent from source A.
    (should (equal "http://www.w3.org/2000/01/rdf-schema#label"
                   (elot-db-expand-curie "rdfs:label" '(("A" nil)))))))

(ert-deftest test-elot-db-default-empty-prefix ()
  "The empty-string prefix handles CURIEs of the form `:local'."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "A")
    (elot-db-add-prefix "A" nil "" "http://default.example/")
    (should (equal "http://default.example/foo"
                   (elot-db-expand-curie ":foo" '(("A" nil)))))))

(ert-deftest test-elot-db-expand-curie-miss ()
  "Unknown prefix returns nil; non-CURIE input returns nil."
  (elot-db-test--with-fresh-db
    (should (null (elot-db-expand-curie "nope:bar" nil)))
    (should (null (elot-db-expand-curie "not-a-curie" nil)))))

(ert-deftest test-elot-db-contract-uri-longest-match ()
  "URI matching two expansions returns the longer-prefix CURIE first."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "A")
    ;; Two expansions where one is a strict prefix of the other.
    (elot-db-add-prefix "A" nil "ex"     "http://example.org/")
    (elot-db-add-prefix "A" nil "exsub"  "http://example.org/sub/")
    (let ((cs (elot-db-contract-uri "http://example.org/sub/foo"
                                    '(("A" nil)))))
      (should (equal "exsub:foo" (car cs)))
      (should (member "ex:sub/foo" cs)))))

(ert-deftest test-elot-db-list-prefixes-scoped ()
  "`elot-db-list-prefixes' returns scoped results per source."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-source "A")
    (elot-db-test--seed-source "B")
    (elot-db-add-prefix "A" nil "ex" "http://a.example/")
    (elot-db-add-prefix "B" nil "ex" "http://b.example/")
    (should (= 1 (length (elot-db-list-prefixes "A"))))
    (should (= 2 (length (elot-db-list-prefixes))))))

;;;; Subtask C: two-pass label lookup

(ert-deftest test-elot-db-get-label-any-literal-match ()
  "Pass 1: literal id match wins without consulting prefix tables.
UC3 scenario: an opaque `EMP-12345' with no prefix anywhere."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-entity "EMP-12345" "Ada Lovelace" "emp.csv")
    (should (equal "Ada Lovelace"
                   (elot-db-get-label-any "EMP-12345"
                                          '(("emp.csv" nil)))))))

(ert-deftest test-elot-db-get-label-curie-only-source ()
  "Source stores `entities.id = ex:hello' with no prefixes row;
lookup with `ex:hello' still returns the label because the literal
pass fires first."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-entity "ex:hello" "Greeting" "A" nil "curie")
    (should (equal "Greeting"
                   (elot-db-get-label-any "ex:hello" '(("A" nil)))))))

(ert-deftest test-elot-db-get-label-any-curie-to-uri ()
  "Pass 2: entity stored under full URI, queried as CURIE; prefix
machinery re-resolves and finds it."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-entity "http://example.org/hello" "Greeting"
                               "A" nil "uri")
    (elot-db-add-prefix "A" nil "ex" "http://example.org/")
    (should (equal "Greeting"
                   (elot-db-get-label-any "ex:hello" '(("A" nil)))))))

(ert-deftest test-elot-db-get-label-any-uri-to-curie ()
  "Pass 3: entity stored as CURIE, queried as the full URI; URI is
contracted and found as CURIE."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-entity "ex:hello" "Greeting" "A" nil "curie")
    (elot-db-add-prefix "A" nil "ex" "http://example.org/")
    (should (equal "Greeting"
                   (elot-db-get-label-any "http://example.org/hello"
                                          '(("A" nil)))))))

(ert-deftest test-elot-db-get-label-any-miss ()
  "All three passes miss -> nil."
  (elot-db-test--with-fresh-db
    (elot-db-test--seed-entity "EMP-1" "Alice" "emp.csv")
    (should (null (elot-db-get-label-any "EMP-404" '(("emp.csv" nil)))))
    (should (null (elot-db-get-label-any "ex:nope" '(("emp.csv" nil)))))
    (should (null (elot-db-get-label-any "http://no/where"
                                         '(("emp.csv" nil)))))))

(ert-deftest test-elot-db-get-label-late-binding-duplicates ()
  "When a CURIE-stored row and a URI-stored row for the same resource
coexist, both are individually resolvable via `elot-db-get-label-any'.
v1 accepts the duplication as a documented limitation."
  (elot-db-test--with-fresh-db
    ;; Source A: ships the bare CURIE.
    (elot-db-test--seed-entity "ex:hello" "Greeting (CURIE source)"
                               "A" nil "curie")
    ;; Source B: ships the full URI, plus the prefix binding.
    (elot-db-test--seed-entity "http://example.org/hello"
                               "Greeting (URI source)"
                               "B" nil "uri")
    (elot-db-add-prefix "B" nil "ex" "http://example.org/")
    (let ((active '(("A" nil) ("B" nil))))
      ;; Query as CURIE: literal match in A wins.
      (should (equal "Greeting (CURIE source)"
                     (elot-db-get-label-any "ex:hello" active)))
      ;; Query as URI: literal match in B wins.
      (should (equal "Greeting (URI source)"
                     (elot-db-get-label-any "http://example.org/hello"
                                            active))))))

(provide 'elot-db-prefixes-test)

;;; elot-db-prefixes-test.el ends here
