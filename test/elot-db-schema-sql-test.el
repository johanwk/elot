;;; elot-db-schema-sql-test.el --- Step 2.1 schema.sql pinning test  -*- lexical-binding: t; -*-

;;; Commentary:

;; Step 2.1 test: ensure the on-disk `schema.sql' file (shared
;; verbatim with the VS Code / CLI port) and the embedded fallback
;; string `elot-db--schema-ddl-embedded' are byte-identical, so the
;; two cannot drift.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

(ert-deftest test-elot-db-schema-sql-file-exists ()
  "`schema.sql' must ship alongside `elot-db.el'."
  (let ((path (elot-db--schema-sql-path)))
    (should path)
    (should (file-readable-p path))))

(ert-deftest test-elot-db-schema-sql-matches-embedded-ddl ()
  "On-disk `schema.sql' and the embedded fallback must match byte-for-byte."
  (let* ((path (elot-db--schema-sql-path))
         (on-disk (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string))))
    (should (equal on-disk elot-db--schema-ddl-embedded))))

(ert-deftest test-elot-db-init-uses-schema-sql ()
  "A fresh DB init via the file path produces the v3 shape."
  (let ((tmp (make-temp-file "elot-db-schema-sql-" nil ".sqlite")))
    (ignore-errors (delete-file tmp))
    (unwind-protect
        (let ((db (elot-db-init tmp)))
          (unwind-protect
              (progn
                ;; schema_version seeded to 3
                (should (equal
                         3
                         (caar (sqlite-select
                                db "SELECT version FROM schema_version"))))
                ;; attributes has `lang' column
                (should
                 (cl-some (lambda (r) (equal (nth 1 r) "lang"))
                          (sqlite-select
                           db "PRAGMA table_info(attributes)"))))
            (ignore-errors (elot-db-close))))
      (ignore-errors (delete-file tmp)))))

(provide 'elot-db-schema-sql-test)

;;; elot-db-schema-sql-test.el ends here
