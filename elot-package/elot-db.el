;;; elot-db.el --- SQLite-backed label/attribute cache for ELOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 The ELOT authors
;; SPDX-License-Identifier: MIT

;; Author: ELOT contributors
;; Keywords: tools, hypermedia, data

;;; Commentary:

;; Low-level SQLite layer for the ELOT global-slurp subsystem.
;;
;; This file is the *only* file in ELOT that talks SQL.  Everything else
;; interacts with labels and attributes through the `elot-db-*' API.  It
;; is also the portability seam: the VS Code port reimplements these
;; functions in TypeScript against `sql.js', sharing the schema
;; verbatim.
;;
;; See `ELOT-DB-PLAN.org' (top-level of the repository) for the
;; authoritative design.  This module implements Step 1.1 of that plan:
;; the schema, the connection lifecycle (`elot-db-init',
;; `elot-db-close'), and the migration stub (`elot-db-migrate').
;;
;; Note: unlike most of ELOT, this file is authored directly as Elisp
;; rather than tangled from an Org document -- see the Decisions Log
;; in `ELOT-DB-PLAN.org' for rationale.
;;
;; Requires Emacs 29+ for built-in `sqlite.el'.

;;; Code:

(require 'sqlite)

;;;; Customisation & state

(defgroup elot-db nil
  "SQLite-backed label/attribute cache for ELOT."
  :group 'elot
  :prefix "elot-db-")

(defcustom elot-db-file
  (locate-user-emacs-file "elot-cache.sqlite")
  "Path to the SQLite file backing the ELOT global-slurp cache.
Cross-editor sharing (e.g. with a VS Code port) is deliberately out of
scope at this stage."
  :type 'file
  :group 'elot-db)

(defconst elot-db-schema-version 1
  "Code-side schema version.
Compared against the value stored in the `schema_version' table by
`elot-db-migrate'.  Bump this constant when the DDL changes and add a
corresponding migration branch.")

(defvar elot-db nil
  "Open SQLite connection used by the `elot-db-*' API, or nil.")

;;;; Schema

(defconst elot-db--schema-ddl
  "\
CREATE TABLE IF NOT EXISTS schema_version (
  version INTEGER PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS sources (
  source        TEXT NOT NULL,
  data_source   TEXT NOT NULL DEFAULT '',
  type          TEXT,
  last_modified REAL,
  last_updated  REAL,
  PRIMARY KEY (source, data_source)
);

CREATE TABLE IF NOT EXISTS entities (
  id          TEXT,
  label       TEXT,
  source      TEXT NOT NULL,
  data_source TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (id, source, data_source),
  FOREIGN KEY (source, data_source)
    REFERENCES sources(source, data_source) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS attributes (
  id          TEXT,
  source      TEXT NOT NULL,
  data_source TEXT NOT NULL DEFAULT '',
  prop        TEXT,
  value       TEXT,
  FOREIGN KEY (id, source, data_source)
    REFERENCES entities(id, source, data_source) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_attrs_id_source
  ON attributes(id, source, data_source);
CREATE INDEX IF NOT EXISTS idx_entities_id
  ON entities(id);
"
  "Canonical DDL applied by `elot-db-init'.
Kept verbatim so it can later be lifted into `schema.sql' (Step 2.1 in
the plan) without content changes.")

;;;; Lifecycle

(defun elot-db--apply-schema (db)
  "Apply `elot-db--schema-ddl' to DB.
Statements are split on `;' and executed individually because
`sqlite-execute' accepts only a single statement."
  (dolist (stmt (split-string elot-db--schema-ddl ";" t "[ \t\n\r]+"))
    (sqlite-execute db stmt)))

(defun elot-db--fresh-p (db)
  "Return non-nil if DB has no row in `schema_version'."
  (null (sqlite-select db "SELECT version FROM schema_version LIMIT 1")))

(defun elot-db--seed-version (db)
  "Insert the current `elot-db-schema-version' into DB."
  (sqlite-execute db
                  "INSERT INTO schema_version (version) VALUES (?)"
                  (list elot-db-schema-version)))

(defun elot-db-migrate (&optional db)
  "Check the stored schema version against `elot-db-schema-version'.
Signal a `user-error' on mismatch.  No automatic migration is
performed: users must invoke the appropriate migration tooling
manually.  DB defaults to `elot-db'."
  (let* ((db (or db elot-db))
         (row (sqlite-select db "SELECT version FROM schema_version LIMIT 1"))
         (stored (caar row)))
    (cond
     ((null stored)
      (user-error
       "elot-db: schema_version table is empty; database looks corrupt"))
     ((not (equal stored elot-db-schema-version))
      (user-error
       "elot-db: schema version mismatch (stored %s, code %s); \
no automatic migration performed"
       stored elot-db-schema-version))
     (t stored))))

;;;###autoload
(defun elot-db-init (&optional path)
  "Open the ELOT SQLite cache and ensure the schema is present.
PATH overrides `elot-db-file' (useful for tests).  On a fresh database,
the schema is created and `schema_version' is seeded with
`elot-db-schema-version'.  On a pre-existing database,
`elot-db-migrate' is called and will error on version mismatch.
Returns the open connection and stores it in `elot-db'."
  (unless (fboundp 'sqlite-open)
    (error "elot-db: this Emacs build has no SQLite support"))
  (let* ((file (or path elot-db-file))
         (db   (sqlite-open file))
         (ok   nil))
    (unwind-protect
        (progn
          ;; `sqlite-pragma' is the documented way to toggle pragmas;
          ;; going through `sqlite-execute' is unreliable here.
          (sqlite-pragma db "foreign_keys = ON")
          (elot-db--apply-schema db)
          (if (elot-db--fresh-p db)
              (elot-db--seed-version db)
            (elot-db-migrate db))
          (setq elot-db db
                ok t)
          db)
      ;; If anything above signalled (typically a migration mismatch),
      ;; close the freshly-opened handle so the caller can delete the
      ;; file on Windows where open handles keep files locked.
      (unless ok
        (ignore-errors (sqlite-close db))))))

(defun elot-db-close ()
  "Close the connection stored in `elot-db', if any."
  (when (and elot-db (sqlitep elot-db))
    (sqlite-close elot-db))
  (setq elot-db nil))

(provide 'elot-db)

;;; elot-db.el ends here
