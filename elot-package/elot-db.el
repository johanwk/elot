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
;; authoritative design.  This module implements Steps 1.1 and 1.1.1:
;; the schema (v2, with prefix support), the connection lifecycle
;; (`elot-db-init', `elot-db-close'), the migration stub
;; (`elot-db-migrate', including a v1->v2 branch), a small prefix API
;; (`elot-db-add-prefix', `elot-db-list-prefixes',
;; `elot-db-expand-curie', `elot-db-contract-uri'), and the two-pass
;; label lookup (`elot-db-get-label-any').
;;
;; Note: unlike most of ELOT, this file is authored directly as Elisp
;; rather than tangled from an Org document -- see the Decisions Log
;; in `ELOT-DB-PLAN.org' for rationale.
;;
;; Requires Emacs 29+ for built-in `sqlite.el'.

;;; Code:

(require 'sqlite)
(require 'cl-lib)
(require 'seq)

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

(defcustom elot-db-default-global-prefixes
  '(("rdf"     . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    ("rdfs"    . "http://www.w3.org/2000/01/rdf-schema#")
    ("owl"     . "http://www.w3.org/2002/07/owl#")
    ("xsd"     . "http://www.w3.org/2001/XMLSchema#")
    ("skos"    . "http://www.w3.org/2004/02/skos/core#")
    ("dcterms" . "http://purl.org/dc/terms/")
    ("foaf"    . "http://xmlns.com/foaf/0.1/")
    ("obo"     . "http://purl.obolibrary.org/obo/")
    ("sh"      . "http://www.w3.org/ns/shacl#")
    ("prov"    . "http://www.w3.org/ns/prov#"))
  "Seed entries for the `global_prefixes' table.
Each entry is a (PREFIX . EXPANSION) cons.  Rows are inserted with
INSERT OR IGNORE at `elot-db-init' time, so user edits to existing
rows are preserved across re-init calls."
  :type '(alist :key-type string :value-type string)
  :group 'elot-db)

(defconst elot-db-schema-version 2
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
  kind        TEXT NOT NULL DEFAULT 'unknown',
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

CREATE TABLE IF NOT EXISTS prefixes (
  source        TEXT NOT NULL,
  data_source   TEXT NOT NULL DEFAULT '',
  prefix        TEXT NOT NULL,
  expansion     TEXT NOT NULL,
  PRIMARY KEY (source, data_source, prefix),
  FOREIGN KEY (source, data_source)
    REFERENCES sources(source, data_source) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS global_prefixes (
  prefix      TEXT PRIMARY KEY,
  expansion   TEXT NOT NULL,
  trust       INTEGER NOT NULL DEFAULT 1
);

CREATE INDEX IF NOT EXISTS idx_attrs_id_source
  ON attributes(id, source, data_source);
CREATE INDEX IF NOT EXISTS idx_entities_id
  ON entities(id);
CREATE INDEX IF NOT EXISTS idx_prefixes_expansion
  ON prefixes(expansion);
"
  "Canonical DDL applied by `elot-db-init' (schema v2).
Kept verbatim so it can later be lifted into `schema.sql' (Step 2.1 in
the plan) without content changes.")

;;;; Lifecycle

(defun elot-db--apply-schema (db)
  "Apply `elot-db--schema-ddl' to DB.
Statements are split on `;' and executed individually because
`sqlite-execute' accepts only a single statement.  All CREATE statements
use IF NOT EXISTS so this is idempotent on existing databases."
  (dolist (stmt (split-string elot-db--schema-ddl ";" t "[ \t\n\r]+"))
    (sqlite-execute db stmt)))

(defun elot-db--fresh-p (db)
  "Return non-nil if DB has no row in `schema_version'."
  (null (sqlite-select db "SELECT version FROM schema_version LIMIT 1")))

(defun elot-db--seed-version (db version)
  "Insert schema VERSION into DB (for a fresh database)."
  (sqlite-execute db
                  "INSERT INTO schema_version (version) VALUES (?)"
                  (list version)))

(defun elot-db--entities-has-kind-p (db)
  "Return non-nil if the `entities' table in DB has a `kind' column."
  (seq-some (lambda (row) (equal (nth 1 row) "kind"))
            (sqlite-select db "PRAGMA table_info(entities)")))

(defun elot-db--seed-global-prefixes (db)
  "Insert `elot-db-default-global-prefixes' into DB (INSERT OR IGNORE).
User edits to existing rows are preserved."
  (dolist (pair elot-db-default-global-prefixes)
    (sqlite-execute
     db
     "INSERT OR IGNORE INTO global_prefixes (prefix, expansion) VALUES (?, ?)"
     (list (car pair) (cdr pair)))))

(defun elot-db--migrate-v1-to-v2 (db)
  "Apply the v1 -> v2 migration steps to DB.
Assumes `elot-db--apply-schema' has already created any missing v2
tables and indexes via IF NOT EXISTS; this only handles changes that
cannot be expressed as CREATE IF NOT EXISTS (namely, adding the
`kind' column to an existing `entities' table) and bumps the stored
version."
  (unless (elot-db--entities-has-kind-p db)
    (sqlite-execute
     db
     "ALTER TABLE entities ADD COLUMN kind TEXT NOT NULL DEFAULT 'unknown'"))
  (sqlite-execute db "UPDATE schema_version SET version = ?" (list 2)))

(defun elot-db-migrate (&optional db)
  "Reconcile the stored schema version with `elot-db-schema-version'.
Handles the v1 -> v2 transition (Step 1.1.1) automatically.  For any
other mismatch, signal a `user-error' rather than silently rewrite the
user's cache.  DB defaults to `elot-db'."
  (let* ((db (or db elot-db))
         (row (sqlite-select db "SELECT version FROM schema_version LIMIT 1"))
         (stored (caar row)))
    (cond
     ((null stored)
      (user-error
       "elot-db: schema_version table is empty; database looks corrupt"))
     ((equal stored elot-db-schema-version)
      stored)
     ((and (equal stored 1) (equal elot-db-schema-version 2))
      (elot-db--migrate-v1-to-v2 db)
      elot-db-schema-version)
     (t
      (user-error
       "elot-db: schema version mismatch (stored %s, code %s); \
no automatic migration path"
       stored elot-db-schema-version)))))

;;;###autoload
(defun elot-db-init (&optional path)
  "Open the ELOT SQLite cache and ensure the schema is present.
PATH overrides `elot-db-file' (useful for tests).  On a fresh database,
the schema is created and `schema_version' is seeded with
`elot-db-schema-version'.  On a pre-existing database,
`elot-db-migrate' reconciles the stored version; a supported transition
(e.g. v1 -> v2) is applied automatically, any other mismatch errors.
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
              (elot-db--seed-version db elot-db-schema-version)
            (elot-db-migrate db))
          ;; Global-prefix seed is idempotent; safe to run every init.
          (elot-db--seed-global-prefixes db)
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


;;;; -------------------------------------------------------------------
;;;; Step 1.1.1 subtask B: Prefix API
;;;; -------------------------------------------------------------------

(defun elot-db--normalize-ds (data-source)
  "Return DATA-SOURCE coerced to the empty-string sentinel when nil."
  (or data-source ""))

(defun elot-db-add-prefix (source data-source prefix expansion)
  "Upsert a row (SOURCE, DATA-SOURCE, PREFIX, EXPANSION) in `prefixes'.
DATA-SOURCE may be nil; the empty-string sentinel is stored in that
case.  PREFIX may be the empty string to represent the default (`:')
prefix."
  (sqlite-execute
   elot-db
   "INSERT OR REPLACE INTO prefixes (source, data_source, prefix, expansion)
    VALUES (?, ?, ?, ?)"
   (list source (elot-db--normalize-ds data-source) prefix expansion)))

(defun elot-db-list-prefixes (&optional source data-source)
  "Return prefix rows, optionally scoped to SOURCE / DATA-SOURCE.
Each returned element is a list (SOURCE DATA-SOURCE PREFIX EXPANSION)."
  (cond
   ((and source data-source)
    (sqlite-select
     elot-db
     "SELECT source, data_source, prefix, expansion FROM prefixes
       WHERE source = ? AND data_source = ?
       ORDER BY prefix"
     (list source (elot-db--normalize-ds data-source))))
   (source
    (sqlite-select
     elot-db
     "SELECT source, data_source, prefix, expansion FROM prefixes
       WHERE source = ?
       ORDER BY data_source, prefix"
     (list source)))
   (t
    (sqlite-select
     elot-db
     "SELECT source, data_source, prefix, expansion FROM prefixes
       ORDER BY source, data_source, prefix"))))

(defun elot-db--expansion-in-sources (prefix active-sources)
  "Look up PREFIX's expansion in ACTIVE-SOURCES (in priority order).
Return the first match or nil.  ACTIVE-SOURCES entries are
(SOURCE DATA-SOURCE) pairs."
  (cl-loop
   for entry in active-sources
   for src = (nth 0 entry)
   for ds  = (elot-db--normalize-ds (nth 1 entry))
   for row = (sqlite-select
              elot-db
              "SELECT expansion FROM prefixes
                WHERE prefix = ? AND source = ? AND data_source = ?
                LIMIT 1"
              (list prefix src ds))
   when row return (caar row)))

(defun elot-db--expansion-global (prefix)
  "Look up PREFIX's expansion in `global_prefixes', or return nil."
  (caar (sqlite-select
         elot-db
         "SELECT expansion FROM global_prefixes WHERE prefix = ? LIMIT 1"
         (list prefix))))

(defun elot-db-expand-curie (curie &optional active-sources)
  "Expand CURIE (\"prefix:local\") to a full URI, or return nil.
Resolution consults ACTIVE-SOURCES in order first, then falls through
to `global_prefixes'.  The default prefix (e.g. `:foo') is supported
via the empty-string prefix row.  If CURIE does not look like a CURIE,
return nil."
  (when (and (stringp curie)
             (string-match "\\`\\([^:]*\\):\\(.*\\)\\'" curie))
    (let* ((prefix (match-string 1 curie))
           (local  (match-string 2 curie))
           (exp    (or (elot-db--expansion-in-sources prefix active-sources)
                       (elot-db--expansion-global prefix))))
      (and exp (concat exp local)))))

(defun elot-db-contract-uri (uri &optional active-sources)
  "Return a list of candidate CURIE strings for URI.
Candidates are ordered by longest-expansion match first, then by
ACTIVE-SOURCES priority (earlier = higher), with `global_prefixes'
entries appended last.  Returns nil if no prefix expansion is a strict
prefix of URI."
  (let (candidates)
    ;; Source-scoped rows, in active-source priority order.
    (dolist (entry active-sources)
      (let ((src (nth 0 entry))
            (ds  (elot-db--normalize-ds (nth 1 entry))))
        (dolist (row (sqlite-select
                      elot-db
                      "SELECT prefix, expansion FROM prefixes
                        WHERE source = ? AND data_source = ?"
                      (list src ds)))
          (let ((p (nth 0 row)) (e (nth 1 row)))
            (when (and (> (length uri) (length e))
                       (string-prefix-p e uri))
              (push (cons (length e)
                          (concat p ":" (substring uri (length e))))
                    candidates))))))
    ;; Global rows, lowest priority.
    (dolist (row (sqlite-select
                  elot-db
                  "SELECT prefix, expansion FROM global_prefixes"))
      (let ((p (nth 0 row)) (e (nth 1 row)))
        (when (and (> (length uri) (length e))
                   (string-prefix-p e uri))
          (push (cons (length e)
                      (concat p ":" (substring uri (length e))))
                candidates))))
    ;; Stable sort by descending expansion length; ties keep
    ;; insertion order (Emacs `sort' is stable).
    (setq candidates (nreverse candidates))
    (mapcar #'cdr (sort candidates (lambda (a b) (> (car a) (car b)))))))


;;;; -------------------------------------------------------------------
;;;; Step 1.1.1 subtask C: Two-pass label lookup
;;;; -------------------------------------------------------------------

(defun elot-db--looks-like-uri-p (s)
  "Return non-nil if S looks like a full URI (contains `://')."
  (and (stringp s) (string-match-p "://" s)))

(defun elot-db--looks-like-curie-p (s)
  "Return non-nil if S looks like a CURIE (has `:' but no `://')."
  (and (stringp s)
       (string-match-p ":" s)
       (not (elot-db--looks-like-uri-p s))))

(defun elot-db--label-in-sources (id active-sources)
  "Return label for ID by trying each entry in ACTIVE-SOURCES in order.
First hit wins; returns nil if no source has ID.  Entries are
(SOURCE DATA-SOURCE) pairs."
  (cl-loop
   for entry in active-sources
   for src = (nth 0 entry)
   for ds  = (elot-db--normalize-ds (nth 1 entry))
   for row = (sqlite-select
              elot-db
              "SELECT label FROM entities
                WHERE id = ? AND source = ? AND data_source = ?
                LIMIT 1"
              (list id src ds))
   when row return (caar row)))

(defun elot-db-get-label-any (token &optional active-sources)
  "Two-pass label lookup for TOKEN, restricted to ACTIVE-SOURCES.

1. Try TOKEN as a literal `entities.id' (the valuable case for UC3
   and for CURIE-only sources).
2. If TOKEN looks like a CURIE, expand it via the active sources'
   `prefixes' tables (then `global_prefixes') and re-lookup as a
   full URI.
3. If TOKEN looks like a full URI, contract it via the same prefix
   tables (longest-expansion first, then by active-source priority)
   and try each candidate CURIE.

Return the label, or nil if all passes miss."
  (or (elot-db--label-in-sources token active-sources)
      (and (elot-db--looks-like-curie-p token)
           (let ((uri (elot-db-expand-curie token active-sources)))
             (and uri (elot-db--label-in-sources uri active-sources))))
      (and (elot-db--looks-like-uri-p token)
           (cl-some (lambda (curie)
                      (elot-db--label-in-sources curie active-sources))
                    (elot-db-contract-uri token active-sources)))))

(provide 'elot-db)

;;; elot-db.el ends here
