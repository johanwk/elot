;;; elot-db.el --- SQLite-backed label/attribute cache for ELOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Keywords: tools, hypermedia, data

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Low-level SQLite layer for the ELOT global-slurp subsystem.
;;
;; This file is the *only* file in ELOT that talks SQL.  Everything else
;; interacts with labels and attributes through the `elot-db-*' API.  It
;; is also the portability seam: the VS Code port reimplements these
;; functions in TypeScript against `sql.js', sharing the schema
;; verbatim.
;;
;; The module provides the schema (with prefix support), the
;; connection lifecycle (`elot-db-init', `elot-db-close'), the
;; migration handler (`elot-db-migrate'), a small prefix API
;; (`elot-db-add-prefix', `elot-db-list-prefixes',
;; `elot-db-expand-curie', `elot-db-contract-uri'), the two-pass
;; label lookup (`elot-db-get-label-any'), plus the read/write
;; primitives (`elot-db-update-source', `elot-db-remove-source',
;; `elot-db-source-exists-p', `elot-db-list-sources',
;; `elot-db-source-needs-update-p', `elot-db-get-label',
;; `elot-db-get-attr', `elot-db-get-all-attrs').
;;
;; Unlike most of ELOT, this file is authored directly as Elisp
;; rather than tangled from an Org document.
;;
;; The canonical schema DDL lives in the sibling file `schema.sql',
;; shared verbatim with the VS Code / CLI port.  `elot-db-init'
;; reads it on first use; an embedded copy
;; (`elot-db--schema-ddl-embedded') is kept as a defensive fallback
;; and test-pinned byte-for-byte to the on-disk file.
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

(defcustom elot-db-sync-on-slurp t
  "When non-nil (default), `elot-slurp-to-vars' writes the slurp
into the ELOT label DB (`elot-db-file').

Set to nil in test harnesses or other ephemeral contexts to keep
short-lived fixture files out of the user's global label cache.
The local `elot-slurp', `elot-codelist-ht' and `elot-attriblist-ht'
buffer-local variables are unaffected -- only the cross-buffer DB
sync is suppressed.

Disable from the command line via:

    emacs --batch --eval \"(setq elot-db-sync-on-slurp nil)\" ..."
  :type 'boolean
  :group 'elot-db)

(defcustom elot-db-busy-timeout-ms 5000
  "SQLite `busy_timeout' (milliseconds) applied at connection open.
When a second connection encounters a locked database (typically a
short-lived writer from `elot-slurp-to-vars' contending with the main
ELOT connection), SQLite will retry up to this many milliseconds before
returning `database is locked'.  The default of 5 seconds matches the
`sqlite' CLI and is comfortably more than any single ELOT write takes.
Set to 0 to disable (restore the historical immediate-fail behaviour)."
  :type 'integer
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

(defconst elot-db-schema-version 3
  "Code-side schema version.
Compared against the value stored in the `schema_version' table by
`elot-db-migrate'.  Bump this constant when the DDL changes and add a
corresponding migration branch.")

(defvar elot-db nil
  "Open SQLite connection used by the `elot-db-*' API, or nil.")

(defvar-local elot-active-label-sources nil
  "Buffer-local ordered list of active label sources.
Each entry is a \(SOURCE DATA-SOURCE) pair; DATA-SOURCE is nil (or
the empty string) for non-SPARQL sources.  Earlier entries have
higher priority.  Provides a stable default for the label-lookup
primitives when no project-local configuration is active.")
;;;###autoload
(put 'elot-active-label-sources 'safe-local-variable
     (lambda (v)
       (and (listp v)
            (cl-every (lambda (e)
                        (and (listp e)
                             (>= (length e) 1)
                             (stringp (nth 0 e))
                             (or (null (nth 1 e))
                                 (stringp (nth 1 e)))))
                      v))))

;;;; Language preferences

(defcustom elot-preferred-languages nil
  "Ordered list of preferred language tags for labels and annotations.

Each element is either a BCP-47 language tag as a string (e.g. \"en\",
\"ko\", \"nb\"), or the symbol `:untagged' which represents literals
carrying no language tag at all.

When non-nil, the list specifies the user's priority order: earlier
entries win over later ones.  If `:untagged' does not appear
explicitly, it is treated as appended to the tail of the list; to
pin untagged literals at a specific position, include `:untagged'
in the list at the desired rank.

When nil (the default), the built-in policy applies:
 1. untagged literals win;
 2. failing that, \"en\" wins;
 3. failing that, the alphabetically first available tag wins.

This is a presentation preference, used uniformly for labels and for
annotations such as `rdfs:comment' and `skos:definition'.  It is
intentionally not stored in the SQLite cache.

Typically set via Customize, or per project via a `.dir-locals.el'
entry for `elot-preferred-languages'.

Consulted by `elot-db-get-attr', `elot-db-get-all-attrs',
`elot-db-get-label', and `elot-db-all-active-labels' via
`elot-db--pick-value-by-lang' / `elot-db--select-by-language'.
Slurp-path readers in ELOT Org buffers are intentionally not
affected.  See also `README-global-label-display.org' for examples."
  :type '(repeat (choice (const :tag "Untagged literal" :untagged)
                         (string :tag "Language tag (e.g. \"en\")")))
  :group 'elot-db)

;;;###autoload
(put 'elot-preferred-languages 'safe-local-variable
     (lambda (v)
       (and (listp v)
            (cl-every (lambda (e)
                        (or (eq e :untagged)
                            (stringp e)))
                      v))))

(defun elot-db--effective-language-prefs (&optional prefs)
  "Return the effective language-preference list.

PREFS overrides `elot-preferred-languages' when non-nil.  When the
resolved list is nil, return the built-in default policy
\\='(:untagged \"en\") -- callers treat the alphabetical-first
fallback as implicit.  When the resolved list is non-nil and does
not contain `:untagged', append `:untagged' at the tail so that
untagged literals are always considered after the user's
explicitly-ranked tags (but before the alphabetical fallback)."
  (let ((lst (or prefs elot-preferred-languages)))
    (cond
     ((null lst) '(:untagged "en"))
     ((memq :untagged lst) lst)
     (t (append lst (list :untagged))))))

(defun elot-db--select-by-language (rows &optional prefs)
  "Pick the best row from ROWS according to language preferences.

ROWS is a list of (VALUE . LANG) cons cells, where LANG is either a
non-empty string language tag or nil/\"\" meaning untagged.  Returns
the winning (VALUE . LANG) pair, or nil when ROWS is empty.

PREFS is the caller-supplied preference list (see
`elot-db--effective-language-prefs').  Selection policy:

 1. Walk the effective preference list in order.  The first entry
    that matches any row wins (`:untagged' matches rows whose LANG is
    nil or empty; a string entry matches rows whose LANG equals it,
    case-insensitively).
 2. If nothing in the preference list matches, fall back to the row
    whose LANG sorts alphabetically first (ties broken by input
    order; untagged rows are considered only via `:untagged', not
    here).
 3. If ROWS has exactly one element, return it directly."
  (cond
   ((null rows) nil)
   ((null (cdr rows)) (car rows))
   (t
    (let* ((effective (elot-db--effective-language-prefs prefs))
           (norm (lambda (l) (if (or (null l) (equal l "")) nil
                               (downcase l))))
           (winner nil))
      (catch 'found
        (dolist (pref effective)
          (dolist (row rows)
            (let ((rl (funcall norm (cdr row))))
              (cond
               ((and (eq pref :untagged) (null rl))
                (setq winner row) (throw 'found nil))
               ((and (stringp pref) rl
                     (string= rl (downcase pref)))
                (setq winner row) (throw 'found nil)))))))
      (or winner
          ;; Alphabetical fallback over tagged rows only.
          (let* ((tagged (seq-filter (lambda (r)
                                       (and (cdr r) (not (equal (cdr r) ""))))
                                     rows))
                 (sorted (sort (copy-sequence (or tagged rows))
                               (lambda (a b)
                                 (string< (or (cdr a) "") (or (cdr b) ""))))))
            (car sorted)))))))

;;;; Schema

(defconst elot-db--schema-ddl-embedded
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
  lang        TEXT NOT NULL DEFAULT '',
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
CREATE INDEX IF NOT EXISTS idx_attrs_prop_lang
  ON attributes(prop, lang);
"
  "Canonical DDL applied by `elot-db-init' as an embedded fallback.
Used when the sibling `schema.sql' file cannot be located; schema v3.
The authoritative on-disk copy is `elot-package/schema.sql' and is
shared verbatim with the VS Code / CLI port.  The embedded copy
exists purely as a defensive fallback; a test pins
the two together byte-for-byte so drift is caught at test time.")

(defun elot-db--schema-sql-path ()
  "Return the filesystem path to the canonical `schema.sql', or nil.
Located alongside the `elot-db' library file; returns nil when the
library itself cannot be located (defensive -- e.g. when running
from a raw source tree before byte-compilation)."
  (let ((lib (or (locate-library "elot-db")
                 ;; When loaded via `load-file' the library is not
                 ;; yet on `load-path'; fall back to
                 ;; `load-file-name' / `buffer-file-name' at
                 ;; compile time.
                 load-file-name
                 (and (boundp 'byte-compile-current-file)
                      byte-compile-current-file))))
    (when lib
      (let ((candidate (expand-file-name
                        "schema.sql"
                        (file-name-directory lib))))
        (and (file-readable-p candidate) candidate)))))

(defun elot-db--read-schema-sql ()
  "Return the contents of the canonical `schema.sql' as a string.
Falls back to `elot-db--schema-ddl-embedded' if the file is
missing or unreadable."
  (let ((path (elot-db--schema-sql-path)))
    (if path
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      elot-db--schema-ddl-embedded)))

(defvar elot-db--schema-ddl nil
  "Canonical DDL applied by `elot-db-init' (schema v3).
Resolved lazily on first use by `elot-db--apply-schema': reads
`schema.sql' from the package directory, or falls back to the
embedded copy in `elot-db--schema-ddl-embedded'.")

;;;; Lifecycle

(defun elot-db--apply-schema (db)
  "Apply the canonical schema DDL to DB.
Reads `schema.sql' (shipped alongside this library) on first
call, caching in `elot-db--schema-ddl'; falls back to the
embedded copy in `elot-db--schema-ddl-embedded' when the file is
missing.  Statements are split on `;' and executed individually
because `sqlite-execute' accepts only a single statement.  All
CREATE statements use IF NOT EXISTS so this is idempotent on
existing databases."
  (unless elot-db--schema-ddl
    (setq elot-db--schema-ddl (elot-db--read-schema-sql)))
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

(defun elot-db--attributes-has-lang-p (db)
  "Return non-nil if the `attributes' table in DB has a `lang' column."
  (seq-some (lambda (row) (equal (nth 1 row) "lang"))
            (sqlite-select db "PRAGMA table_info(attributes)")))

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

(defun elot-db--migrate-v2-to-v3 (db)
  "Apply the v2 -> v3 migration steps to DB.
Additive: adds the `lang' column to `attributes' (empty-string
default, so pre-existing rows are left untagged) and creates the
`idx_attrs_prop_lang' index.  The `CREATE INDEX IF NOT EXISTS' in
`elot-db--apply-schema' covers the index on fresh opens; we repeat
it here so that callers holding a v2 DB handle get the index even
if they skip re-applying the schema.  Bumps the stored version."
  (unless (elot-db--attributes-has-lang-p db)
    (sqlite-execute
     db
     "ALTER TABLE attributes ADD COLUMN lang TEXT NOT NULL DEFAULT ''"))
  (sqlite-execute
   db
   "CREATE INDEX IF NOT EXISTS idx_attrs_prop_lang ON attributes(prop, lang)")
  (sqlite-execute db "UPDATE schema_version SET version = ?" (list 3)))

(defun elot-db-migrate (&optional db)
  "Reconcile the stored schema version with `elot-db-schema-version'.
Handles the v1 -> v2 and v2 -> v3 transitions automatically,
including the combined v1 -> v3 path.
For any other mismatch, signal a `user-error' rather than silently
rewrite the user's cache.  DB defaults to `elot-db'."
  (let* ((db (or db elot-db))
         (row (sqlite-select db "SELECT version FROM schema_version LIMIT 1"))
         (stored (caar row)))
    (cond
     ((null stored)
      (user-error
       "Elot-db: schema_version table is empty; database looks corrupt"))
     ((equal stored elot-db-schema-version)
      stored)
     ((and (equal stored 1) (equal elot-db-schema-version 3))
      (elot-db--migrate-v1-to-v2 db)
      (elot-db--migrate-v2-to-v3 db)
      elot-db-schema-version)
     ((and (equal stored 1) (equal elot-db-schema-version 2))
      (elot-db--migrate-v1-to-v2 db)
      elot-db-schema-version)
     ((and (equal stored 2) (equal elot-db-schema-version 3))
      (elot-db--migrate-v2-to-v3 db)
      elot-db-schema-version)
     (t
      (user-error
       "Elot-db: schema version mismatch (stored %s, code %s); \
no automatic migration path"
       stored elot-db-schema-version)))))

;;;###autoload
(defun elot-db-init (&optional path)
  "Open the ELOT SQLite cache and ensure the schema is present.
PATH overrides `elot-db-file' (useful for tests).  On a fresh database,
the schema is created and `schema_version' is seeded with
`elot-db-schema-version'.  On a pre-existing database,
`elot-db-migrate' reconciles the stored version; a supported transition
\(e.g. v1 -> v2) is applied automatically, any other mismatch errors.
Returns the open connection and stores it in `elot-db'."
  (unless (fboundp 'sqlite-open)
    (error "Elot-db: this Emacs build has no SQLite support"))
  (let* ((file (or path elot-db-file))
         (db   (sqlite-open file))
         (ok   nil))
    (unwind-protect
        (progn
          ;; `sqlite-pragma' is the documented way to toggle pragmas;
          ;; going through `sqlite-execute' is unreliable here.
          ;; Apply `busy_timeout' first so every subsequent statement
          ;; (including the schema probe and migration) benefits from
          ;; the retry budget when a concurrent connection holds a
          ;; write lock (e.g. `elot-slurp-to-vars' running in a second
          ;; connection during lint).
          (when (and (integerp elot-db-busy-timeout-ms)
                     (> elot-db-busy-timeout-ms 0))
            (sqlite-pragma db (format "busy_timeout = %d"
                                      elot-db-busy-timeout-ms)))
          (sqlite-pragma db "foreign_keys = ON")
          (if (null (sqlite-select
                     db
                     "SELECT 1 FROM sqlite_master \
WHERE type='table' AND name='schema_version' LIMIT 1"))
              ;; Fresh DB: create everything, seed version.
              (progn
                (elot-db--apply-schema db)
                (elot-db--seed-version db elot-db-schema-version))
            ;; Existing DB: migrate first (adds any missing columns
            ;; that later DDL statements might reference, e.g. the
            ;; v3 `lang' column referenced by `idx_attrs_prop_lang'),
            ;; then re-apply schema to pick up any new IF NOT EXISTS
            ;; tables/indexes introduced since the DB was created.
            (let ((elot-db db))
              (elot-db-migrate db))
            (elot-db--apply-schema db))
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
;;;; Prefix API
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
\(SOURCE DATA-SOURCE) pairs."
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
;;;; Two-pass label lookup
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
\(SOURCE DATA-SOURCE) pairs."
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

Return the label, or nil if all passes miss.
ACTIVE-SOURCES defaults to the buffer-local `elot-active-label-sources'."
  (let ((active-sources (elot-db--active-or-default active-sources)))
    (or (elot-db--label-in-sources token active-sources)
        (and (elot-db--looks-like-curie-p token)
             (let ((uri (elot-db-expand-curie token active-sources)))
               (and uri (elot-db--label-in-sources uri active-sources))))
        (and (elot-db--looks-like-uri-p token)
             (cl-some (lambda (curie)
                        (elot-db--label-in-sources curie active-sources))
                      (elot-db-contract-uri token active-sources))))))


;;;; -------------------------------------------------------------------
;;;; Write path
;;;; -------------------------------------------------------------------

(defun elot-db-source-exists-p (source &optional data-source)
  "Return non-nil if a `sources' row exists for (SOURCE, DATA-SOURCE).
DATA-SOURCE defaults to the empty-string sentinel."
  (let ((row (sqlite-select
              elot-db
              "SELECT 1 FROM sources
                WHERE source = ? AND data_source = ? LIMIT 1"
              (list source (elot-db--normalize-ds data-source)))))
    (and row t)))

(defun elot-db-list-sources ()
  "Return a list of rows (SOURCE DATA-SOURCE TYPE LAST-MODIFIED LAST-UPDATED)
for every registered source.  DATA-SOURCE is returned as stored (empty
string for non-SPARQL sources)."
  (sqlite-select
   elot-db
   "SELECT source, data_source, type, last_modified, last_updated
     FROM sources
     ORDER BY source, data_source"))

(defun elot-db-remove-source (source &optional data-source)
  "Delete the sources row for (SOURCE, DATA-SOURCE) and cascade.
Returns non-nil if a row was removed, nil otherwise."
  (let ((existed (elot-db-source-exists-p source data-source)))
    (when existed
      (sqlite-execute
       elot-db
       "DELETE FROM sources WHERE source = ? AND data_source = ?"
       (list source (elot-db--normalize-ds data-source))))
    existed))

(defun elot-db-source-needs-update-p (file &optional data-source)
  "Return t if FILE has been modified since last parsed into the DB.
Returns t when the source is unknown (i.e. has never been
registered).  DATA-SOURCE defaults to the empty-string sentinel."
  (let* ((attrs (file-attributes file))
         (mtime (and attrs
                     (float-time (file-attribute-modification-time attrs))))
         (row   (sqlite-select
                 elot-db
                 "SELECT last_modified FROM sources
                   WHERE source = ? AND data_source = ? LIMIT 1"
                 (list file (elot-db--normalize-ds data-source))))
         (db-mtime (caar row)))
    (cond
     ((null db-mtime) t)
     ((null mtime)    nil)
     (t (> mtime db-mtime)))))

(defun elot-db-update-source (source data-source type data &optional file-mtime)
  "Replace DB records for (SOURCE, DATA-SOURCE) with DATA.
DATA is a list of (ID LABEL PLIST) triples, where PLIST is a flat
list of \"prop\" \"value\" pairs and may also contain a
`:kind' keyword whose value (\"uri\" / \"curie\" / \"unknown\") is
written to `entities.kind' (the `:kind' pair is *not* written to
`attributes').

A PLIST value may also be a two-element list (VALUE LANG) carrying
an explicit language tag; in that case LANG is written to the
`attributes.lang' column.  Bare-string values write
`lang = \='\='''.  DATA-SOURCE is nil or the empty-string sentinel
for non-SPARQL sources; a local file path or endpoint URL for
SPARQL sources.  TYPE is e.g. \"org\", \"csv\", \"tsv\", \"ttl\",
\"rq\".  FILE-MTIME is stored as `last_modified' (0.0 if nil).

The whole operation runs inside a single transaction: the
existing source row (and, by cascade, its entities / attributes /
prefixes) is deleted first, then the new source row is written
and DATA is ingested.  Returns the number of entity rows written."
  (let ((ds       (elot-db--normalize-ds data-source))
        (mtime    (or file-mtime 0.0))
        (now      (float-time))
        (n        0)
        (ok       nil))
    (sqlite-transaction elot-db)
    (unwind-protect
        (progn
          (sqlite-execute
           elot-db
           "DELETE FROM sources WHERE source = ? AND data_source = ?"
           (list source ds))
          (sqlite-execute
           elot-db
           "INSERT INTO sources (source, data_source, type, last_modified, last_updated)
             VALUES (?, ?, ?, ?, ?)"
           (list source ds type mtime now))
          ;; Coalesce DATA by id so that multiple triples for the
          ;; same id (e.g. one row per language-tagged rdfs:label
          ;; coming out of a ROBOT SPARQL projection) don't trigger
          ;; the `INSERT OR REPLACE INTO entities' cascade that
          ;; would wipe attribute rows written by earlier
          ;; iterations.  First-seen label wins; plists are
          ;; concatenated in order; :kind is taken from the first
          ;; occurrence.
          (let ((merged (make-hash-table :test 'equal))
                (order  nil))
            (dolist (row data)
              (let* ((id    (nth 0 row))
                     (label (nth 1 row))
                     (plist (nth 2 row))
                     (prev  (gethash id merged)))
                (if prev
                    (let ((prev-label (nth 0 prev))
                          (prev-plist (nth 1 prev)))
                      (puthash id
                               (list (if (and (or (null prev-label)
                                                  (string-empty-p
                                                   prev-label))
                                              label)
                                         label
                                       prev-label)
                                     (append prev-plist plist))
                               merged))
                  (push id order)
                  (puthash id (list label plist) merged))))
            (dolist (id (nreverse order))
              (let* ((cell  (gethash id merged))
                     (label (nth 0 cell))
                     (plist (nth 1 cell))
                     (kind  (or (plist-get plist :kind) "unknown"))
                     ;; When the plist carries `rdfs:label' rows
                     ;; with language info (emitted by
                     ;; multi-row-per-id ingestors like the ROBOT
                     ;; TTL path), derive the denormalised
                     ;; `entities.label' via the language picker
                     ;; instead of taking the arbitrary first-seen
                     ;; row-cell.  This makes `entities.label'
                     ;; deterministic and honours
                     ;; `elot-preferred-languages' at ingest time.
                     (label-variants
                      (cl-loop for (prop val) on plist by #'cddr
                               when (and (stringp prop)
                                         (equal prop "rdfs:label"))
                               collect (if (consp val)
                                           (cons (nth 0 val)
                                                 (or (nth 1 val) ""))
                                         (cons val ""))))
                     (picked (and label-variants
                                  (elot-db--pick-value-by-lang
                                   label-variants)))
                     (label (or picked label)))
                (sqlite-execute
                 elot-db
                 "INSERT OR REPLACE INTO entities (id, label, source, data_source, kind)
                   VALUES (?, ?, ?, ?, ?)"
                 (list id label source ds kind))
                (cl-loop for (prop val) on plist by #'cddr
                         unless (keywordp prop) do
                         (let ((v (if (consp val) (nth 0 val) val))
                               (lang (if (consp val) (or (nth 1 val) "") "")))
                           (sqlite-execute
                            elot-db
                            "INSERT INTO attributes (id, source, data_source, prop, value, lang)
                            VALUES (?, ?, ?, ?, ?, ?)"
                            (list id source ds prop v lang))))
                (cl-incf n))))
          (sqlite-commit elot-db)
          (setq ok t))
      (unless ok
        (ignore-errors (sqlite-rollback elot-db))))
    n))


;;;; -------------------------------------------------------------------
;;;; Priority-aware read path
;;;; -------------------------------------------------------------------

(defun elot-db--active-or-default (active-sources)
  "Return ACTIVE-SOURCES or, if nil, `elot-active-label-sources'."
  (or active-sources elot-active-label-sources))

(defun elot-db-get-label (id &optional active-sources)
  "Return label for ID, restricted and prioritised by ACTIVE-SOURCES.
ACTIVE-SOURCES defaults to the buffer-local `elot-active-label-sources'.
Returns nil if ACTIVE-SOURCES is empty or no active source has ID.

When the winning source has `rdfs:label' rows in the
`attributes' table, they are consulted first and resolved via
`elot-db--pick-value-by-lang' against `elot-preferred-languages'.
Otherwise the denormalised `entities.label' column is returned
\(the default ingest-time label, which itself reflects the default
language policy when the ingestor emitted multiple variants)."
  (let ((sources (elot-db--active-or-default active-sources)))
    (when sources
      (cl-loop
       for entry in sources
       for src = (nth 0 entry)
       for ds  = (elot-db--normalize-ds (nth 1 entry))
       for ent-row = (sqlite-select
                      elot-db
                      "SELECT label FROM entities
                        WHERE id = ? AND source = ? AND data_source = ?
                        LIMIT 1"
                      (list id src ds))
       when ent-row return
       (let ((attr-rows (sqlite-select
                         elot-db
                         "SELECT value, lang FROM attributes
                           WHERE id = ? AND prop = 'rdfs:label'
                             AND source = ? AND data_source = ?"
                         (list id src ds))))
         (if attr-rows
             (elot-db--pick-value-by-lang
              (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r))) attr-rows))
           (caar ent-row)))))))

(defun elot-db--pick-value-by-lang (rows &optional prefs)
  "Pick one VALUE from ROWS, a list of (VALUE . LANG) cons cells.
Delegates to `elot-db--select-by-language', which consults PREFS
\(or, when nil, `elot-preferred-languages' via
`elot-db--effective-language-prefs').  Returns the VALUE string
\(unwrapped from the (VALUE . LANG) cons) of the winning row, or
nil when ROWS is empty."
  (car (elot-db--select-by-language rows prefs)))

(defun elot-db-get-attr (id prop &optional active-sources)
  "Return the value of PROP for ID, priority-resolved over ACTIVE-SOURCES.
First active source that has a matching attribute row wins.  If a
given source has multiple attribute rows for the same (ID, PROP),
one is chosen via `elot-db--pick-value-by-lang', which consults
`elot-preferred-languages'."
  (let ((sources (elot-db--active-or-default active-sources)))
    (when sources
      (cl-loop
       for entry in sources
       for src = (nth 0 entry)
       for ds  = (elot-db--normalize-ds (nth 1 entry))
       for rows = (sqlite-select
                   elot-db
                   "SELECT value, lang FROM attributes
                     WHERE id = ? AND prop = ?
                       AND source = ? AND data_source = ?"
                   (list id prop src ds))
       when rows return
       (elot-db--pick-value-by-lang
        (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r))) rows))))))

(defun elot-db-source-entity-count (source &optional data-source)
  "Return the number of `entities' rows for (SOURCE, DATA-SOURCE).
DATA-SOURCE defaults to the empty-string sentinel.  Used by the
source-listing UI."
  (caar (sqlite-select
         elot-db
         "SELECT COUNT(*) FROM entities
           WHERE source = ? AND data_source = ?"
         (list source (elot-db--normalize-ds data-source)))))

(defun elot-db-get-all-attrs (id &optional active-sources)
  "Return a flat plist of all attributes for ID from ACTIVE-SOURCES.
Sources are consulted in priority order; within each source,
attribute rows are collected in insertion order.  The first source
that has any attribute row for ID wins and its rows are returned;
lower-priority sources are not merged (v1 policy: a single source
owns an entity's attribute view).  Returns nil when no active
source has rows for ID.

The returned plist carries an additional `:source-origin'
keyword whose value is the cons cell
\(SOURCE . DATA-SOURCE) identifying the winning source.  Callers
that look up only string-keyed attributes (using `string=' as
predicate, as ELOT's formatters do) are unaffected; the extra
entry is a no-op for them.

For each prop, all rows in the winning source are grouped and
`elot-db--pick-value-by-lang' collapses them to a
single value by consulting `elot-preferred-languages'.  For data
with no language-tagged rows, behaviour is identical to the
pre-widening path (the lone untagged row wins trivially)."
  (let ((sources (elot-db--active-or-default active-sources)))
    (when sources
      (cl-loop
       for entry in sources
       for src = (nth 0 entry)
       for ds  = (elot-db--normalize-ds (nth 1 entry))
       for rows = (sqlite-select
                   elot-db
                   "SELECT prop, value, lang FROM attributes
                     WHERE id = ? AND source = ? AND data_source = ?"
                   (list id src ds))
       when rows return
       (let ((by-prop nil))
         ;; Preserve first-seen prop order while grouping.
         (dolist (row rows)
           (let* ((p (nth 0 row))
                  (v (nth 1 row))
                  (l (nth 2 row))
                  (cell (assoc p by-prop)))
             (if cell
                 (setcdr cell (append (cdr cell) (list (cons v l))))
               (push (cons p (list (cons v l))) by-prop))))
         (setq by-prop (nreverse by-prop))
         (append
          (cl-loop for (p . vls) in by-prop
                   append (list p (elot-db--pick-value-by-lang vls)))
          (list :source-origin (cons src ds))))))))


;;;; -------------------------------------------------------------------
;;;; DB-driven font-lock support (all-active-ids)
;;;; -------------------------------------------------------------------

(defun elot-db-all-active-ids (&optional active-sources include-curies)
  "Return a deduped list of `entities.id' strings across ACTIVE-SOURCES.
ACTIVE-SOURCES defaults to the buffer-local `elot-active-label-sources'.
Order is not significant; the result is suitable as input to
`regexp-opt' for building a DB-driven font-lock matcher in
`elot-global-label-display-mode'.  Returns nil if no
active sources are set or none contain any entities.

When INCLUDE-CURIES is non-nil, each id that looks like a full
URI is additionally contracted via `elot-db-contract-uri'
against the active sources' prefix tables + globals, and every
resulting CURIE form is added to the output.  This is what lets
`elot-global-label-display-mode' match CURIE-form tokens in
buffers whose stored ids are full IRIs.  IRI forms
come first in the returned list for deterministic fallback
behaviour; duplicates are removed with `equal' semantics."
  (let ((sources (elot-db--active-or-default active-sources)))
    (when sources
      (let ((seen (make-hash-table :test 'equal))
            (out nil))
        (dolist (entry sources)
          (let* ((src (nth 0 entry))
                 (ds  (elot-db--normalize-ds (nth 1 entry)))
                 (rows (sqlite-select
                        elot-db
                        "SELECT DISTINCT id FROM entities
                          WHERE source = ? AND data_source = ?"
                        (list src ds))))
            (dolist (row rows)
              (let ((id (car row)))
                (unless (gethash id seen)
                  (puthash id t seen)
                  (push id out))))))
        ;; Second pass: augment with CURIE contractions.
        (when include-curies
          (let ((iris (reverse out)))  ; insertion order (oldest first)
            (dolist (id iris)
              (when (elot-db--looks-like-uri-p id)
                (dolist (curie (elot-db-contract-uri id sources))
                  (unless (gethash curie seen)
                    (puthash curie t seen)
                    (push curie out)))))))
        (nreverse out)))))

;;;; -------------------------------------------------------------------
;;;; DB-driven label collection for elot-label-lookup
;;;; -------------------------------------------------------------------

(defun elot-db-all-active-labels (&optional active-sources)
  "Return a hashtable mapping label -> list-of-ids over ACTIVE-SOURCES.
ACTIVE-SOURCES defaults to the buffer-local `elot-active-label-sources'.

Sources are consulted in priority order.  Within a single winning
source, /all/ ids carrying the label are preserved in the returned
list (industrial-asset files routinely have 20+ identifiers
sharing a single label like \"thimble B\").  When two
sources share a label, the higher-priority source wins - all of
its ids are kept, and the lower-priority source's ids for that
label are ignored silently.  First-source-wins, consistent with
`elot-db-get-label'.  Returns an empty hashtable (never nil) when
no active sources are set or none have labelled entities.

When a source carries `rdfs:label' attribute rows for an id,
`elot-db--pick-value-by-lang' (against
`elot-preferred-languages') chooses the label key.  Ids without
such rows fall back to the denormalised `entities.label' column.

Each value is a list of `entities.id' strings, ordered by SQL
return order within the winning source.  Use `elot-db-ids-for-label'
for a direct lookup."
  (let ((sources (elot-db--active-or-default active-sources))
        (ht (make-hash-table :test 'equal))
        ;; Track which labels have been claimed by a source already,
        ;; so later (lower-priority) sources don't append to them.
        (claimed (make-hash-table :test 'equal)))
    (when sources
      (dolist (entry sources)
        (let* ((src  (nth 0 entry))
               (ds   (elot-db--normalize-ds (nth 1 entry)))
               (ent-rows (sqlite-select
                          elot-db
                          "SELECT DISTINCT id, label FROM entities
                            WHERE source = ? AND data_source = ?
                              AND label IS NOT NULL"
                          (list src ds)))
               ;; Fetch all rdfs:label rows for this source in one pass;
               ;; group by id.
               (lbl-rows (sqlite-select
                          elot-db
                          "SELECT id, value, lang FROM attributes
                            WHERE source = ? AND data_source = ?
                              AND prop = 'rdfs:label'"
                          (list src ds)))
               (by-id (make-hash-table :test 'equal))
               (this-pass (make-hash-table :test 'equal)))
          (dolist (row lbl-rows)
            (let ((id (nth 0 row)))
              (puthash id
                       (append (gethash id by-id)
                               (list (cons (nth 1 row) (nth 2 row))))
                       by-id)))
          (dolist (row ent-rows)
            (let* ((id    (nth 0 row))
                   (ent-l (nth 1 row))
                   (variants (gethash id by-id))
                   (label (if variants
                              (elot-db--pick-value-by-lang variants)
                            ent-l)))
              (when (and label (not (gethash label claimed)))
                (puthash label
                         (append (gethash label ht) (list id))
                         ht)
                (puthash label t this-pass))))
          ;; Freeze everything this source claimed.
          (maphash (lambda (l _) (puthash l t claimed)) this-pass))))
    ht))

(defun elot-db-ids-for-label (label &optional active-sources)
  "Return the list of ids carrying LABEL in the winning active source.
ACTIVE-SOURCES defaults to the buffer-local `elot-active-label-sources'.
Convenience wrapper around `elot-db-all-active-labels'.  Returns
nil when LABEL is unknown."
  (gethash label (elot-db-all-active-labels active-sources)))

(defun elot-db-label-variants (id &optional active-sources)
  "Return the list of (VALUE . LANG) `rdfs:label' rows for ID.
ACTIVE-SOURCES defaults to the buffer-local
`elot-active-label-sources'.  The first source (in priority
order) that has any `rdfs:label' attribute rows for ID wins; all
of its variants are returned.  Returns nil when ID has no such
rows in any active source.

Used by the completion-display disambiguator to surface `@LANG'
when a singleton label has multiple language variants in the
winning source."
  (let ((sources (elot-db--active-or-default active-sources)))
    (when sources
      (cl-loop
       for entry in sources
       for src = (nth 0 entry)
       for ds  = (elot-db--normalize-ds (nth 1 entry))
       for rows = (sqlite-select
                   elot-db
                   "SELECT value, lang FROM attributes
                     WHERE id = ? AND prop = 'rdfs:label'
                       AND source = ? AND data_source = ?"
                   (list id src ds))
       when rows return
       (mapcar (lambda (r) (cons (nth 0 r) (nth 1 r))) rows)))))

;;;; -------------------------------------------------------------------
;;;; Milestone 6 Step 6.1: read-only SQL gate
;;;; -------------------------------------------------------------------

(defun elot-db--sql-strip-comments (sql)
  "Return SQL with SQL line and block comments removed.

Strips `-- ... EOL' line comments and `/* ... */' block
comments.  Text inside single-quoted string literals is
preserved verbatim, so a comment marker appearing inside a
literal does not start a comment; doubled single quotes (`''')
inside a literal are treated as an escaped quote and do not end
the literal.

Used by `elot-db--sql-first-token' so the read-only gate
recognises SELECT / WITH even when preceded by comments, and
cannot be fooled by a mutating keyword that occurs inside a
comment."
  (with-temp-buffer
    (insert sql)
    (goto-char (point-min))
    (let ((in-string nil))
      (while (not (eobp))
        (cond
         ((and (not in-string) (eq (char-after) ?\'))
          (setq in-string t)
          (forward-char 1))
         ((and in-string (eq (char-after) ?\'))
          (if (eq (char-after (1+ (point))) ?\')
              (forward-char 2)
            (setq in-string nil)
            (forward-char 1)))
         (in-string
          (forward-char 1))
         ((looking-at "--")
          (delete-region (point) (line-end-position)))
         ((looking-at "/\\*")
          (let ((start (point)))
            (if (re-search-forward "\\*/" nil t)
                (delete-region start (point))
              (delete-region start (point-max)))))
         (t (forward-char 1)))))
    (buffer-string)))

(defun elot-db--sql-first-token (sql)
  "Return the first SQL keyword in SQL (uppercased), or nil.

Comments are stripped via `elot-db--sql-strip-comments' first;
the result is then scanned past leading whitespace for a run of
ASCII letters, which is returned uppercased.  Returns nil when
SQL is empty or starts with a non-letter token."
  (let ((stripped (elot-db--sql-strip-comments sql)))
    (when (string-match "\\`[ \t\n\r]*\\([A-Za-z]+\\)" stripped)
      (upcase (match-string 1 stripped)))))

(defun elot-db-execute-readonly (sql &optional params return-type)
  "Execute SQL as a read-only query and return its rows.

SQL must be a SELECT or WITH ... SELECT statement (case
insensitive; SQL line/block comments are allowed before the
keyword).  Anything else -- INSERT, UPDATE, DELETE, CREATE,
DROP, ALTER, ATTACH, DETACH, PRAGMA, REPLACE, VACUUM, ANALYZE,
REINDEX, BEGIN, COMMIT, ROLLBACK, SAVEPOINT, RELEASE -- signals
`user-error' without touching the database.

For the duration of the call the connection's
`PRAGMA query_only' is set to 1 (and unconditionally restored
to 0 in cleanup), so even a SELECT carrying an embedded
expression that attempts a write would be refused at the
SQLite level.

PARAMS, when non-nil, is the bind-parameter list passed to
`sqlite-select'.  RETURN-TYPE, when non-nil, is forwarded to
`sqlite-select' (use the symbol `full' to also receive the
column-name header alongside the data rows).  Returns the
result rows exactly as `sqlite-select' would.  Signals
`user-error' when there is no open connection (call
`elot-db-init' first).

Designed as the single read-only SQL gate used by the
Milestone 6 gptel tools (`elot_db_query', `elot_db_search_label',
`elot_db_borrow_term')."
  (unless (and (stringp sql) (not (string-empty-p (string-trim sql))))
    (user-error "elot-db: empty SQL"))
  (let ((tok (elot-db--sql-first-token sql)))
    (unless (member tok '("SELECT" "WITH"))
      (user-error
       "elot-db: read-only gate refuses non-SELECT statement \
(first token: %s)"
       (or tok "<none>"))))
  (unless (and elot-db (sqlitep elot-db))
    (user-error "elot-db: no open connection (call `elot-db-init')"))
  (unwind-protect
      (progn
        (sqlite-pragma elot-db "query_only = 1")
        (cond
         ((and params return-type)
          (sqlite-select elot-db sql params return-type))
         (params
          (sqlite-select elot-db sql params))
         (return-type
          (sqlite-select elot-db sql nil return-type))
         (t
          (sqlite-select elot-db sql))))
    (ignore-errors (sqlite-pragma elot-db "query_only = 0"))))


;;;; -------------------------------------------------------------------
;;;; Milestone 6 Step 6.5: cross-ontology identifier reuse
;;;; -------------------------------------------------------------------

(defconst elot-db--search-kind-map
  '(("class"               . "owl:Class")
    ("object-property"     . "owl:ObjectProperty")
    ("data-property"       . "owl:DatatypeProperty")
    ("annotation-property" . "owl:AnnotationProperty")
    ("individual"          . "owl:NamedIndividual")
    ("datatype"            . "rdfs:Datatype")
    ("ontology"            . "owl:Ontology"))
  "Mapping from human-friendly entity kinds to RDF-type CURIEs.
Used by `elot-db-search-entities' to filter results by what the
entity actually IS in the ontology (its `rdf:type'), as opposed
to the `entities.kind' column which only records `uri'/`curie'/
`unknown' (the identifier *form*, not the resource *category*).")

(defun elot-db--search-kind-curie (kind)
  "Resolve KIND (a human-friendly string) to its RDF-type CURIE.
Returns nil when KIND is nil or empty.  Signals `user-error'
when KIND is non-empty but unknown."
  (cond
   ((or (null kind) (and (stringp kind) (string-empty-p kind))) nil)
   ((not (stringp kind))
    (user-error "elot-db: kind must be a string"))
   (t (or (cdr (assoc (downcase kind) elot-db--search-kind-map))
          (user-error
           "elot-db: unknown kind %S (expected one of: %s)"
           kind
           (mapconcat #'car elot-db--search-kind-map ", "))))))

(defconst elot-db--search-curie-shape-regexp
  "\\`[A-Za-z_][A-Za-z0-9_-]*:.+\\'"
  "Regexp matching a CURIE-shaped QUERY (`prefix:localname').
Used by `elot-db-search-entities' to decide when to issue the
M11.1 cross-prefix local-name fallback SELECT.")

(defun elot-db--search-curie-shaped-p (query)
  "Non-nil when QUERY looks like a CURIE (`prefix:local').
Wildcard characters (`%') in QUERY suppress the fallback --
the caller is doing explicit LIKE matching, not asking about
a single CURIE."
  (and (stringp query)
       (not (string-match-p "%" query))
       (string-match-p elot-db--search-curie-shape-regexp query)))

(defun elot-db--search-curie-localname (query)
  "Return the local-name part of a CURIE-shaped QUERY, or nil."
  (when (elot-db--search-curie-shaped-p query)
    (substring query (1+ (string-match ":" query)))))

(defun elot-db-search-entities (query &optional limit kind source lang exact-only)
  "Search every registered source for entities matching QUERY.

QUERY is a non-empty string matched (case-insensitively, as a
substring) against `entities.label' and `entities.id'.  Use
`%' as a wildcard if you want explicit LIKE semantics; bare
QUERY is wrapped in `%QUERY%' automatically when it contains
no wildcards.

LIMIT caps the number of rows returned (default 50; nil = 50;
0 or negative = no cap).  KIND, when non-nil, restricts the
result to entities asserted with the corresponding `rdf:type'
(class / object-property / data-property / annotation-property
/ individual / datatype / ontology -- see
`elot-db--search-kind-map').  SOURCE, when non-nil, restricts
to a single registered source.  LANG, when non-nil, requires
the entity to carry at least one `rdfs:label' row with that
language tag.

EXACT-ONLY, when non-nil, suppresses the M11.1 cross-prefix
local-name fallback (described below).  Use this when strict
equality semantics are required (e.g. `elot-db-entity-citation'
already matches on the id column exactly and does not want
imperfect alternatives).

Returns a list of rows
  (id label rdf-type ontology-iri source data-source via)
where RDF-TYPE is the entity's asserted `rdf:type' CURIE (nil
when unknown), ONTOLOGY-IRI is the id of the ontology
declaration in the same source -- the citation target for an
`rdfs:isDefinedBy' back-pointer -- and VIA is a marker
indicating how the row was found:
  `exact'           id column matched the (un-wrapped) query exactly.
  `label-substring' label or id matched as a LIKE substring.
  `local-name'      cross-prefix fallback (M11.1): QUERY was
                    CURIE-shaped, the direct prefix:local lookup
                    missed, but an entity in some source carries
                    the same local-name under a different prefix.
The first two come from the primary SELECT; `local-name'
appears only when QUERY looks like `prefix:local' and
EXACT-ONLY is nil.  Rows found by both passes are reported
once with their primary-pass VIA value (no duplication).

Read-only over the entire database; runs through the M6.1
`elot-db-execute-readonly' gate.  Designed as the sole DB
access path for `elot_db_search_label' (and, in a follow-up,
`elot_db_borrow_term')."
  (unless (and (stringp query) (not (string-empty-p query)))
    (user-error "elot-db: query must be a non-empty string"))
  (let* ((q-bare query)
         (q     (if (string-match-p "%" query)
                    query
                  (concat "%" query "%")))
         (lim   (cond
                 ((null limit) 50)
                 ((and (integerp limit) (<= limit 0)) nil)
                 ((integerp limit) limit)
                 (t (user-error
                     "elot-db: limit must be an integer"))))
         (kind-curie (elot-db--search-kind-curie kind))
         (extra-clauses nil)
         (extra-params  nil))
    (when source
      (push "AND e.source = ?" extra-clauses)
      (setq extra-params (append extra-params (list source))))
    (when kind-curie
      (push "AND EXISTS (SELECT 1 FROM attributes ak
                          WHERE ak.id = e.id
                            AND ak.source = e.source
                            AND ak.data_source = e.data_source
                            AND ak.prop = 'rdf:type'
                            AND ak.value = ?)" extra-clauses)
      (setq extra-params (append extra-params (list kind-curie))))
    (when lang
      (push "AND EXISTS (SELECT 1 FROM attributes al
                          WHERE al.id = e.id
                            AND al.source = e.source
                            AND al.data_source = e.data_source
                            AND al.prop = 'rdfs:label'
                            AND al.lang = ?)" extra-clauses)
      (setq extra-params (append extra-params (list lang))))
    (let* ((extra (mapconcat #'identity (nreverse extra-clauses) "\n   "))
           (select-cols "SELECT
   e.id,
   e.label,
   (SELECT a1.value FROM attributes a1
     WHERE a1.id = e.id AND a1.source = e.source
       AND a1.data_source = e.data_source
       AND a1.prop = 'rdf:type'
     LIMIT 1) AS rdf_type,
   (SELECT a2.id FROM attributes a2
     WHERE a2.source = e.source AND a2.data_source = e.data_source
       AND a2.prop = 'rdf:type' AND a2.value = 'owl:Ontology'
     LIMIT 1) AS ontology_iri,
   e.source,
   e.data_source,
   CASE WHEN LOWER(e.id) = LOWER(?) THEN 'exact'
        ELSE 'label-substring'
   END AS via
 FROM entities e
")
           (order " ORDER BY e.source, e.label, e.id")
           (limit-clause (when lim (format " LIMIT %d" lim)))
           ;; Primary pass: substring on label OR id.
           (primary-sql
            (concat select-cols
                    " WHERE (LOWER(e.label) LIKE LOWER(?) "
                    "        OR LOWER(e.id) LIKE LOWER(?)) "
                    extra
                    order limit-clause))
           (primary-params
            (append (list q-bare q q) extra-params))
           (primary (elot-db-execute-readonly primary-sql primary-params))
           (localname (and (not exact-only)
                           (elot-db--search-curie-localname q-bare))))
      (if (not localname)
          primary
        ;; M11.1 fallback: cross-prefix local-name match.  Use the
        ;; CURIE local-name to match either the id column's
        ;; local-name part (after the last `:') or the label column
        ;; exactly (case-insensitive).  Rows already returned by the
        ;; primary pass are dropped to avoid duplication.
        (let* ((local-q localname)
               (fallback-sql
                (concat select-cols
                        " WHERE (LOWER(substr(e.id, instr(e.id, ':')+1))
                                 = LOWER(?)
                                 OR LOWER(e.label) = LOWER(?)) "
                        extra
                        order limit-clause))
               (fallback-params
                (append (list q-bare local-q local-q) extra-params))
               (fallback (elot-db-execute-readonly
                          fallback-sql fallback-params))
               ;; Build a set of primary-pass keys so we can dedupe.
               (primary-keys
                (mapcar (lambda (r)
                          (cons (nth 0 r)
                                (cons (nth 4 r) (nth 5 r))))
                        primary))
               (extras
                (cl-remove-if
                 (lambda (r)
                   (member (cons (nth 0 r)
                                 (cons (nth 4 r) (nth 5 r)))
                           primary-keys))
                 fallback)))
          ;; Tag every fallback-only row as `local-name'.  In SQL the
          ;; CASE would have classified them as `label-substring' or
          ;; `exact' on a coincidence -- override here so the marker
          ;; actually reflects how the row was found.
          (setq extras
                (mapcar (lambda (r)
                          (append (butlast r) (list "local-name")))
                        extras))
          (let ((merged (append primary extras)))
            (if lim
                (cl-subseq merged 0 (min lim (length merged)))
              merged)))))))


;;; --------------------------------------------------------------------
;;; Entity citation -- M6.5 second half (reuse-before-mint)
;;; --------------------------------------------------------------------

(defun elot-db-entity-citation (token)
  "Return citation metadata for TOKEN, or nil when unknown.

TOKEN is an entity id as stored in `entities.id' -- typically
a CURIE like `ex:dog' or a full IRI.  Angle brackets around an
IRI are stripped.  The match is exact on `entities.id'.

Returns a plist with keys
  :id             -- the entity id (as stored)
  :label          -- the entity's display label (from `entities.label')
  :label-lang     -- best `rdfs:label' lang tag (string, or nil)
  :definition     -- `skos:definition' value, if cached (or nil)
  :rdf-type       -- the entity's asserted `rdf:type' CURIE (or nil)
  :source         -- the source name the entity lives in
  :data-source    -- the data_source string
  :ontology-iri   -- id of the same source's owl:Ontology declaration
                     (the citation target for `rdfs:isDefinedBy'),
                     or nil if the source has no such declaration
  :ontology-title -- `dcterms:title' of the ontology id (or nil)

Returns nil when TOKEN does not name a known entity.  Read-only;
runs through the M6.1 `elot-db-execute-readonly' gate.  This is
the sole DB access path for `elot_db_borrow_term'."
  (unless (and (stringp token) (not (string-empty-p token)))
    (user-error "elot-db: token must be a non-empty string"))
  (let* ((id (if (and (string-prefix-p "<" token)
                      (string-suffix-p ">" token)
                      (> (length token) 2))
                 (substring token 1 -1)
               token))
         (rows (elot-db-execute-readonly
                "SELECT id, label, source, data_source
                   FROM entities WHERE id = ? LIMIT 1"
                (list id))))
    (when rows
      (let* ((row    (car rows))
             (id*    (nth 0 row))
             (label  (nth 1 row))
             (source (nth 2 row))
             (data   (nth 3 row))
             (one (lambda (sql params)
                    (caar (elot-db-execute-readonly sql params))))
             (rdf-type
              (funcall one
                       "SELECT value FROM attributes
                          WHERE id = ? AND source = ? AND data_source = ?
                            AND prop = 'rdf:type' LIMIT 1"
                       (list id* source data)))
             (label-lang
              (funcall one
                       "SELECT lang FROM attributes
                          WHERE id = ? AND source = ? AND data_source = ?
                            AND prop = 'rdfs:label'
                            AND lang IS NOT NULL AND lang != ''
                          LIMIT 1"
                       (list id* source data)))
             (definition
              (funcall one
                       "SELECT value FROM attributes
                          WHERE id = ? AND source = ? AND data_source = ?
                            AND prop = 'skos:definition' LIMIT 1"
                       (list id* source data)))
             (ont-iri
              (funcall one
                       "SELECT id FROM attributes
                          WHERE source = ? AND data_source = ?
                            AND prop = 'rdf:type'
                            AND value = 'owl:Ontology' LIMIT 1"
                       (list source data)))
             (ont-title
              (and ont-iri
                   (funcall one
                            "SELECT value FROM attributes
                               WHERE id = ? AND source = ? AND data_source = ?
                                 AND prop = 'dcterms:title' LIMIT 1"
                            (list ont-iri source data)))))
        (list :id            id*
              :label         label
              :label-lang    label-lang
              :definition    definition
              :rdf-type      rdf-type
              :source        source
              :data-source   data
              :ontology-iri  ont-iri
              :ontology-title ont-title)))))


(provide 'elot-db)

;;; elot-db.el ends here
