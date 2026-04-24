;;; elot-db-golden-gen.el --- Canonical JSON dump for ELOT DB  -*- lexical-binding: t; -*-

;;; Commentary:

;; Slice 3a (Step 2.2.5): produce a byte-identical canonical JSON
;; dump of an ELOT DB's data tables.  Used by `elot-db-golden-test'
;; to verify writer parity against the TypeScript port (see
;; tools/elot-cli/src/db/goldenDump.ts -- both implementations must
;; produce byte-identical output for the same fixture).
;;
;; Format:
;;   - Minified UTF-8 JSON, single line, trailing newline.
;;   - Keys alphabetically sorted at every object level.
;;   - Top-level shape:
;;       {"attributes":[...],"entities":[...],"prefixes":[...],"sources":[...]}
;;   - Excluded: sources.last_modified / last_updated (timestamps);
;;     global_prefixes (seed data, identical on both sides).
;;   - Row sort orders: see SELECT ORDER BY clauses below.
;;   - String escapes: \" \\ \n \r \t and \u00XX for other ASCII
;;     control chars; non-ASCII emitted as raw UTF-8 bytes.  Forward
;;     slashes are NOT escaped.

;;; Code:

(require 'sqlite)
(require 'cl-lib)

(defun elot-db-golden--escape-string (s)
  "Return JSON encoding of S using the canonical-dump escape rules."
  (let ((out (list "\"")))
    (dolist (ch (string-to-list s))
      (cond
       ((eq ch ?\")        (push "\\\"" out))
       ((eq ch ?\\)        (push "\\\\" out))
       ((eq ch ?\n)        (push "\\n"  out))
       ((eq ch ?\r)        (push "\\r"  out))
       ((eq ch ?\t)        (push "\\t"  out))
       ((< ch ?\s)         (push (format "\\u%04x" ch) out))
       (t                  (push (string ch) out))))
    (push "\"" out)
    (apply #'concat (nreverse out))))

(defun elot-db-golden--emit-value (v)
  "Emit V (a string or nil) per the canonical dump rules."
  (if (null v) "null" (elot-db-golden--escape-string v)))

(defun elot-db-golden--emit-row (row)
  "Emit ROW (an alist of (KEY . VALUE) pairs) with keys sorted."
  (let* ((sorted (sort (copy-sequence row)
                       (lambda (a b) (string< (car a) (car b)))))
         (parts (mapcar (lambda (kv)
                          (format "%s:%s"
                                  (elot-db-golden--escape-string (car kv))
                                  (elot-db-golden--emit-value (cdr kv))))
                        sorted)))
    (concat "{" (mapconcat #'identity parts ",") "}")))

(defun elot-db-golden--emit-array (rows)
  "Emit ROWS (list of alists) as a JSON array."
  (concat "[" (mapconcat #'elot-db-golden--emit-row rows ",") "]"))

(defun elot-db-golden--rows (db sql cols)
  "Run SQL on DB and project rows as alists keyed by COLS (list of strings)."
  (mapcar (lambda (row)
            (cl-loop for c in cols
                     for v in row
                     collect (cons c
                                   (cond
                                    ((null v) nil)
                                    ((stringp v) v)
                                    (t (format "%s" v))))))
          (sqlite-select db sql nil)))

(defun elot-db-golden-dump (db)
  "Return the canonical golden-dump string for DB."
  (let* ((sources
          (elot-db-golden--rows
           db
           "SELECT source, data_source, type
              FROM sources
             ORDER BY source, data_source"
           '("source" "data_source" "type")))
         (entities
          (elot-db-golden--rows
           db
           "SELECT id, label, source, data_source, kind
              FROM entities
             ORDER BY id, source, data_source"
           '("id" "label" "source" "data_source" "kind")))
         (attributes
          (elot-db-golden--rows
           db
           "SELECT id, source, data_source, prop, value, lang
              FROM attributes
             ORDER BY id, source, data_source, prop, lang, value"
           '("id" "source" "data_source" "prop" "value" "lang")))
         (prefixes
          (elot-db-golden--rows
           db
           "SELECT source, data_source, prefix, expansion
              FROM prefixes
             ORDER BY source, data_source, prefix"
           '("source" "data_source" "prefix" "expansion"))))
    (concat
     "{"
     "\"attributes\":" (elot-db-golden--emit-array attributes) ","
     "\"entities\":"   (elot-db-golden--emit-array entities)   ","
     "\"prefixes\":"   (elot-db-golden--emit-array prefixes)   ","
     "\"sources\":"    (elot-db-golden--emit-array sources)
     "}\n")))

(provide 'elot-db-golden-gen)

;;; elot-db-golden-gen.el ends here
