;;; elot-db-golden-test.el --- Golden round-trip tests vs the TS port  -*- lexical-binding: t; -*-

;;; Commentary:

;; Slice 3a (Step 2.2.5): byte-identical golden round-trip tests.
;; Both this Elisp test and the matching TypeScript test
;; (tools/elot-cli/src/tests/db/golden.test.ts) ingest the same
;; source fixtures via the language-aware writer, then produce a
;; canonical JSON dump (see elot-db-golden-gen.el resp.
;; goldenDump.ts).  The dumps must be byte-identical to the
;; checked-in goldens under test/fixtures/golden/.
;;
;; Bootstrap / regenerate the goldens:
;;
;;   ELOT_GOLDEN_REGEN=1 emacs --batch \
;;     -L elot-package -L test \
;;     -l ert -l test/elot-db-golden-test.el \
;;     -f ert-run-tests-batch-and-exit
;;
;; Then on the TS side:
;;
;;   ELOT_GOLDEN_REGEN=1 npx tsx tools/elot-cli/src/tests/db/golden.test.ts
;;
;; If both regen runs produce identical files, writer parity is
;; verified (and the two golden files should agree with a plain
;; `diff -u`).

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Dynamically locate sibling files / fixtures relative to this test.
(let* ((this-file (or load-file-name buffer-file-name))
       (test-dir  (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path test-dir))

(require 'elot-db)
(require 'elot-sources)
(require 'elot-db-golden-gen)

(defvar elot-db-golden-test--dir
  (file-name-directory
   (or load-file-name buffer-file-name
       (locate-library "elot-db-golden-test")))
  "Directory holding this test file (test/).")

(defvar elot-db-golden-test--fixtures
  (expand-file-name "fixtures/" elot-db-golden-test--dir))

(defvar elot-db-golden-test--golden-dir
  (expand-file-name "golden/" elot-db-golden-test--fixtures))

(defun elot-db-golden-test--regen-p ()
  (let ((v (getenv "ELOT_GOLDEN_REGEN")))
    (and v (not (string-empty-p v)) (not (equal v "0")))))

(defun elot-db-golden-test--ingest (input type source)
  "Open a fresh DB in a tempfile, ingest INPUT (TYPE) under SOURCE.
Return the dump string."
  (let* ((tmp (make-temp-file "elot-golden-" nil ".sqlite"))
         (db (progn (ignore-errors (delete-file tmp))
                    (elot-db-init tmp)))
         (raw (cond
               ((equal type "csv")  (elot-source-parse-csv input))
               ((equal type "tsv")  (elot-source-parse-tsv input))
               ((equal type "json") (elot-source-parse-json input))
               (t (error "elot-db-golden-test: unsupported TYPE %S" type))))
         (pair (elot-source--entries-and-prefixes raw))
         (entries (car pair))
         (prefixes (cdr pair)))
    (unwind-protect
        (progn
          (elot-db-update-source source nil type entries 0.0)
          (dolist (p prefixes)
            (elot-db-add-prefix source nil (car p) (cdr p)))
          (elot-db-golden-dump db))
      (ignore-errors (elot-db-close))
      (ignore-errors (delete-file tmp)))))

(defun elot-db-golden-test--check (name input type source golden)
  "Run one golden round-trip case."
  (let* ((got (elot-db-golden-test--ingest input type source))
         (golden-path (expand-file-name
                       golden elot-db-golden-test--golden-dir)))
    (cond
     ((elot-db-golden-test--regen-p)
      (with-temp-file golden-path
        (set-buffer-file-coding-system 'utf-8-unix)
        (insert got))
      (message "REGEN %s -> %s" name golden-path))
     (t
      (let ((want (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix))
                      (insert-file-contents golden-path))
                    (buffer-string))))
        (should (equal got want)))))))

(ert-deftest test-elot-db-golden-csv ()
  "labels.csv -> CSV writer -> canonical dump matches the golden."
  (elot-db-golden-test--check
   "labels.csv"
   (expand-file-name "labels.csv" elot-db-golden-test--fixtures)
   "csv" "labels"
   "labels-csv.golden.json"))

(ert-deftest test-elot-db-golden-nested-json ()
  "labels-nested.json -> JSON writer -> canonical dump matches the golden."
  (elot-db-golden-test--check
   "labels-nested.json"
   (expand-file-name "labels-nested.json" elot-db-golden-test--fixtures)
   "json" "labels"
   "labels-nested-json.golden.json"))

(provide 'elot-db-golden-test)

;;; elot-db-golden-test.el ends here
