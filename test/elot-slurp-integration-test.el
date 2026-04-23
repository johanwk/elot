;;; elot-slurp-integration-test.el --- Tests for Step 1.6  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.6: integration between `elot-slurp-to-vars' (local ELOT buffer
;; parsing) and the ELOT label DB.
;;
;; - Local HT still takes precedence over DB in `elot-codelist-id-label'.
;; - DB fallback returns a label when the id is absent locally.
;; - `elot-slurp-to-vars' syncs the current buffer's slurp into the DB
;;   keyed by `buffer-file-name'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-tangle)
(require 'elot-label-display)

(defconst elot-si-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-si-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-si-test--dir))

(defvar elot-si-test--tmp-db nil)
(defvar elot-si-test--tmp-files nil)

(defun elot-si-test--setup ()
  (setq elot-si-test--tmp-db  (make-temp-file "elot-si-" nil ".sqlite"))
  (setq elot-si-test--tmp-files nil)
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-si-test--tmp-db))

(defun elot-si-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-si-test--tmp-db (file-exists-p elot-si-test--tmp-db))
    (ignore-errors (delete-file elot-si-test--tmp-db)))
  (dolist (f elot-si-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f))))
  ;; Clear any buffer-local active-sources we may have set.
  (setq-default elot-active-label-sources nil))

(defun elot-si-test--copy-fixture (name)
  (let* ((src (elot-si-test--fixture name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-si-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-si-test--tmp-files)
    tmp))

(defmacro elot-si-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-si-test--setup) ,@body)
     (elot-si-test--teardown)))

;;; ---------------------------------------------------------------------------
;;; Tier 1: two-tier lookup semantics on `elot-codelist-id-label' /
;;;         `elot-attriblist-label-value'.

(ert-deftest test-local-slurp-takes-precedence ()
  "When the id is present in the local HT, its label wins over the DB."
  (elot-si-test--with-fresh
   ;; Register a DB source in which ex:Widget -> "DB-Widget".
   (let ((csv (elot-si-test--copy-fixture "labels.csv")))
     (elot-label-register-source csv)
     (elot-label-set-active-sources `((,csv))))
   ;; Populate local HTs so ex:Widget -> "Local-Widget".
   (let ((elot-codelist-ht  (make-hash-table :test 'equal))
         (elot-attriblist-ht (make-hash-table :test 'equal)))
     (puthash "ex:Widget" "Local-Widget" elot-codelist-ht)
     (should (equal "Local-Widget" (elot-codelist-id-label "ex:Widget"))))))

(ert-deftest test-db-fallback-when-local-missing ()
  "When the id is absent from local HT, DB lookup is consulted."
  (elot-si-test--with-fresh
   (let ((csv (elot-si-test--copy-fixture "labels.csv")))
     (elot-label-register-source csv)
     (elot-label-set-active-sources `((,csv)))
     ;; Local HTs exist but do not contain ex:Widget.
     (let ((elot-codelist-ht  (make-hash-table :test 'equal))
           (elot-attriblist-ht (make-hash-table :test 'equal)))
       (should (equal "Widget" (elot-codelist-id-label "ex:Widget")))
       ;; Nil local HTs also work (nothing to consult -> DB wins).
       (let ((elot-codelist-ht nil)
             (elot-attriblist-ht nil))
         (should (equal "Widget" (elot-codelist-id-label "ex:Widget"))))))))

(ert-deftest test-codelist-id-label-miss-returns-nil ()
  "Id in neither local HT nor DB returns nil, not an error."
  (elot-si-test--with-fresh
   (let ((elot-codelist-ht  (make-hash-table :test 'equal))
         (elot-attriblist-ht (make-hash-table :test 'equal)))
     (should (null (elot-codelist-id-label "no:such-id"))))))

;;; ---------------------------------------------------------------------------
;;; Tier 2: `elot-slurp-to-vars' writes to the DB when SQLite is available.

(ert-deftest test-slurp-to-vars-syncs-to-db ()
  "Calling `elot-slurp-to-vars' in an Org buffer writes entries to the DB."
  (skip-unless (fboundp 'sqlite-open))
  (elot-si-test--with-fresh
   (let* ((org (elot-si-test--copy-fixture "minimal-ontology.org")))
     ;; Point elot-db-path at our isolated DB (elot-db-init already opened it,
     ;; but `elot-slurp-to-vars' may call `elot-db-init' again; make sure it
     ;; re-opens the same file).
     (let ((elot-db-file elot-si-test--tmp-db))
       (with-current-buffer (find-file-noselect org)
         (unwind-protect
             (let ((bfn buffer-file-name))
               (delay-mode-hooks (org-mode))
               (elot-update-headline-hierarchy)
               (elot-slurp-to-vars)
               ;; Local HTs populated.
               (should (hash-table-p elot-codelist-ht))
               (should (> (hash-table-count elot-codelist-ht) 0))
               ;; DB now has a source row for this buffer's file.
               ;; Use `buffer-file-name' (what the sync actually wrote)
               ;; rather than `org' to avoid case/normalisation skew
               ;; between `make-temp-file' and `find-file-noselect'.
               (should (elot-db-source-exists-p bfn))
               ;; And the entries are findable through the DB path.
               (let ((elot-active-label-sources `((,bfn))))
                 (should (equal "Widget"
                                (elot-db-get-label "ex:Widget")))
                 (should (equal "Gadget"
                                (elot-db-get-label "ex:Gadget")))))
           (kill-buffer)))))))

(ert-deftest test-slurp-to-vars-no-buffer-file-no-sync ()
  "A buffer with no visited file must not attempt DB sync."
  (skip-unless (fboundp 'sqlite-open))
  (elot-si-test--with-fresh
   (let ((elot-db-file elot-si-test--tmp-db))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       ;; Empty hierarchy => empty slurp => no DB writes regardless.
       (setq elot-headline-hierarchy nil)
       (elot-slurp-to-vars)
       (should (null (elot-db-list-sources)))))))

(provide 'elot-slurp-integration-test)
;;; elot-slurp-integration-test.el ends here
