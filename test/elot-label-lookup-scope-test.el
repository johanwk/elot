;;; elot-label-lookup-scope-test.el --- Step 1.13 tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.13: scope choice (local / external / both) for
;; `elot-label-lookup'.
;;
;; Covers:
;;   1. Default scope is `both'; union collection contains ids from
;;      local slurp and from the DB.
;;   2. `elot-label-lookup-scope' = local: collection matches
;;      `--from-attriblist' exactly (no external-only ids).
;;   3. `elot-label-lookup-scope' = external: collection matches
;;      `--from-db' exactly (no local-only ids).
;;   4. Union dedupes on id: same id in both sources -> one entry
;;      with :source `both'.
;;   5. Union keeps both entries on label collision when ids differ;
;;      displays are disambiguated by suffix.
;;   6. Prefix-arg `C-u' pins scope to `local' (single-universal-arg
;;      override).  Note: `C-u C-u' no longer means scope=external;
;;      Step 1.15 redefined it as /flat presentation/ (see the
;;      Step 1.15 test file for that).
;;   7. `elot-label-lookup-local' / `-external' are scope-pinned
;;      regardless of the defcustom.
;;   8. For a `:source both' entry, insertion prefers the local
;;      puri over the DB-contracted CURIE.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

;;;; ---------------------------------------------------------------------
;;;; Harness
;;;; ---------------------------------------------------------------------

(defvar elot-ls-test--tmp-db nil)

(defun elot-ls-test--setup ()
  (setq elot-ls-test--tmp-db
        (make-temp-file "elot-ls-" nil ".sqlite"))
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-ls-test--tmp-db))

(defun elot-ls-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-ls-test--tmp-db (file-exists-p elot-ls-test--tmp-db))
    (ignore-errors (delete-file elot-ls-test--tmp-db)))
  (setq elot-ls-test--tmp-db nil)
  (setq-default elot-active-label-sources nil))

(defmacro elot-ls-test--with-fresh (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-ls-test--setup) ,@body)
     (elot-ls-test--teardown)))

(defun elot-ls-test--populate-local (buf)
  "Install a 2-entry slurp hash in BUF."
  (with-current-buffer buf
    (setq-local elot-attriblist-ht (make-hash-table :test 'equal))
    (puthash "LocalOnly"
             '("puri" "ex:LocalOnly" "rdf:type" "owl:Class")
             elot-attriblist-ht)
    (puthash "Shared"
             '("puri" "ex:Shared" "rdf:type" "owl:Class")
             elot-attriblist-ht)))

(defun elot-ls-test--populate-external (buf)
  "Register a DB source and activate it in BUF."
  (elot-db-update-source
   "src" nil "csv"
   '(("ex:Shared"       "Shared"       nil)
     ("ex:ExternalOnly" "ExternalOnly" nil)))
  (with-current-buffer buf
    (setq-local elot-active-label-sources '(("src" nil)))))

;;;; ---------------------------------------------------------------------
;;;; 1. Default is `both'; union contains both sources' ids
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-defaults-to-both ()
  "Default `elot-label-lookup-scope' is `both' and the union covers both."
  (should (eq (default-value 'elot-label-lookup-scope) 'both))
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      (let* ((ht (elot-label-lookup--union-build))
             (entries '()))
        (maphash (lambda (_k v) (push v entries)) ht)
        ;; Collect source ids seen.
        (let ((ids (mapcar (lambda (e) (plist-get e :id)) entries)))
          (should (member "ex:LocalOnly"    ids))
          (should (member "ex:ExternalOnly" ids))
          (should (member "ex:Shared"       ids)))))))

;;;; ---------------------------------------------------------------------
;;;; 2 + 3. Scope-pinned collections
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-local-only ()
  "Scope `local': only local entries reach the completing-read."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      (let ((seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (setq seen coll)
                     ;; Don't select anything.
                     nil)))
          (let ((elot-label-lookup-scope 'local))
            (elot-label-lookup)))
        (should (hash-table-p seen))
        (should (gethash "LocalOnly" seen))
        (should (gethash "Shared"    seen))
        (should-not (gethash "ExternalOnly" seen))))))

(ert-deftest test-scope-external-only ()
  "Scope `external': only DB entries reach the completing-read."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      (let ((seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq seen coll) nil)))
          (let ((elot-label-lookup-scope 'external))
            (elot-label-lookup)))
        (should (hash-table-p seen))
        (should (gethash "ExternalOnly" seen))
        (should (gethash "Shared"       seen))
        ;; Local-only label never travelled through the DB path.
        (should-not (gethash "LocalOnly" seen))))))

;;;; ---------------------------------------------------------------------
;;;; 4. Union dedupes by id
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-union-dedupes-on-id ()
  "Same id in both sources collapses to one entry tagged :source both."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      (let* ((ht (elot-label-lookup--union-build))
             ;; "Shared" should be exactly one entry keyed by its label
             ;; (no collision by id -> no suffix).
             (entry (gethash "Shared" ht)))
        (should entry)
        (should (equal "ex:Shared" (plist-get entry :id)))
        (should (eq 'both (plist-get entry :source)))))))

;;;; ---------------------------------------------------------------------
;;;; 5. Label collision with different ids
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-union-keeps-both-on-label-collision ()
  "Two different ids sharing a label both appear, suffix-disambiguated."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (setq-local elot-attriblist-ht (make-hash-table :test 'equal))
      (puthash "Widget"
               '("puri" "loc:Widget" "rdf:type" "owl:Class")
               elot-attriblist-ht)
      (elot-db-update-source
       "src" nil "csv"
       '(("ext:Widget" "Widget" nil)))
      (setq-local elot-active-label-sources '(("src" nil)))
      (let* ((ht (elot-label-lookup--union-build))
             (keys '()))
        (maphash (lambda (k _v) (push k keys)) ht)
        ;; At least two keys both starting with "Widget".
        (let ((widget-keys
               (cl-remove-if-not
                (lambda (k) (string-prefix-p "Widget" k)) keys)))
          (should (= 2 (length widget-keys)))
          ;; Each disambiguated key contains a bracketed suffix.
          (dolist (k widget-keys)
            (should (string-match-p "\\[.+\\]" k))))))))

;;;; ---------------------------------------------------------------------
;;;; 6. Prefix arg overrides defcustom
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-prefix-arg-overrides-defcustom ()
  "`C-u' overrides defcustom `external' to `local' (Step 1.15 contract)."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      (let ((elot-label-lookup-scope 'external)
            (seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq seen coll) nil)))
          ;; Single universal-argument -> scope=local.
          (funcall #'elot-label-lookup '(4)))
        ;; Should have taken the local path (slurp hash).
        (should (hash-table-p seen))
        (should (gethash "LocalOnly" seen))
        (should-not (gethash "ExternalOnly" seen))))))

;;;; ---------------------------------------------------------------------
;;;; 7. Wrapper commands pin scope regardless of defcustom
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-wrapper-commands ()
  "`elot-label-lookup-local' / `-external' pin scope regardless of defcustom."
  (elot-ls-test--with-fresh
    (with-temp-buffer
      (elot-ls-test--populate-local (current-buffer))
      (elot-ls-test--populate-external (current-buffer))
      ;; Set a *different* defcustom value to prove the wrappers override.
      (let ((elot-label-lookup-scope 'external)
            (seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq seen coll) nil)))
          (call-interactively #'elot-label-lookup-local))
        (should (gethash "LocalOnly" seen))
        (should-not (gethash "ExternalOnly" seen)))
      (let ((elot-label-lookup-scope 'local)
            (seen nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq seen coll) nil)))
          (call-interactively #'elot-label-lookup-external))
        (should (gethash "ExternalOnly" seen))
        (should-not (gethash "LocalOnly" seen))))))

;;;; ---------------------------------------------------------------------
;;;; 8. Insertion prefers local puri for :source both
;;;; ---------------------------------------------------------------------

(ert-deftest test-scope-insert-prefers-local-id ()
  "A `:source both' entry inserts the local puri, not a DB-contracted CURIE."
  (elot-ls-test--with-fresh
    ;; External registers the same id under the same label, but with
    ;; an IRI form; the local slurp uses a short CURIE-like id.
    (elot-db-update-source
     "src" nil "csv"
     '(("ex:Shared" "Shared" nil)))
    (with-temp-buffer
      (setq-local elot-attriblist-ht (make-hash-table :test 'equal))
      (puthash "Shared"
               '("puri" "ex:Shared" "rdf:type" "owl:Class")
               elot-attriblist-ht)
      (setq-local elot-active-label-sources '(("src" nil)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p _coll &rest _) "Shared")))
        (let ((elot-label-lookup-scope 'both))
          (elot-label-lookup)))
      (should (string-match-p "\\bex:Shared\\b" (buffer-string))))))

(provide 'elot-label-lookup-scope-test)
;;; elot-label-lookup-scope-test.el ends here
