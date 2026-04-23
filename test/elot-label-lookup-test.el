;;; elot-label-lookup-test.el --- Tests for Step 1.12 Item A  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.12 Item A: `elot-label-lookup' generalisation.
;;
;; Covers:
;;   1. `elot-db-all-active-labels' with overlapping sources
;;      (highest-priority id wins per label).
;;   2. `elot-db-all-active-labels' with no active sources
;;      (returns an empty hash, does not error).
;;   3. Dispatch to the slurp/attriblist path when
;;      `elot-attriblist-ht' is populated.
;;   4. Dispatch to the DB path when only active sources are set.
;;   5. `user-error' when neither data source is available.
;;   6. DB path with a full-IRI id yields a CURIE on insertion.
;;   7. DB path with a CURIE id inserts the id verbatim.
;;   8. Autoload smoke-test: a fresh `emacs -Q' batch, with a
;;      freshly generated `elot-autoloads.el' on `load-path', has
;;      `elot-label-lookup' available as an autoload without
;;      loading any elot-* file eagerly, and can `fboundp' it
;;      after dispatch.

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

(defvar elot-ll-test--tmp-db nil)

(defun elot-ll-test--setup ()
  (setq elot-ll-test--tmp-db
        (make-temp-file "elot-ll-" nil ".sqlite"))
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-ll-test--tmp-db))

(defun elot-ll-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-ll-test--tmp-db
             (file-exists-p elot-ll-test--tmp-db))
    (ignore-errors (delete-file elot-ll-test--tmp-db)))
  (setq elot-ll-test--tmp-db nil)
  (setq-default elot-active-label-sources nil))

(defmacro elot-ll-test--with-fresh (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-ll-test--setup) ,@body)
     (elot-ll-test--teardown)))

;;;; ---------------------------------------------------------------------
;;;; 1 + 2: elot-db-all-active-labels
;;;; ---------------------------------------------------------------------

(ert-deftest test-elot-db-all-active-labels-basic ()
  "Two overlapping sources: highest-priority id wins per label."
  (elot-ll-test--with-fresh
    (elot-db-update-source "src-high" nil "csv"
                           '(("ex:foo" "SharedLabel" nil)
                             ("ex:high-only" "HighOnly"   nil)))
    (elot-db-update-source "src-low" nil "csv"
                           '(("ex:bar" "SharedLabel" nil)
                             ("ex:low-only" "LowOnly"   nil)))
    (let* ((active '(("src-high" nil) ("src-low" nil)))
           (ht (elot-db-all-active-labels active)))
      (should (hash-table-p ht))
      ;; High-priority source wins on collision.
      (should (equal "ex:foo"       (gethash "SharedLabel" ht)))
      (should (equal "ex:high-only" (gethash "HighOnly"    ht)))
      (should (equal "ex:low-only"  (gethash "LowOnly"     ht))))
    ;; Reverse priority: low wins for SharedLabel.
    (let* ((active '(("src-low" nil) ("src-high" nil)))
           (ht (elot-db-all-active-labels active)))
      (should (equal "ex:bar" (gethash "SharedLabel" ht))))))

(ert-deftest test-elot-db-all-active-labels-empty ()
  "No active sources: returns an empty hashtable, not nil, no error."
  (elot-ll-test--with-fresh
    (let ((ht (elot-db-all-active-labels nil)))
      (should (hash-table-p ht))
      (should (= 0 (hash-table-count ht))))))

;;;; ---------------------------------------------------------------------
;;;; 3 + 4 + 5: dispatcher behaviour
;;;; ---------------------------------------------------------------------

(ert-deftest test-label-lookup-dispatches-to-attriblist ()
  "With a populated `elot-attriblist-ht', the slurp path runs."
  (elot-ll-test--with-fresh
    (with-temp-buffer
      (setq-local elot-attriblist-ht (make-hash-table :test 'equal))
      (puthash "Widget" '("puri" "ex:Widget") elot-attriblist-ht)
      (let ((attriblist-called 0)
            (db-called         0))
        (cl-letf (((symbol-function 'elot-label-lookup--from-attriblist)
                   (lambda () (cl-incf attriblist-called)))
                  ((symbol-function 'elot-label-lookup--from-db)
                   (lambda () (cl-incf db-called))))
          (elot-label-lookup))
        (should (= 1 attriblist-called))
        (should (= 0 db-called))))))

(ert-deftest test-label-lookup-dispatches-to-db ()
  "With only `elot-active-label-sources' set, the DB path runs."
  (elot-ll-test--with-fresh
    (elot-db-update-source "src" nil "csv"
                           '(("ex:foo" "Foo" nil)))
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (let ((attriblist-called 0)
            (db-called         0))
        (cl-letf (((symbol-function 'elot-label-lookup--from-attriblist)
                   (lambda () (cl-incf attriblist-called)))
                  ((symbol-function 'elot-label-lookup--from-db)
                   (lambda () (cl-incf db-called))))
          (elot-label-lookup))
        (should (= 0 attriblist-called))
        (should (= 1 db-called))))))

(ert-deftest test-label-lookup-no-sources-user-error ()
  "With neither slurp data nor active sources, a user-error is raised."
  (elot-ll-test--with-fresh
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources nil)
      (let ((err (should-error (elot-label-lookup) :type 'user-error)))
        (should (string-match-p "no slurp data and no active DB sources"
                                (error-message-string err)))))))

;;;; ---------------------------------------------------------------------
;;;; 6 + 7: DB path insertion
;;;; ---------------------------------------------------------------------

(ert-deftest test-label-lookup-inserts-curie-for-iri ()
  "DB path: selected id is a full IRI with a known prefix -> CURIE token inserted."
  (elot-ll-test--with-fresh
    ;; Source must exist (FK target) before we can attach a prefix to it.
    (elot-db-update-source
     "src" nil "csv"
     '(("http://example.org/ex/Widget" "Widget" nil)))
    ;; Source-scoped prefix so `elot-db-contract-uri' has something to match.
    (elot-db-add-prefix "src" nil "ex" "http://example.org/ex/")
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "Widget")))
        (elot-label-lookup))
      (should (string-match-p "\\bex:Widget\\b" (buffer-string))))))

(ert-deftest test-label-lookup-inserts-curie-for-curie ()
  "DB path: selected id is already a CURIE -> inserted verbatim."
  (elot-ll-test--with-fresh
    (elot-db-update-source
     "src" nil "csv"
     '(("ex:Gadget" "Gadget" nil)))
    (with-temp-buffer
      (setq-local elot-attriblist-ht nil)
      (setq-local elot-active-label-sources '(("src" nil)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _coll &rest _) "Gadget")))
        (elot-label-lookup))
      (should (string-match-p "\\bex:Gadget\\b" (buffer-string))))))

;;;; ---------------------------------------------------------------------
;;;; 8: autoload smoke test in a fresh `emacs -Q' subprocess
;;;; ---------------------------------------------------------------------

(defconst elot-ll-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst elot-ll-test--pkg-dir
  (expand-file-name "../elot-package/" elot-ll-test--dir))

(defun elot-ll-test--generate-autoloads (target-dir)
  "Generate `elot-autoloads.el' covering `elot-package/' into TARGET-DIR.
Returns the absolute path of the generated file.
Uses `loaddefs-generate' on Emacs 29+; falls back to
`make-directory-autoloads' / `update-directory-autoloads' otherwise."
  (let ((out (expand-file-name "elot-autoloads.el" target-dir)))
    (cond
     ((fboundp 'loaddefs-generate)
      (loaddefs-generate elot-ll-test--pkg-dir out))
     ((fboundp 'make-directory-autoloads)
      (make-directory-autoloads elot-ll-test--pkg-dir out))
     (t
      (let ((generated-autoload-file out))
        (update-directory-autoloads elot-ll-test--pkg-dir))))
    (unless (file-readable-p out)
      (error "autoload generation failed: %s not produced" out))
    out))

(ert-deftest test-label-lookup-autoloaded ()
  "In a fresh `emacs -Q --batch', `elot-label-lookup' is reachable via autoloads alone."
  (skip-unless (executable-find invocation-name))
  (let* ((tmpdir (make-temp-file "elot-ll-autoloads-" t))
         (autoloads-file
          (condition-case err
              (elot-ll-test--generate-autoloads tmpdir)
            (error
             (delete-directory tmpdir t)
             (signal (car err) (cdr err)))))
         (script
          ;; The child asserts, in order:
          ;;   - after (require 'elot-autoloads), the symbol is fboundp
          ;;     and its function cell is an autoload object;
          ;;   - after loading elot-label-display.el explicitly, the
          ;;     symbol resolves to a real function (no autoload
          ;;     redirection to a missing file).
          "(progn
             (let ((ok t))
               (require 'elot-autoloads)
               (unless (fboundp 'elot-label-lookup)
                 (message \"FAIL not-fboundp\") (setq ok nil))
               (unless (autoloadp (symbol-function 'elot-label-lookup))
                 (message \"FAIL not-autoload\") (setq ok nil))
               (require 'elot-db)
               (require 'elot-label-display)
               (unless (and (fboundp 'elot-label-lookup)
                            (not (autoloadp (symbol-function 'elot-label-lookup))))
                 (message \"FAIL unresolved-after-load\") (setq ok nil))
               (kill-emacs (if ok 0 1))))")
         (exit
          (call-process
           invocation-name nil nil nil
           "-Q" "--batch"
           "-L" elot-ll-test--pkg-dir
           "-L" tmpdir
           "--eval" script)))
    (delete-directory tmpdir t)
    (should (equal 0 exit))))

(provide 'elot-label-lookup-test)
;;; elot-label-lookup-test.el ends here
