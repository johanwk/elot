;;; elot-sources-register-test.el --- ERT tests for Step 1.4  -*- lexical-binding: t; -*-

;; Usage:  make -C test register-test  (or)
;;         cd test && emacs --batch -L ../elot-package -L . \
;;            -l elot-sources-register-test.el \
;;            -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.4 tests: the source-registration UI built on top of
;; `elot-source-parse' (Step 1.3) and the `elot-db-*' primitives
;; (Steps 1.1 / 1.1.1 / 1.2).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file)))

(require 'elot-db)
(require 'elot-sources)

;;;; Harness

(defvar elot-sreg-test--tmp-db nil)
(defvar elot-sreg-test--tmp-files nil)

(defconst elot-sreg-test--dir
  (file-name-directory
   (or load-file-name buffer-file-name (expand-file-name "dummy")))
  "Directory containing this test file, captured at load time.")

(defun elot-sreg-test--fixture (name)
  "Return absolute path to fixture file NAME under test/fixtures/."
  (expand-file-name (concat "fixtures/" name) elot-sreg-test--dir))

(defun elot-sreg-test--temp-copy (fixture)
  "Copy FIXTURE (name under test/fixtures) to a temp file; track for cleanup."
  (let* ((src  (elot-sreg-test--fixture fixture))
         (ext  (file-name-extension fixture))
         (dst  (make-temp-file "elot-sreg-" nil (concat "." ext))))
    (copy-file src dst t)
    (push dst elot-sreg-test--tmp-files)
    dst))

(defun elot-sreg-test--setup ()
  (ignore-errors (elot-db-close))
  (setq elot-sreg-test--tmp-db
        (make-temp-file "elot-sreg-db-" nil ".sqlite"))
  (ignore-errors (delete-file elot-sreg-test--tmp-db))
  (setq elot-db-file elot-sreg-test--tmp-db)
  (elot-db-init elot-sreg-test--tmp-db)
  (setq elot-sreg-test--tmp-files nil))

(defun elot-sreg-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-sreg-test--tmp-db
             (file-exists-p elot-sreg-test--tmp-db))
    (ignore-errors (delete-file elot-sreg-test--tmp-db)))
  (dolist (f elot-sreg-test--tmp-files)
    (ignore-errors (delete-file f)))
  (setq elot-sreg-test--tmp-db nil
        elot-sreg-test--tmp-files nil))

(defmacro elot-sreg-test--with-env (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-sreg-test--setup) ,@body)
     (elot-sreg-test--teardown)))

;;;; register-source

(ert-deftest test-register-source-adds-to-db ()
  "Registering a CSV source writes the entities + attributes to the DB."
  (elot-sreg-test--with-env
    (let* ((csv    (elot-sreg-test--temp-copy "labels.csv"))
           (result (elot-label-register-source csv nil)))
      (should (eq :written (car result)))
      (should (= 3 (nth 3 result)))
      (should (elot-db-source-exists-p csv))
      (should (= 3 (elot-db-source-entity-count csv)))
      (should (equal "Widget"
                     (elot-db-get-label "ex:Widget" `((,csv nil))))))))

(ert-deftest test-register-source-skips-unchanged ()
  "Re-registering a source whose mtime has not advanced is a no-op."
  (elot-sreg-test--with-env
    (let* ((csv (elot-sreg-test--temp-copy "labels.csv")))
      (should (eq :written (car (elot-label-register-source csv nil))))
      (let ((result (elot-label-register-source csv nil)))
        (should (eq :skipped (car result)))))))

(ert-deftest test-register-source-rq-requires-data-source ()
  "A `.rq' source with no data-source is an error."
  (elot-sreg-test--with-env
    (let* ((rq (make-temp-file "elot-sreg-" nil ".rq")))
      (push rq elot-sreg-test--tmp-files)
      (with-temp-file rq
        (insert "SELECT ?id ?label WHERE { ?id <x> ?label }\n"))
      (should-error (elot-label-register-source rq nil)
                    :type 'user-error))))

(ert-deftest test-register-source-unknown-extension ()
  "A file with no registered parser is an error."
  (elot-sreg-test--with-env
    (let* ((bad (make-temp-file "elot-sreg-" nil ".wat")))
      (push bad elot-sreg-test--tmp-files)
      (with-temp-file bad (insert "noop\n"))
      (should-error (elot-label-register-source bad nil)
                    :type 'user-error))))

;;;; refresh-source

(ert-deftest test-refresh-source-forces-reparse ()
  "`elot-label-refresh-source' re-parses even when mtime is unchanged."
  (elot-sreg-test--with-env
    (let* ((csv (elot-sreg-test--temp-copy "labels.csv")))
      (elot-label-register-source csv nil)
      ;; Skipped on a second plain register...
      (should (eq :skipped (car (elot-label-register-source csv nil))))
      ;; ...but refresh always writes.
      (let ((r (elot-label-refresh-source csv nil)))
        (should (eq :written (car r)))
        (should (= 3 (nth 3 r)))))))

;;;; unregister

(ert-deftest test-unregister-source-cascades ()
  "`elot-label-unregister-source' deletes the source row and cascades."
  (elot-sreg-test--with-env
    (let ((csv (elot-sreg-test--temp-copy "labels.csv")))
      (elot-label-register-source csv nil)
      (should (elot-label-unregister-source csv nil))
      (should-not (elot-db-source-exists-p csv))
      (should (= 0 (caar (sqlite-select
                          elot-db "SELECT COUNT(*) FROM entities"))))
      (should (= 0 (caar (sqlite-select
                          elot-db "SELECT COUNT(*) FROM attributes"))))
      ;; Removing an unknown source is a no-op returning nil.
      (should-not (elot-label-unregister-source "no-such" nil)))))

;;;; refresh-all

(ert-deftest test-refresh-all-continues-on-error ()
  "A mid-batch failure does not abort refresh-all; successes still count."
  (elot-sreg-test--with-env
    (let ((csv (elot-sreg-test--temp-copy "labels.csv"))
          (tsv (elot-sreg-test--temp-copy "labels.tsv")))
      (elot-label-register-source csv nil)
      (elot-label-register-source tsv nil)
      ;; Delete one file on disk so the refresh will fail for it
      ;; (insert-file-contents on a missing path signals an error).
      (delete-file csv)
      (setq elot-sreg-test--tmp-files
            (delete csv elot-sreg-test--tmp-files))
      (let ((summary (elot-label-refresh-all-sources)))
        (should (= 1 (plist-get summary :ok)))
        (should (= 1 (plist-get summary :failed)))
        (should (= 1 (length (plist-get summary :errors))))))))

(ert-deftest test-refresh-all-all-succeed ()
  "When every source can be refreshed, :failed is 0 and :errors is empty."
  (elot-sreg-test--with-env
    (let ((csv  (elot-sreg-test--temp-copy "labels.csv"))
          (tsv  (elot-sreg-test--temp-copy "labels.tsv"))
          (json (elot-sreg-test--temp-copy "labels-flat.json")))
      (elot-label-register-source csv nil)
      (elot-label-register-source tsv nil)
      (elot-label-register-source json nil)
      (let ((summary (elot-label-refresh-all-sources)))
        (should (= 3 (plist-get summary :ok)))
        (should (= 0 (plist-get summary :failed)))
        (should (null (plist-get summary :errors)))))))

;;;; list-sources

(ert-deftest test-list-sources-tabulated ()
  "`elot-label-list-sources' builds entries for every registered source."
  (elot-sreg-test--with-env
    (let ((csv (elot-sreg-test--temp-copy "labels.csv"))
          (tsv (elot-sreg-test--temp-copy "labels.tsv")))
      (elot-label-register-source csv nil)
      (elot-label-register-source tsv nil)
      (let ((entries (elot-label--build-entries)))
        (should (= 2 (length entries)))
        ;; Each entry is (id vector-of-5-columns).
        (should (cl-every (lambda (e)
                            (and (consp (car e))
                                 (vectorp (cadr e))
                                 (= 5 (length (cadr e)))))
                          entries))
        ;; The Entities column matches the DB count.
        (dolist (e entries)
          (let* ((src (car (car e)))
                 (n   (string-to-number (elt (cadr e) 3))))
            (should (= n (elot-db-source-entity-count src)))))))))

;;;; Round-trip via get-label after registration

(ert-deftest test-register-then-get-label ()
  "End-to-end: register CSV + JSON, make them active, look up labels."
  (elot-sreg-test--with-env
    (let ((csv  (elot-sreg-test--temp-copy "labels.csv"))
          (json (elot-sreg-test--temp-copy "labels-nested.json")))
      (elot-label-register-source csv nil)
      (elot-label-register-source json nil)
      (let ((elot-active-label-sources
             `((,csv nil) (,json nil))))
        (should (equal "Widget" (elot-db-get-label "ex:Widget")))
        ;; JSON-only id falls through to second active source.
        (should (stringp (elot-db-get-label "ex:Widget")))))))

(provide 'elot-sources-register-test)

;;; elot-sources-register-test.el ends here
