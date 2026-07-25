;;; elot-gptel-db-activate-source-test.el --- Tests for M9 Step 9.1  -*- lexical-binding: t; -*-

;; Usage:  make -C test gptel-db-activate-source-test  (or)
;;         cd test && emacs --batch -L ../elot-package -L . \
;;             -l elot-gptel-db-activate-source-test.el \
;;             -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.1: tests for
;; `elot_db_activate_source' -- the gptel wrapper over
;; `elot-label-activate-source-for-file'.  Pure-Elisp; no ROBOT
;; required.

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
(require 'elot-gptel)

;;;; Harness

(defvar elot-gptel-act-test--tmp-db nil)
(defvar elot-gptel-act-test--tmp-files nil)

(defconst elot-gptel-act-test--dir
  (file-name-directory
   (or load-file-name buffer-file-name (expand-file-name "dummy")))
  "Directory containing this test file, captured at load time.")

(defun elot-gptel-act-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-gptel-act-test--dir))

(defun elot-gptel-act-test--temp-copy (fixture)
  (let* ((src (elot-gptel-act-test--fixture fixture))
         (ext (file-name-extension fixture))
         ;; Place the temp file under the test dir so the gptel
         ;; project-root containment check accepts it (tests run with
         ;; default-directory = test/, which is the project root in batch).
         (dst (make-temp-file
               (expand-file-name "elot-act-" elot-gptel-act-test--dir)
               nil (concat "." ext))))
    (copy-file src dst t)
    (push dst elot-gptel-act-test--tmp-files)
    dst))

(defun elot-gptel-act-test--setup ()
  (ignore-errors (elot-db-close))
  (setq elot-gptel-act-test--tmp-db
        (make-temp-file "elot-act-db-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-act-test--tmp-db))
  (setq elot-db-file elot-gptel-act-test--tmp-db)
  (elot-db-init elot-gptel-act-test--tmp-db)
  (setq-default elot-active-label-sources nil)
  (setq elot-gptel-act-test--tmp-files nil))

(defun elot-gptel-act-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-act-test--tmp-db
             (file-exists-p elot-gptel-act-test--tmp-db))
    (ignore-errors (delete-file elot-gptel-act-test--tmp-db)))
  (dolist (f elot-gptel-act-test--tmp-files)
    (ignore-errors (delete-file f)))
  (setq-default elot-active-label-sources nil)
  (setq elot-gptel-act-test--tmp-db nil
        elot-gptel-act-test--tmp-files nil))

(defmacro elot-gptel-act-test--with-env (&rest body)
  (declare (indent 0) (debug t))
  `(let ((default-directory elot-gptel-act-test--dir))
     (unwind-protect
         (progn (elot-gptel-act-test--setup) ,@body)
       (elot-gptel-act-test--teardown))))

;;;; Tests -- helper

(ert-deftest elot-gptel-act-test-helper-registers-fresh-csv ()
  "First call on a CSV: status=registered, source becomes active."
  (elot-gptel-act-test--with-env
    (let* ((csv (elot-gptel-act-test--temp-copy "labels.csv"))
           (result (elot-label-activate-source-for-file csv)))
      (should (equal csv (plist-get result :source)))
      (should (eq :registered (plist-get result :status)))
      (should (= 3 (plist-get result :entities)))
      (should (null (plist-get result :was-active)))
      ;; default value updated
      (should (equal (list csv nil)
                     (car (default-value 'elot-active-label-sources)))))))

(ert-deftest elot-gptel-act-test-helper-idempotent ()
  "Second call on an unchanged source: status=already-active."
  (elot-gptel-act-test--with-env
    (let* ((csv (elot-gptel-act-test--temp-copy "labels.csv")))
      (elot-label-activate-source-for-file csv)
      (let ((result (elot-label-activate-source-for-file csv)))
        (should (eq :already-active (plist-get result :status)))
        (should (plist-get result :was-active))
        ;; Still at the head, no duplicate.
        (should (= 1 (cl-count-if (lambda (e) (equal (car e) csv))
                                  (default-value
                                    'elot-active-label-sources))))))))

(ert-deftest elot-gptel-act-test-helper-rejects-missing-file ()
  (elot-gptel-act-test--with-env
    (should-error (elot-label-activate-source-for-file
                   (expand-file-name "no-such-file.org"
                                     elot-gptel-act-test--dir))
                  :type 'user-error)))

(ert-deftest elot-gptel-act-test-helper-rejects-unknown-extension ()
  (elot-gptel-act-test--with-env
    (let ((bad (make-temp-file
                (expand-file-name "elot-act-" elot-gptel-act-test--dir)
                nil ".wat")))
      (push bad elot-gptel-act-test--tmp-files)
      (with-temp-file bad (insert "noop\n"))
      (should-error (elot-label-activate-source-for-file bad)
                    :type 'user-error))))

;;;; Tests -- gptel wrapper

(ert-deftest elot-gptel-act-test-wrapper-ok-envelope ()
  (elot-gptel-act-test--with-env
    (let* ((csv  (elot-gptel-act-test--temp-copy "labels.csv"))
           (out  (elot-gptel-tool-db-activate-source csv)))
      (should (stringp out))
      (should (string-match-p "^OK: activated " out))
      (should (string-match-p "status=registered" out))
      (should (string-match-p "3 entities" out)))))

(ert-deftest elot-gptel-act-test-wrapper-idempotent-already-active ()
  (elot-gptel-act-test--with-env
    (let* ((csv (elot-gptel-act-test--temp-copy "labels.csv")))
      (elot-gptel-tool-db-activate-source csv)
      (let ((out (elot-gptel-tool-db-activate-source csv)))
        (should (string-match-p "status=already-active" out))))))

(ert-deftest elot-gptel-act-test-wrapper-rejects-missing-file ()
  (elot-gptel-act-test--with-env
    (let ((out (elot-gptel-tool-db-activate-source
                "definitely-not-here.org")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "file not found" out)))))

(ert-deftest elot-gptel-act-test-wrapper-rejects-out-of-project ()
  (elot-gptel-act-test--with-env
    (let* ((out (elot-gptel-tool-db-activate-source "/tmp/outside.org")))
      (should (string-prefix-p "ERROR:" out))
      ;; Either "refusing path outside project" or "file not found"
      ;; depending on platform; both shapes are acceptable refusals.
      (should (or (string-match-p "outside project" out)
                  (string-match-p "file not found" out))))))

(ert-deftest elot-gptel-act-test-wrapper-rejects-empty-path ()
  (elot-gptel-act-test--with-env
    (let ((out (elot-gptel-tool-db-activate-source "")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "missing file argument" out)))))

;;;; Tool spec + dispatcher arity

(ert-deftest elot-gptel-act-test-spec-registered ()
  (let ((spec (cl-find "elot_db_activate_source"
                       elot-gptel--tool-specs
                       :key #'car :test #'string=)))
    (should spec)
    (should (eq 'elot-gptel-tool-db-activate-source
                (plist-get (cdr spec) :function)))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 1 (length args)))
      (should (equal "file" (plist-get (car args) :name))))))

(ert-deftest elot-gptel-act-test-dispatcher-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-db-activate-source)))
    (should (functionp thunk))
    ;; Calling with no args should be a wrong-number-of-arguments error
    ;; (the thunk requires exactly one positional).
    (should-error (funcall thunk))))

;;;; Subsequent label lookup sees the activated source

(ert-deftest elot-gptel-act-test-label-lookup-after-activate ()
  "Activating a source makes its labels resolvable via the standard API."
  (elot-gptel-act-test--with-env
    (let* ((csv (elot-gptel-act-test--temp-copy "labels.csv")))
      (elot-gptel-tool-db-activate-source csv)
      ;; The default value of elot-active-label-sources now contains the
      ;; CSV at the head; label lookup against the default falls through.
      (should (equal "Widget"
                     (elot-db-get-label
                      "ex:Widget"
                      (default-value 'elot-active-label-sources)))))))

(provide 'elot-gptel-db-activate-source-test)

;;; elot-gptel-db-activate-source-test.el ends here
