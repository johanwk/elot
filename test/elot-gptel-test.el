;;; elot-gptel-test.el --- Tests for elot-gptel.el  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 1 Step 1.5:
;;
;;   elot-gptel-test-lint-clean      lint a known-good fixture
;;   elot-gptel-test-lint-broken     lint a malformed fixture
;;   elot-gptel-test-lint-traversal  refuse paths outside the project
;;
;; The tests exercise `elot-gptel-tool-lint' directly (the function
;; that the registered tool dispatches to), so they do not require
;; gptel to be installed.  A separate test verifies that
;; `elot-gptel-register-tools' errors helpfully without gptel.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-test--repo-root repo-root
        elot-gptel-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(defvar elot-gptel-test--repo-root nil)
(defvar elot-gptel-test--fixtures nil)

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-test-lint-clean ()
  "The known-good minimal fixture passes ELOT lint cleanly."
  (let* ((file (expand-file-name "minimal-ontology.org"
                                 elot-gptel-test--fixtures))
         (default-directory elot-gptel-test--repo-root)
         (out (elot-gptel-tool-lint file nil nil)))
    (should (stringp out))
    ;; Either fully clean or only low-severity notes; must not start
    ;; with `ERROR:' (which would indicate a tool-level failure).
    (should-not (string-prefix-p "ERROR:" out))
    ;; Acceptable: "OK: no lint issues" OR a summary with zero errors.
    (should (or (string= out "OK: no lint issues")
                (string-match-p "Summary: 0 errors" out)))))

(ert-deftest elot-gptel-test-lint-broken ()
  "The malformed fixture trips the prefix-table checker."
  (let* ((file (expand-file-name "gptel-lint-broken.org"
                                 elot-gptel-test--fixtures))
         (default-directory elot-gptel-test--repo-root)
         (out (elot-gptel-tool-lint file "all" nil)))
    (should (stringp out))
    (should-not (string= out "OK: no lint issues"))
    (should (string-match-p "elot/prefix-table" out))
    (should (string-match-p "^Summary: " out))))

(ert-deftest elot-gptel-test-lint-broken-ontology-decl ()
  "A malformed ontology-declaration heading trips the new checker.
Catches the typo class where the title parenthetical fails to parse,
which would otherwise cause the tangler to silently omit the
\"Ontology:\" block from the OMN output."
  (let* ((file (expand-file-name "gptel-lint-broken-ontology-decl.org"
                                 elot-gptel-test--fixtures))
         (default-directory elot-gptel-test--repo-root)
         (out (elot-gptel-tool-lint file "all" nil)))
    (should (stringp out))
    (should (string-match-p "elot/ontology-declaration-heading" out))))

(ert-deftest elot-gptel-test-lint-traversal ()
  "Paths escaping the project root are refused."
  (let* ((default-directory elot-gptel-test--repo-root)
         (out (elot-gptel-tool-lint "../../etc/passwd" nil nil)))
    (should (stringp out))
    (should (string-prefix-p "ERROR:" out))
    (should (string-match-p "outside project\\|not found" out))))

(ert-deftest elot-gptel-test-lint-severity-filter ()
  "Severity filter accepts the three documented values."
  (let* ((file (expand-file-name "gptel-lint-broken.org"
                                 elot-gptel-test--fixtures))
         (default-directory elot-gptel-test--repo-root))
    ;; `error' filter -- output is either clean or contains only errors.
    (let ((out (elot-gptel-tool-lint file "error" nil)))
      (should (stringp out))
      (should-not (string-prefix-p "ERROR: elot-gptel" out)))
    ;; Bad severity value yields a structured error, not a backtrace.
    (let ((out (elot-gptel-tool-lint file "frobnicate" nil)))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-gptel-test-register-requires-gptel ()
  "Registration errors with a helpful message when gptel is absent.
If gptel happens to be installed in the test environment, the
call must instead succeed and populate `elot-gptel--tools'."
  (if (require 'gptel nil 'noerror)
      (progn
        (elot-gptel-register-tools)
        (should (consp elot-gptel--tools))
        (elot-gptel-unregister-tools)
        (should (null elot-gptel--tools)))
    (should-error (elot-gptel-register-tools) :type 'user-error)))

(provide 'elot-gptel-test)
;;; elot-gptel-test.el ends here
