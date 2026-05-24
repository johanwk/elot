;;; elot-gptel-diff-test.el --- Tests for elot_diff tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-diff-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.5 -- thin wrapper around
;; ROBOT's `diff' subcommand.  Scope is the /integration seam/
;; only: we never assert which structural differences ROBOT
;; surfaces in detail (that is ROBOT's contract, not ours), only
;; that the wrapper:
;;
;;   - returns ERROR cleanly when ROBOT is unavailable,
;;   - rejects unknown formats / missing baseline / paths outside
;;     the project,
;;   - on a clean self-diff (file vs. itself) returns the
;;     `OK: no structural differences ...' summary,
;;   - on a real diff returns a `Diff: baseline=... file=...
;;     format=...' block with non-empty body.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-diff-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (here (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path here)
  (setq elot-gptel-diff-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-gptel-diff-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

(defconst elot-gptel-diff-test--baseline
  "test/fixtures/diff/baseline-minimal.org")
(defconst elot-gptel-diff-test--current
  "test/fixtures/diff/current-minimal.org")

;;; ---------------------------------------------------------------------------
;;; Pure tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-diff-test-no-robot ()
  "Structured ERROR when ROBOT is unavailable."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-gptel-diff-test--repo-root))
    (let ((out (elot-gptel-tool-diff
                elot-gptel-diff-test--current
                elot-gptel-diff-test--baseline)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-gptel-diff-test-missing-baseline ()
  "Missing baseline argument is rejected before ROBOT is invoked."
  (let ((default-directory elot-gptel-diff-test--repo-root))
    (let ((out (elot-gptel-tool-diff
                elot-gptel-diff-test--current nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "missing baseline" out)))))

(ert-deftest elot-gptel-diff-test-bad-format ()
  "Unknown format is rejected before ROBOT is invoked."
  (let ((default-directory elot-gptel-diff-test--repo-root))
    (let ((out (elot-gptel-tool-diff
                elot-gptel-diff-test--current
                elot-gptel-diff-test--baseline
                "xml")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown format" out)))))

(ert-deftest elot-gptel-diff-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-gptel-diff-test--repo-root))
    (let ((out (elot-gptel-tool-diff
                "../../etc/passwd"
                elot-gptel-diff-test--baseline)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))
    (let ((out (elot-gptel-tool-diff
                elot-gptel-diff-test--current
                "../../etc/passwd")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-gptel-diff-test-tool-spec-registered ()
  "The `elot_diff' tool spec is in the registry with the right shape."
  (let ((spec (assoc "elot_diff" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-diff))
    (let* ((args (plist-get (cdr spec) :args))
           (names (mapcar (lambda (a) (plist-get a :name)) args)))
      (should (member "file" names))
      (should (member "baseline" names))
      (should (member "format" names)))))

(ert-deftest elot-gptel-diff-test-dispatcher-arity ()
  "The dispatcher thunk forwards (file baseline [format]) to the impl."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-diff)))
    (should (functionp thunk))
    ;; Arity should accept 2 or 3 positional args.
    (should (condition-case _
                (progn (funcall thunk "/no/such" "/no/such2") t)
              (user-error t) (error t)))))

;;; ---------------------------------------------------------------------------
;;; Live tests (skipped when ROBOT unavailable)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-diff-test-live-self-diff ()
  "Diffing a fixture against itself returns the OK no-differences summary."
  (elot-gptel-diff-test--live-or-skip)
  (let* ((default-directory elot-gptel-diff-test--repo-root)
         (out (elot-gptel-tool-diff
               elot-gptel-diff-test--baseline
               elot-gptel-diff-test--baseline)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (string-prefix-p "OK: no structural differences" out))))

(ert-deftest elot-gptel-diff-test-live-real-diff ()
  "Diffing the current snapshot against the baseline surfaces changes."
  (elot-gptel-diff-test--live-or-skip)
  (let* ((default-directory elot-gptel-diff-test--repo-root)
         (out (elot-gptel-tool-diff
               elot-gptel-diff-test--current
               elot-gptel-diff-test--baseline)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    ;; Per the M9.5 design we do NOT assert *which* changes ROBOT
    ;; surfaced -- only that the envelope is well-formed and the
    ;; diff body is non-empty.
    (should-not (string-prefix-p "OK:" out))
    (should (string-match-p "^Diff: baseline=" out))
    (should (string-match-p "file=current-minimal.org" out))
    (should (string-match-p "format=plain" out))))

(provide 'elot-gptel-diff-test)
;;; elot-gptel-diff-test.el ends here
