;;; elot-gptel-confirm-mutations-test.el --- Tests for confirm-mutations policy  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-confirm-mutations-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F5 -- tests for the
;; `elot-gptel-confirm-mutations' defcustom and its effect on
;; `elot-gptel--confirm-effective-p' / `elot-gptel--register-one'.
;;
;; Pure-Elisp; gptel is not required.  We test the policy helper
;; directly (no `gptel-make-tool' call).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file)))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; `elot-gptel--confirm-effective-p' policy table
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-confirm-mutations-test-always ()
  "Policy `always' honours spec `:confirm t' regardless of side-effects flag."
  (let ((elot-gptel-confirm-mutations 'always))
    (let ((elot-gptel-allow-side-effects nil))
      (should (eq t (elot-gptel--confirm-effective-p t))))
    (let ((elot-gptel-allow-side-effects t))
      (should (eq t (elot-gptel--confirm-effective-p t))))))

(ert-deftest elot-gptel-confirm-mutations-test-never ()
  "Policy `never' suppresses `:confirm t' regardless of side-effects flag."
  (let ((elot-gptel-confirm-mutations 'never))
    (let ((elot-gptel-allow-side-effects nil))
      (should-not (elot-gptel--confirm-effective-p t)))
    (let ((elot-gptel-allow-side-effects t))
      (should-not (elot-gptel--confirm-effective-p t)))))

(ert-deftest elot-gptel-confirm-mutations-test-once-armed ()
  "Policy `once-armed' honours `:confirm t' only when side-effects flag is nil."
  (let ((elot-gptel-confirm-mutations 'once-armed))
    (let ((elot-gptel-allow-side-effects nil))
      (should (eq t (elot-gptel--confirm-effective-p t))))
    (let ((elot-gptel-allow-side-effects t))
      (should-not (elot-gptel--confirm-effective-p t)))))

(ert-deftest elot-gptel-confirm-mutations-test-nil-spec-passthrough ()
  "A spec that omits `:confirm' stays non-confirming under every policy."
  (dolist (policy '(always once-armed never))
    (let ((elot-gptel-confirm-mutations policy))
      (let ((elot-gptel-allow-side-effects nil))
        (should-not (elot-gptel--confirm-effective-p nil)))
      (let ((elot-gptel-allow-side-effects t))
        (should-not (elot-gptel--confirm-effective-p nil))))))

(ert-deftest elot-gptel-confirm-mutations-test-default-once-armed ()
  "The shipped default is `once-armed' (matches Step 9.3.F5 plan)."
  (should (eq 'once-armed (default-value 'elot-gptel-confirm-mutations))))

(ert-deftest elot-gptel-confirm-mutations-test-unknown-policy-defaults-to-always ()
  "An unrecognised policy value falls back to `always' (safe default)."
  (let ((elot-gptel-confirm-mutations 'bogus)
        (elot-gptel-allow-side-effects t))
    (should (eq t (elot-gptel--confirm-effective-p t)))))

;;; ---------------------------------------------------------------------------
;;; Spec-level `:confirm t' is preserved on the M9.3 insert specs
;;; (the policy is applied at registration time, not by mutating the spec).
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-confirm-mutations-test-spec-preserves-confirm-t ()
  "Insert tool specs still carry `:confirm t' regardless of the policy.
The defcustom does not edit the spec table; it only governs how
`elot-gptel--register-one' translates the spec value into the
`gptel-make-tool' call."
  (dolist (name '("elot_insert_sibling_resource"
                  "elot_insert_child_resource"
                  "elot_insert_resource_tree"))
    (let* ((spec (assoc name elot-gptel--tool-specs))
           (plist (cdr spec)))
      (should spec)
      (should (eq t (plist-get plist :confirm))))))

(provide 'elot-gptel-confirm-mutations-test)
;;; elot-gptel-confirm-mutations-test.el ends here
