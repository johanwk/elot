;;; elot-sparql-advice-test.el --- Lifecycle of the SPARQL advice  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.5 / Acceptance check.
;;
;; Encodes as an automated check the assertion that ELOT's around-
;; advice on `org-babel-execute:sparql' is *not* installed at
;; `require' time (no global side effect at load) and is installed
;; only as a side effect of enabling `elot-mode' in a buffer.
;; Disabling `elot-mode' in the last remaining ELOT buffer must
;; uninstall the advice again.
;;
;; The reference-counting helpers under test are
;; `elot-mode--install-sparql-advice' and
;; `elot-mode--uninstall-sparql-advice' (see `elot-mode.org' /
;; `elot-package/elot-mode.el').
;;
;; This test does not require ROBOT and does not execute any
;; SPARQL block.  It only inspects the advice list before and
;; after toggling `elot-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

;; Pull in package.el so an ELPA-installed `sparql-mode' / `ob-sparql'
;; (the optional packages that define `org-babel-execute:sparql') is
;; reachable under `emacs --batch'.  Mirrors the setup used by the
;; round-trip test; both packages are optional and the test below
;; works just as well when `org-babel-execute:sparql' is unbound.
(require 'package)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(require 'sparql-mode nil t)
(require 'ob-sparql  nil t)

(require 'elot)
(require 'elot-mode)

(defun elot-spadv-test--advice-present-p ()
  "Return non-nil if ELOT's SPARQL advice is currently installed."
  (and (fboundp 'org-babel-execute:sparql)
       (advice-member-p 'elot--custom-org-babel-execute-sparql
                        'org-babel-execute:sparql)))

(defmacro elot-spadv-test--with-clean-state (&rest body)
  "Run BODY with a known-pristine SPARQL-advice state.
Saves and restores the install flag, the buffer-count, and the
advice itself so that test execution order does not matter and a
failure in BODY does not leak global state into later tests."
  (declare (indent 0))
  `(let ((saved-installed elot--sparql-advice-installed-p)
         (saved-count     elot--sparql-advice-buffer-count)
         (saved-present   (elot-spadv-test--advice-present-p)))
     (unwind-protect
         (progn
           ;; Force a clean slate before BODY runs.
           (when (elot-spadv-test--advice-present-p)
             (advice-remove 'org-babel-execute:sparql
                            #'elot--custom-org-babel-execute-sparql))
           (setq elot--sparql-advice-installed-p nil
                 elot--sparql-advice-buffer-count 0)
           ,@body)
       ;; Restore the pre-test state irrespective of BODY's outcome.
       (when (elot-spadv-test--advice-present-p)
         (advice-remove 'org-babel-execute:sparql
                        #'elot--custom-org-babel-execute-sparql))
       (when saved-present
         (advice-add 'org-babel-execute:sparql :around
                     #'elot--custom-org-babel-execute-sparql))
       (setq elot--sparql-advice-installed-p saved-installed
             elot--sparql-advice-buffer-count saved-count))))

(defmacro elot-spadv-test--with-elot-buffer (var &rest body)
  "Create a fresh Org buffer that looks like an ELOT file, bind it to VAR.
Run BODY with that buffer current.  `elot-mode-auto-detect' is
disabled inside the buffer so the test drives mode toggling
explicitly via `(elot-mode 1)' / `(elot-mode -1)' rather than via
the `org-mode-hook' auto-enable path."
  (declare (indent 1))
  `(let ((,var (generate-new-buffer " *elot-spadv-test*")))
     (unwind-protect
         (with-current-buffer ,var
           ;; Minimal ELOT-looking content; auto-detect is off so
           ;; `org-mode' does not flip `elot-mode' on for us.
           (insert "* ontology\n"
                   ":PROPERTIES:\n"
                   ":ELOT-context-type: ontology\n"
                   ":END:\n")
           (let ((elot-mode-auto-detect nil))
             (org-mode))
           (setq-local elot-mode-auto-detect nil)
           ,@body)
       (when (buffer-live-p ,var)
         (with-current-buffer ,var
           (when (bound-and-true-p elot-mode)
             (elot-mode -1))
           (set-buffer-modified-p nil))
         (kill-buffer ,var)))))


;;;; Tests

(ert-deftest elot-sparql-advice-not-installed-at-load ()
  "`(require 'elot)' must not install the SPARQL advice as a side effect.
The advice is a global mutation of `org-babel-execute:sparql' and
must therefore live on the `elot-mode' enable/disable lifecycle,
not on package load."
  ;; Capture the state as it stands right after the file's top-of-
  ;; module `(require 'elot)' / `(require 'elot-mode)' calls have
  ;; run.  Any earlier test that left the advice installed would
  ;; falsify this check, which is precisely the regression we want
  ;; to catch — so we deliberately do NOT use the clean-state
  ;; macro here.
  (should-not elot--sparql-advice-installed-p)
  (should (= 0 elot--sparql-advice-buffer-count))
  (should-not (elot-spadv-test--advice-present-p)))

(ert-deftest elot-sparql-advice-lifecycle ()
  "Enabling `elot-mode' installs the SPARQL advice; disabling removes it.
Encodes the Milestone 1 acceptance check verbatim: in a fresh
Emacs, `(advice-member-p ...)' on `org-babel-execute:sparql' is
nil; after `M-x elot-mode' in an ELOT-looking Org buffer it is
non-nil; after disabling `elot-mode' in the last remaining ELOT
buffer it is nil again."
  (elot-spadv-test--with-clean-state
    ;; Pre-condition: advice not installed.
    (should-not (elot-spadv-test--advice-present-p))
    (should (= 0 elot--sparql-advice-buffer-count))

    (elot-spadv-test--with-elot-buffer buf
      ;; Enable: advice goes in, refcount goes to 1.
      (elot-mode 1)
      (should (bound-and-true-p elot-mode))
      (should elot--sparql-advice-installed-p)
      (should (= 1 elot--sparql-advice-buffer-count))
      (should (elot-spadv-test--advice-present-p))

      ;; Disable in the same (and only) buffer: advice goes out,
      ;; refcount goes back to 0.
      (elot-mode -1)
      (should-not (bound-and-true-p elot-mode))
      (should-not elot--sparql-advice-installed-p)
      (should (= 0 elot--sparql-advice-buffer-count))
      (should-not (elot-spadv-test--advice-present-p)))))

(ert-deftest elot-sparql-advice-refcount-multi-buffer ()
  "Two ELOT buffers share the global advice via a refcount.
Disabling `elot-mode' in the first buffer must NOT remove the
advice while a second ELOT buffer is still alive; only the
second toggle-off (the last ELOT buffer) takes the advice down."
  (elot-spadv-test--with-clean-state
    (elot-spadv-test--with-elot-buffer buf-a
      (elot-mode 1)
      (should (= 1 elot--sparql-advice-buffer-count))
      (should (elot-spadv-test--advice-present-p))

      (elot-spadv-test--with-elot-buffer buf-b
        (elot-mode 1)
        (should (= 2 elot--sparql-advice-buffer-count))
        (should (elot-spadv-test--advice-present-p))

        ;; Disable in buf-b first: refcount drops to 1, advice stays.
        (elot-mode -1)
        (should (= 1 elot--sparql-advice-buffer-count))
        (should (elot-spadv-test--advice-present-p)))

      ;; Back in buf-a: now the last ELOT buffer.  Disabling
      ;; `elot-mode' here drops the refcount to 0 and removes the
      ;; global advice.
      (elot-mode -1)
      (should (= 0 elot--sparql-advice-buffer-count))
      (should-not (elot-spadv-test--advice-present-p)))))

(ert-deftest elot-sparql-advice-install-is-idempotent ()
  "Calling the install helper twice from one buffer does not double-add.
Defence-in-depth: the advice is `:around'; double-adding it would
chain it twice and the body would run twice per query.  The
install helper guards against this with
`elot--sparql-advice-installed-p'."
  (elot-spadv-test--with-clean-state
    (elot-mode--install-sparql-advice)
    (elot-mode--install-sparql-advice)
    (should elot--sparql-advice-installed-p)
    (should (= 2 elot--sparql-advice-buffer-count))
    (should (elot-spadv-test--advice-present-p))
    ;; Pair the two installs with two uninstalls so the clean-state
    ;; restore at macro exit sees a balanced refcount.
    (elot-mode--uninstall-sparql-advice)
    (should (elot-spadv-test--advice-present-p))   ; still 1 buffer
    (elot-mode--uninstall-sparql-advice)
    (should-not (elot-spadv-test--advice-present-p))
    (should (= 0 elot--sparql-advice-buffer-count))))

(provide 'elot-sparql-advice-test)
;;; elot-sparql-advice-test.el ends here
