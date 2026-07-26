;;; elot-gptel-metrics-test.el --- Tests for elot_metrics tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-metrics-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Thin wrapper around ROBOT's `measure' subcommand.  Scope is the
;; /integration seam/ only: we never assert the exact metric values
;; ROBOT computes (that is ROBOT's contract, not ours).  Tests are
;; split into:
;;
;;   - Pure tests (always run): no-ROBOT structured error, unknown
;;     format / metrics-level refusal, path-traversal refusal,
;;     dispatcher arity, tool-spec registration.
;;   - Live tests (ROBOT-gated): a real ontology flows through the
;;     tool and yields a non-empty metrics block.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-metrics-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (here (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path here)
  (setq elot-gptel-metrics-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-gptel-metrics-test--live-or-skip ()
  (elot-test-robot-skip-unless-available))

;;; ---------------------------------------------------------------------------
;;; Pure tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-metrics-test-no-robot ()
  "Structured ERROR when ROBOT is unavailable."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-gptel-metrics-test--repo-root))
    (let ((out (elot-gptel-tool-metrics
                "test/fixtures/minimal-ontology.org")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-gptel-metrics-test-bad-format ()
  "Unknown format is rejected before ROBOT is invoked."
  (let ((default-directory elot-gptel-metrics-test--repo-root))
    (let ((out (elot-gptel-tool-metrics
                "test/fixtures/minimal-ontology.org" nil "xml")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown format" out)))))

(ert-deftest elot-gptel-metrics-test-bad-level ()
  "Unknown metrics level is rejected before ROBOT is invoked."
  (let ((default-directory elot-gptel-metrics-test--repo-root))
    (let ((out (elot-gptel-tool-metrics
                "test/fixtures/minimal-ontology.org" "everything")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown metrics level" out)))))

(ert-deftest elot-gptel-metrics-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-gptel-metrics-test--repo-root))
    (let ((out (elot-gptel-tool-metrics "../../etc/passwd")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-gptel-metrics-test-tool-spec-registered ()
  "The `elot_metrics' tool spec is in the registry with the right shape."
  (let ((spec (assoc "elot_metrics" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-metrics))
    (let* ((args (plist-get (cdr spec) :args))
           (names (mapcar (lambda (a) (plist-get a :name)) args)))
      (should (member "file" names))
      (should (member "metrics" names))
      (should (member "format" names))
      (should (member "content" names)))))

(ert-deftest elot-gptel-metrics-test-dispatcher-arity ()
  "The dispatcher thunk forwards (file [metrics [format [content]]])."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-metrics)))
    (should (functionp thunk))
    (should (condition-case _ (progn (funcall thunk "/no/such") t)
              (user-error t) (error t)))))

;;; ---------------------------------------------------------------------------
;;; Live tests (skipped when ROBOT unavailable)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-metrics-test-live-essential ()
  "A real ontology flows through the tool and yields a metrics block."
  (elot-gptel-metrics-test--live-or-skip)
  (let* ((default-directory elot-gptel-metrics-test--repo-root)
         (out (elot-gptel-tool-metrics "examples/pets.org")))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (string-match-p "Ontology .*(metrics=essential" out))))

(ert-deftest elot-gptel-metrics-test-live-extended-yaml ()
  "The `extended' level with YAML format also flows cleanly."
  (elot-gptel-metrics-test--live-or-skip)
  (let* ((default-directory elot-gptel-metrics-test--repo-root)
         (out (elot-gptel-tool-metrics "examples/pets.org" "extended" "yaml")))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (string-match-p "metrics=extended, format=yaml" out))))

(provide 'elot-gptel-metrics-test)
;;; elot-gptel-metrics-test.el ends here
