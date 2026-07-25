;;; elot-gptel-check-test.el --- Tests for M7.5 Step 7.5.6 -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-check-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.6:
;;
;; Composite `elot_check' tool: fail-fast pipeline LINT -> OMN PARSE
;; -> CONSISTENCY -> UNSATISFIABLE.  Tests cover:
;;
;;   - the pure stage-classifier helpers
;;     (`elot-gptel--check-lint-failed-p',
;;     `elot-gptel--check-omn-failed-p',
;;     `elot-gptel--check-consistency-classify');
;;   - end-to-end pipeline framing with the atomic tools stubbed
;;     out, exercising each fail-fast branch (lint-error,
;;     OMN-error, inconsistent, unsat-flow-through, all-pass);
;;   - the no-ROBOT graceful-degradation branch;
;;   - tool-spec / dispatcher arity registration.
;;
;; No real ROBOT invocations; all stages are mocked via `cl-letf'
;; so the test suite stays pure and fast.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-check-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-check-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Pure helpers
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-check-test-lint-failed-p ()
  (should-not (elot-gptel--check-lint-failed-p "OK: no lint issues"))
  (should-not (elot-gptel--check-lint-failed-p
               "12:1 [some/check/low] WARNING: x\nSummary: 0 errors, 1 warning"))
  (should     (elot-gptel--check-lint-failed-p
               "12:1 [some/check/high] ERROR: x\nSummary: 1 error, 0 warnings"))
  (should     (elot-gptel--check-lint-failed-p
               "Summary: 3 errors, 2 warnings"))
  (should-not (elot-gptel--check-lint-failed-p nil))
  (should-not (elot-gptel--check-lint-failed-p "")))

(ert-deftest elot-gptel-check-test-omn-failed-p ()
  (should-not (elot-gptel--check-omn-failed-p
               "OK: 1 ontology parsed and conforms to profile DL"))
  (should-not (elot-gptel--check-omn-failed-p
               "  OK: 1 ontology parsed"))
  (should     (elot-gptel--check-omn-failed-p
               "Ontology my-ont:\nERROR  [robot/unknown]  INVALID ONTOLOGY FILE ERROR ..."))
  (should     (elot-gptel--check-omn-failed-p
               "ERROR: ROBOT not available")))

(ert-deftest elot-gptel-check-test-consistency-classify ()
  (should (eq :ok
              (elot-gptel--check-consistency-classify
               "OK: ontology is consistent (reasoner=hermit, 1 node)")))
  (should (eq :unsat
              (elot-gptel--check-consistency-classify
               "OK: ontology is consistent but has unsatisfiable classes (reasoner=hermit, 1 node)")))
  (should (eq :inconsistent
              (elot-gptel--check-consistency-classify
               "INCONSISTENT [my-ont]: ontology has no models")))
  (should (eq :error
              (elot-gptel--check-consistency-classify
               "Ontology my-ont (reasoner=hermit):\nERROR ..."))))

;;; ---------------------------------------------------------------------------
;;; Tool registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-check-test-tool-spec-registered ()
  "`elot_check' appears in `elot-gptel--tool-specs' with the expected arity."
  (let* ((spec (cdr (assoc "elot_check" elot-gptel--tool-specs)))
         (args (plist-get spec :args))
         (names (mapcar (lambda (a) (plist-get a :name)) args)))
    (should spec)
    (should (eq (plist-get spec :function) 'elot-gptel-tool-check))
    (should (equal names '("file" "content" "profile" "reasoner")))
    ;; File is required, the other three optional.
    (should-not (plist-get (car args) :optional))
    (should (cl-every (lambda (a) (plist-get a :optional)) (cdr args)))))

(ert-deftest elot-gptel-check-test-dispatcher-arity ()
  "Dispatcher accepts (file &optional content profile reasoner)."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-check)))
    (should (functionp thunk))
    ;; Arity range: required 1, accepts up to 4 (or unevalled rest).
    (let ((arity (func-arity thunk)))
      (should (= (car arity) 1))
      (should (or (= (cdr arity) 4)
                  (eq (cdr arity) 'many))))))

;;; ---------------------------------------------------------------------------
;;; End-to-end pipeline (all atomic tools stubbed)
;;; ---------------------------------------------------------------------------

(defmacro elot-gptel-check-test--with-stubs
    (&rest body)
  "Run BODY with the four atomic tools redefined per the dynamic vars.
Bindings used:
  `elot-gptel-check-test--lint-result'
  `elot-gptel-check-test--omn-result'
  `elot-gptel-check-test--consistency-result'
  `elot-gptel-check-test--unsat-result'
  `elot-gptel-check-test--robot-available'"
  (declare (indent 0))
  `(cl-letf (((symbol-function 'elot-gptel-tool-lint)
              (lambda (&rest _) elot-gptel-check-test--lint-result))
             ((symbol-function 'elot-gptel-tool-omn-validate)
              (lambda (&rest _) elot-gptel-check-test--omn-result))
             ((symbol-function 'elot-gptel-tool-consistency)
              (lambda (&rest _) elot-gptel-check-test--consistency-result))
             ((symbol-function 'elot-gptel-tool-unsatisfiable)
              (lambda (&rest _) elot-gptel-check-test--unsat-result))
             ((symbol-function 'require) (lambda (&rest _) t))
             ((symbol-function 'elot-robot-available-p)
              (lambda () elot-gptel-check-test--robot-available)))
     ,@body))

(defvar elot-gptel-check-test--lint-result        "OK: no lint issues")
(defvar elot-gptel-check-test--omn-result         "OK: 1 ontology parsed")
(defvar elot-gptel-check-test--consistency-result "OK: ontology is consistent (reasoner=hermit, 1 node)")
(defvar elot-gptel-check-test--unsat-result       "OK: no unsatisfiable classes (reasoner=hermit, 1 node)")
(defvar elot-gptel-check-test--robot-available    t)

(ert-deftest elot-gptel-check-test-all-pass ()
  "Clean ontology: all four stages succeed, summary is the green verdict."
  (let ((elot-gptel-check-test--lint-result        "OK: no lint issues")
        (elot-gptel-check-test--omn-result         "OK: 1 ontology parsed and conform to profile DL")
        (elot-gptel-check-test--consistency-result "OK: ontology is consistent (reasoner=hermit, 1 node)")
        (elot-gptel-check-test--robot-available    t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil "DL" "hermit")))
        (should (string-match-p "== LINT (examples/pets.org) ==" out))
        (should (string-match-p "== OMN PARSE (profile=DL) ==" out))
        (should (string-match-p "== CONSISTENCY (reasoner=hermit) ==" out))
        (should (string-match-p
                 "== UNSATISFIABLE CLASSES ==\nSKIPPED: consistency reported no unsatisfiable classes"
                 out))
        (should (string-match-p "== SUMMARY ==" out))
        (should (string-match-p
                 (regexp-quote "OK: all checks pass (lint + OMN parse + DL + consistency)")
                 out))))))

(ert-deftest elot-gptel-check-test-lint-error-short-circuits ()
  "A lint *error* aborts downstream stages."
  (let ((elot-gptel-check-test--lint-result
         "190:1  [elot/nodeclare-id-prefix-label/high]  ERROR: No identifier found\nSummary: 1 error, 2 warnings")
        (elot-gptel-check-test--robot-available t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil nil nil)))
        (should (string-match-p "Summary: 1 error" out))
        (should (string-match-p
                 "== OMN PARSE ==\nSKIPPED: prior stage failed" out))
        (should (string-match-p
                 "== CONSISTENCY (reasoner=hermit) ==\nSKIPPED: prior stage failed"
                 out))
        (should (string-match-p
                 "== UNSATISFIABLE CLASSES ==\nSKIPPED: prior stage failed" out))
        (should (string-match-p "FAIL at stage: lint (1 error)" out))
        (should (string-match-p "HINT: fix lint errors" out))))))

(ert-deftest elot-gptel-check-test-lint-warning-does-not-block ()
  "Lint *warnings* alone do not abort the pipeline."
  (let ((elot-gptel-check-test--lint-result
         "1:1  [foo/bar/low]  WARNING: something\nSummary: 0 errors, 1 warning")
        (elot-gptel-check-test--robot-available t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil nil nil)))
        (should-not (string-match-p "FAIL at stage:" out))
        (should (string-match-p "OK: all checks pass" out))))))

(ert-deftest elot-gptel-check-test-omn-failure-short-circuits ()
  "An OMN-parse failure aborts the reasoning stages."
  (let ((elot-gptel-check-test--omn-result
         "Ontology my-ont:\nERROR  [robot/unknown]  INVALID ONTOLOGY FILE ERROR ...")
        (elot-gptel-check-test--robot-available t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil "DL" nil)))
        (should (string-match-p "INVALID ONTOLOGY FILE ERROR" out))
        (should (string-match-p
                 "== CONSISTENCY (reasoner=hermit) ==\nSKIPPED: prior stage failed"
                 out))
        (should (string-match-p "FAIL at stage: OMN parse" out))))))

(ert-deftest elot-gptel-check-test-inconsistent-short-circuits ()
  "An inconsistent ontology aborts the unsat-class enumeration."
  (let ((elot-gptel-check-test--consistency-result
         "INCONSISTENT [my-ont]: individual ex:spooky in disjoint classes")
        (elot-gptel-check-test--robot-available t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil nil "hermit")))
        (should (string-match-p "INCONSISTENT \\[my-ont\\]" out))
        (should (string-match-p
                 "== UNSATISFIABLE CLASSES ==\nSKIPPED: prior stage failed" out))
        (should (string-match-p "FAIL at stage: consistency" out))
        (should (string-match-p "explain `owl:Thing SubClassOf: owl:Nothing'" out))))))

(ert-deftest elot-gptel-check-test-unsat-flows-through ()
  "consistent-but-unsat triggers stage 4 which reports the IRIs."
  (let ((elot-gptel-check-test--consistency-result
         "OK: ontology is consistent but has unsatisfiable classes (reasoner=hermit, 1 node -- run elot_unsatisfiable for details)")
        (elot-gptel-check-test--unsat-result
         "Ontology pizza.owl (reasoner=hermit):\nUNSATISFIABLE CLASSES (1):\n  - :IceCream")
        (elot-gptel-check-test--robot-available t))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pizza.org" nil nil "hermit")))
        ;; Both stages 3 and 4 fired with their respective payloads.
        (should (string-match-p "consistent but has unsatisfiable" out))
        (should (string-match-p "UNSATISFIABLE CLASSES (1)" out))
        (should (string-match-p ":IceCream" out))
        ;; And -- crucially -- the SUMMARY is still OK overall (no failure).
        (should-not (string-match-p "FAIL at stage:" out))
        (should (string-match-p "OK: all checks pass" out))))))

(ert-deftest elot-gptel-check-test-no-robot-graceful ()
  "Without ROBOT, stages 2-4 are SKIPPED but a clean lint still summarises OK."
  (let ((elot-gptel-check-test--lint-result     "OK: no lint issues")
        (elot-gptel-check-test--robot-available nil))
    (elot-gptel-check-test--with-stubs
      (let ((out (elot-gptel-tool-check "examples/pets.org" nil nil nil)))
        (should (string-match-p "== LINT" out))
        (should (string-match-p
                 "== OMN PARSE ==\nSKIPPED: ROBOT not configured" out))
        (should (string-match-p
                 "== CONSISTENCY (reasoner=hermit) ==\nSKIPPED: ROBOT not configured"
                 out))
        (should (string-match-p
                 "== UNSATISFIABLE CLASSES ==\nSKIPPED: ROBOT not configured" out))
        (should (string-match-p
                 "OK: lint clean (ROBOT-backed stages SKIPPED)" out))))))

(ert-deftest elot-gptel-check-test-content-arg-forwarded ()
  "When CONTENT is supplied, lint and omn-validate receive it."
  (let* ((seen-lint nil)
         (seen-omn  nil)
         (draft "#+title: Draft\n* my-ont\n")
         (elot-gptel-check-test--robot-available t))
    (cl-letf (((symbol-function 'elot-gptel-tool-lint)
               (lambda (file _sev _cat content)
                 (setq seen-lint (list file content))
                 "OK: no lint issues"))
              ((symbol-function 'elot-gptel-tool-omn-validate)
               (lambda (file _prof content)
                 (setq seen-omn (list file content))
                 "OK: 1 ontology parsed"))
              ((symbol-function 'elot-gptel-tool-consistency)
               (lambda (&rest _) "OK: ontology is consistent (reasoner=hermit, 1 node)"))
              ((symbol-function 'elot-gptel-tool-unsatisfiable)
               (lambda (&rest _) "OK: no unsatisfiable classes (reasoner=hermit, 1 node)"))
              ((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'elot-robot-available-p) (lambda () t)))
      (elot-gptel-tool-check "drafts/new.org" draft nil nil)
      (should (equal seen-lint (list "drafts/new.org" draft)))
      (should (equal seen-omn  (list "drafts/new.org" draft))))))

(provide 'elot-gptel-check-test)
;;; elot-gptel-check-test.el ends here
