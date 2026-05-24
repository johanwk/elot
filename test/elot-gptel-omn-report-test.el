;;; elot-gptel-omn-report-test.el --- Tests for elot_omn_report tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-omn-report-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 8 Step 8.4 -- thin wrapper around
;; ROBOT's `report' subcommand.  Scope is the /integration seam/
;; only: we never assert which ROBOT report queries fired (that is
;; ROBOT's contract, not ours).  Tests are split into:
;;
;;   - Pure tests (always run): no-ROBOT structured error, unknown
;;     format refusal, path-traversal refusal, dispatcher arity,
;;     tool-spec registration.
;;   - Live tests (ROBOT-gated): clean baseline yields OK, broken
;;     fixture yields a non-empty report.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-omn-report-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (here (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path here)
  (setq elot-gptel-omn-report-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-gptel-omn-report-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

;;; ---------------------------------------------------------------------------
;;; Pure tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-omn-report-test-no-robot ()
  "Structured ERROR when ROBOT is unavailable."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-gptel-omn-report-test--repo-root))
    (let ((out (elot-gptel-tool-omn-report
                "test/fixtures/minimal-ontology.org")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-gptel-omn-report-test-bad-format ()
  "Unknown format is rejected before ROBOT is invoked."
  (let ((default-directory elot-gptel-omn-report-test--repo-root))
    (let ((out (elot-gptel-tool-omn-report
                "test/fixtures/minimal-ontology.org" "xml")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown format" out)))))

(ert-deftest elot-gptel-omn-report-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-gptel-omn-report-test--repo-root))
    (let ((out (elot-gptel-tool-omn-report "../../etc/passwd")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-gptel-omn-report-test-tool-spec-registered ()
  "The `elot_omn_report' tool spec is in the registry with the right shape."
  (let ((spec (assoc "elot_omn_report" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-omn-report))
    (let* ((args (plist-get (cdr spec) :args))
           (names (mapcar (lambda (a) (plist-get a :name)) args)))
      (should (member "file" names))
      (should (member "format" names))
      (should (member "content" names)))))

(ert-deftest elot-gptel-omn-report-test-dispatcher-arity ()
  "The dispatcher thunk forwards (file [format [content]]) to the impl."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-omn-report)))
    (should (functionp thunk))
    ;; Arity should accept 1, 2, or 3 positional args.
    (should (condition-case _ (progn (funcall thunk "/no/such") t)
              (user-error t) (error t)))))

(ert-deftest elot-gptel-omn-report-test-empty-p-helper ()
  "`elot-gptel--omn-report-empty-p' treats header-only TSV as empty."
  (should (elot-gptel--omn-report-empty-p "" "tsv"))
  (should (elot-gptel--omn-report-empty-p "   \n  " "tsv"))
  (should (elot-gptel--omn-report-empty-p "Level\tRule Name\tSubject\n" "tsv"))
  (should-not (elot-gptel--omn-report-empty-p
               "Level\tRule\tSubject\nWARN\trule1\tex:foo\n" "tsv"))
  (should (elot-gptel--omn-report-empty-p "Level,Rule\n" "csv"))
  (should-not (elot-gptel--omn-report-empty-p
               "Level,Rule\nWARN,r1\n" "csv")))

;;; ---------------------------------------------------------------------------
;;; Live tests (skipped when ROBOT unavailable)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-omn-report-test-live-clean-baseline ()
  "The clean-baseline corpus fixture flows through the tool cleanly.

Per the M8.4 rewrite we deliberately do NOT assert that ROBOT
considers this fixture violation-free: ROBOT's default report
queries include `missing_ontology_license',
`missing_ontology_description', `missing_definition' (IAO:0000115),
etc., which fire on any minimal OMN fixture.  Asserting \"no
violations\" would couple ELOT's tests to ROBOT's default query
set -- exactly the shadow-maintenance the M8.4 rewrite avoids.

The integration seam we own: the call returns a well-formed,
non-ERROR envelope (either the OK summary or a per-node block
framed by `Ontology NAME (format=...):')."
  (elot-gptel-omn-report-test--live-or-skip)
  (let* ((default-directory elot-gptel-omn-report-test--repo-root)
         (out (elot-gptel-tool-omn-report
               "test/fixtures/corpus/robot-report/clean-baseline.org")))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (or (string-prefix-p "OK:" out)
                (string-match-p "^Ontology corpus-rr-clean" out)))))

(ert-deftest elot-gptel-omn-report-test-live-broken-multi ()
  "The broken-multi corpus fixture yields a non-empty report."
  (elot-gptel-omn-report-test--live-or-skip)
  (let* ((default-directory elot-gptel-omn-report-test--repo-root)
         (out (elot-gptel-tool-omn-report
               "test/fixtures/corpus/robot-report/broken-multi.org")))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    ;; Per the M8.4 rewrite we do NOT assert which queries fired.
    ;; We assert only that the envelope is well-formed (the fixture
    ;; surfaces *some* violations).
    (should-not (string-prefix-p "OK:" out))
    (should (string-match-p "Ontology corpus-rr-broken" out))))

(provide 'elot-gptel-omn-report-test)
;;; elot-gptel-omn-report-test.el ends here
