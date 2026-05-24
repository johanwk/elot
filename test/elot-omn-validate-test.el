;;; elot-omn-validate-test.el --- Tests for the elot_omn_validate tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-omn-validate-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 3 Steps 3.1 + 3.2:
;;
;;   elot-omn-validate-test-no-robot-error    -- structured error when
;;                                                ROBOT is unavailable
;;   elot-omn-validate-test-bad-profile       -- profile argument is
;;                                                validated up front
;;   elot-omn-validate-test-traversal         -- path containment still
;;                                                applies
;;   elot-omn-validate-test-live-clean        -- live ROBOT round-trip
;;                                                on the minimal fixture
;;                                                (skipped without ROBOT)
;;
;; Live tests auto-skip when ROBOT is unavailable; the others are pure.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-omn-validate-test--repo-root repo-root
        elot-omn-validate-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(defvar elot-omn-validate-test--repo-root nil)
(defvar elot-omn-validate-test--fixtures nil)

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-omn-validate-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

;;; ---------------------------------------------------------------------------
;;; Pure tests (always run)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-omn-validate-test-no-robot-error ()
  "When ROBOT is unavailable, the tool returns a structured ERROR line."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-omn-validate-test--repo-root))
    (let ((out (elot-gptel-tool-omn-validate
                "test/fixtures/minimal-ontology.org" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-omn-validate-test-bad-profile ()
  "An unknown profile value is rejected before ROBOT is invoked."
  (let ((default-directory elot-omn-validate-test--repo-root))
    (let ((out (elot-gptel-tool-omn-validate
                "test/fixtures/minimal-ontology.org" "Bogus")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown profile" out)))))

(ert-deftest elot-omn-validate-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-omn-validate-test--repo-root))
    (let ((out (elot-gptel-tool-omn-validate "../../etc/passwd" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

;;; ---------------------------------------------------------------------------
;;; Live tests (skipped when ROBOT unavailable)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-omn-validate-test-live-clean ()
  "The minimal fixture tangles to OMN and ROBOT parses it cleanly."
  (elot-omn-validate-test--live-or-skip)
  (let* ((default-directory elot-omn-validate-test--repo-root)
         (out (elot-gptel-tool-omn-validate
               "test/fixtures/minimal-ontology.org" nil)))
    (should (stringp out))
    (should (string-prefix-p "OK:" out))))

(ert-deftest elot-omn-validate-test-live-clean-with-profile ()
  "Profile validation succeeds on the minimal fixture (DL)."
  (elot-omn-validate-test--live-or-skip)
  (let* ((default-directory elot-omn-validate-test--repo-root)
         (out (elot-gptel-tool-omn-validate
               "test/fixtures/minimal-ontology.org" "DL")))
    (should (stringp out))
    (should (string-prefix-p "OK:" out))
    (should (string-match-p "profile DL" out))))

;;; ---------------------------------------------------------------------------
;;; Source-map tests (pure, Step 3.3)
;;; ---------------------------------------------------------------------------

(require 'elot-tangle)

(ert-deftest elot-omn-validate-test-source-map-from-string ()
  "`elot-omn-line-map-from-string' records frame openers with line numbers."
  (let* ((omn (concat
               "Prefix: ex: <http://example.org/>\n"
               "\n"
               "Ontology: <http://example.org/o>\n"
               "\n"
               "Class: ex:Foo\n"
               "    Annotations:\n"
               "        rdfs:label \"Foo\"\n"
               "\n"
               "ObjectProperty: ex:bar\n"))
         (alist '(("ex:Foo" . 100) ("ex:bar" . 200)))
         (map (elot-omn-line-map-from-string omn alist)))
    (should (= 3 (length map)))
    ;; Ontology frame: line 3.
    (let ((e (nth 0 map)))
      (should (= 3 (plist-get e :omn-line)))
      (should (equal "Ontology" (plist-get e :frame))))
    ;; Class frame: line 5, mapped to org-line 100.
    (let ((e (nth 1 map)))
      (should (= 5 (plist-get e :omn-line)))
      (should (equal "Class" (plist-get e :frame)))
      (should (equal "ex:Foo" (plist-get e :uri)))
      (should (= 100 (plist-get e :org-line))))
    ;; ObjectProperty frame: line 9, mapped to org-line 200.
    (let ((e (nth 2 map)))
      (should (= 9 (plist-get e :omn-line)))
      (should (equal "ObjectProperty" (plist-get e :frame)))
      (should (= 200 (plist-get e :org-line))))))

(ert-deftest elot-omn-validate-test-source-map-lookup ()
  "`elot-omn-lookup-org-line' returns the nearest preceding frame anchor."
  (let ((map '((:omn-line 5  :uri "ex:Foo" :org-line 100 :frame "Class")
               (:omn-line 9  :uri "ex:bar" :org-line 200 :frame "ObjectProperty"))))
    ;; Mid-Foo frame: maps to ex:Foo / line 100.
    (let ((a (elot-omn-lookup-org-line 7 map)))
      (should (equal "ex:Foo" (car a)))
      (should (= 100 (cdr a))))
    ;; On the bar opener: maps to ex:bar / line 200.
    (let ((a (elot-omn-lookup-org-line 9 map)))
      (should (equal "ex:bar" (car a))))
    ;; Before any frame: nil.
    (should (null (elot-omn-lookup-org-line 1 map)))))

(ert-deftest elot-omn-validate-test-source-map-sidecar ()
  "Sidecar map file is written and round-trips line numbers."
  (let* ((tmpdir (make-temp-file "elot-omn-map-" t))
         (path (expand-file-name "x.omn.map" tmpdir))
         (map '((:omn-line 5  :uri "ex:Foo" :org-line 100 :frame "Class")
                (:omn-line 9  :uri "ex:bar" :org-line 200 :frame "ObjectProperty"))))
    (unwind-protect
        (progn
          (elot-omn-write-sidecar-map path map "/some/where/foo.org")
          (should (file-exists-p path))
          (with-temp-buffer
            (insert-file-contents path)
            (let ((s (buffer-string)))
              (should (string-match-p "^# elot OMN source map" s))
              (should (string-match-p "foo.org" s))
              (should (string-match-p "^5\t100\tex:Foo\tClass$" s))
              (should (string-match-p "^9\t200\tex:bar\tObjectProperty$" s)))))
      (delete-directory tmpdir t))))

;;; ---------------------------------------------------------------------------
;;; Misuse fixture (Step 3.4)
;;;
;;; The fixture `test/fixtures/op-in-class-position.org' uses an object
;;; property where a class is expected.  ELOT's lint cannot catch this
;;; (the CURIE is declared and Manchester syntax parses cleanly); ROBOT
;;; must, because the OWL API rejects the category error at parse time.
;;; ---------------------------------------------------------------------------

(require 'elot-lint)

(ert-deftest elot-omn-validate-test-misuse-lint-passes ()
  "ELOT lint does not flag the OP-in-class-position fixture."
  (let* ((default-directory elot-omn-validate-test--repo-root)
         (out (elot-gptel-tool-lint
               "test/fixtures/op-in-class-position.org" "all" nil)))
    (should (stringp out))
    ;; The fixture must not provoke axiom-value-curies / omn-syntax /
    ;; description-list-curies / ontology-declaration-heading.  It may
    ;; still produce structural warnings (e.g. naming conventions) that
    ;; are unrelated to the category error we are testing for.
    (should-not (string-match-p "elot/axiom-value-curies" out))
    (should-not (string-match-p "elot/omn-syntax" out))
    (should-not (string-match-p "elot/description-list-curies" out))
    (should-not (string-match-p "elot/ontology-declaration-heading" out))))

(ert-deftest elot-omn-validate-test-misuse-robot-catches ()
  "ROBOT convert rejects the object-property-in-class-position fixture."
  (elot-omn-validate-test--live-or-skip)
  (let* ((default-directory elot-omn-validate-test--repo-root)
         (out (elot-gptel-tool-omn-validate
               "test/fixtures/op-in-class-position.org" nil)))
    (should (stringp out))
    ;; Must surface as an error (any classifier kind is acceptable so
    ;; long as it is not the OK path).
    (should-not (string-prefix-p "OK:" out))
    (should (or (string-match-p "ERROR" out)
                (string-match-p "robot/" out)))
    ;; ROBOT's `convert' is terse for this category error: it reports
    ;; only "INVALID ONTOLOGY FILE ERROR" with no property name.  We
    ;; assert the report identifies the offending OMN file -- the
    ;; structural rejection is the signal under test; richer
    ;; diagnostics (naming `hasPart') would require DL profile
    ;; validation or a reasoner, not `convert' alone.
    (should (string-match-p "op-in-class" out))))

(provide 'elot-omn-validate-test)
;;; elot-omn-validate-test.el ends here
