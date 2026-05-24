;;; elot-reasoning-test.el --- Tests for elot_unsatisfiable / elot_consistency  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-reasoning-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 5 Step 5.1 -- elot_unsatisfiable.
;; (Step 5.2 -- elot_consistency -- will land in this file too.)
;;
;; Pure tests run unconditionally; live tests skip when ROBOT is
;; unavailable.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-reasoning-test--repo-root repo-root))

(defvar elot-reasoning-test--repo-root nil)

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-reasoning-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

;;; ---------------------------------------------------------------------------
;;; Pure tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-reasoning-test-no-robot-error ()
  "When ROBOT is unavailable, the tool returns a structured ERROR line."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-unsatisfiable
                "test/fixtures/minimal-ontology.org" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-reasoning-test-unknown-reasoner ()
  "An unknown reasoner argument is rejected before ROBOT is invoked."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-unsatisfiable
                "test/fixtures/minimal-ontology.org" "pellet")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown reasoner" out)))))

(ert-deftest elot-reasoning-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-unsatisfiable "../../etc/passwd" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-reasoning-test-format-unsat-iris ()
  "`elot-gptel--format-unsat-iris' returns a bulleted list."
  (let ((s (elot-gptel--format-unsat-iris '("ex:Foo" "ex:Bar"))))
    (should (string-match-p "  - ex:Foo" s))
    (should (string-match-p "  - ex:Bar" s))))

;;; ---------------------------------------------------------------------------
;;; Live tests (skipped when ROBOT unavailable)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-reasoning-test-live-clean ()
  "The minimal fixture has no unsatisfiable classes under HermiT."
  (elot-reasoning-test--live-or-skip)
  (let* ((default-directory elot-reasoning-test--repo-root)
         (out (elot-gptel-tool-unsatisfiable
               "test/fixtures/minimal-ontology.org" "hermit")))
    (should (stringp out))
    (should (string-prefix-p "OK:" out))
    (should (string-match-p "reasoner=hermit" out))))

(defun elot-reasoning-test--write-unsat-fixture (path)
  "Write an OMN file at PATH containing one unsatisfiable class."
  (with-temp-file path
    (insert "Prefix: ex: <http://example.org/>\n"
            "Prefix: owl: <http://www.w3.org/2002/07/owl#>\n"
            "Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
            "\n"
            "Ontology: <http://example.org/unsat>\n"
            "\n"
            "Class: ex:A\n"
            "Class: ex:B\n"
            "DisjointClasses: ex:A, ex:B\n"
            "\n"
            "Class: ex:Foo\n"
            "    EquivalentTo: ex:A and ex:B\n")))

(ert-deftest elot-reasoning-test-live-unsat-omn ()
  "An OMN with a disjoint-class intersection yields an unsatisfiable class."
  (elot-reasoning-test--live-or-skip)
  (let* ((tmpdir (make-temp-file "elot-reasoning-test-" t))
         (omn (expand-file-name "unsat.omn" tmpdir))
         (out-path (expand-file-name "unsat-reasoned.ofn" tmpdir)))
    (unwind-protect
        (progn
          (elot-reasoning-test--write-unsat-fixture omn)
          ;; Drive `robot reason' directly to confirm classifier wiring.
          (let* ((res (elot-robot-run
                       (list "reason"
                             "--reasoner" "hermit"
                             "--input"    omn
                             "--output"   out-path)))
                 (exit (plist-get res :exit))
                 (err-raw (or (plist-get res :stderr) ""))
                 (out-raw (or (plist-get res :stdout) ""))
                 (effective (if (string-empty-p (string-trim err-raw))
                                out-raw err-raw)))
            ;; ROBOT exits non-zero on unsatisfiable classes.
            (should (numberp exit))
            (should-not (zerop exit))
            ;; Classifier or raw text must mention "unsatisfiable".
            (should (string-match-p "unsatisfiable\\|UNSATISFIABLE"
                                    effective))))
      (ignore-errors (delete-directory tmpdir t)))))

;;; ---------------------------------------------------------------------------
;;; elot_consistency (Step 5.2)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-reasoning-test-consistency-no-robot ()
  "When ROBOT is unavailable, `elot_consistency' returns an ERROR line."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-consistency
                "test/fixtures/minimal-ontology.org" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-reasoning-test-consistency-unknown-reasoner ()
  "An unknown reasoner argument is rejected before ROBOT is invoked."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-consistency
                "test/fixtures/minimal-ontology.org" "pellet")))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown reasoner" out)))))

(ert-deftest elot-reasoning-test-consistency-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-consistency "../../etc/passwd" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-reasoning-test-consistency-live-clean ()
  "The minimal fixture is consistent under HermiT."
  (elot-reasoning-test--live-or-skip)
  (let* ((default-directory elot-reasoning-test--repo-root)
         (out (elot-gptel-tool-consistency
               "test/fixtures/minimal-ontology.org" "hermit")))
    (should (stringp out))
    (should (string-prefix-p "OK:" out))
    (should (string-match-p "consistent" out))
    (should (string-match-p "reasoner=hermit" out))))

(defun elot-reasoning-test--write-inconsistent-fixture (path)
  "Write an OMN file at PATH containing a disjoint-class violation."
  (with-temp-file path
    (insert "Prefix: ex: <http://example.org/>\n"
            "Prefix: owl: <http://www.w3.org/2002/07/owl#>\n"
            "Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
            "\n"
            "Ontology: <http://example.org/inconsistent>\n"
            "\n"
            "Class: ex:A\n"
            "Class: ex:B\n"
            "DisjointClasses: ex:A, ex:B\n"
            "\n"
            "Individual: ex:x\n"
            "    Types: ex:A, ex:B\n")))

(ert-deftest elot-reasoning-test-consistency-live-inconsistent-omn ()
  "An individual in two disjoint classes makes the ontology inconsistent."
  (elot-reasoning-test--live-or-skip)
  (let* ((tmpdir (make-temp-file "elot-reasoning-test-" t))
         (omn (expand-file-name "inc.omn" tmpdir))
         (out-path (expand-file-name "inc-reasoned.ofn" tmpdir)))
    (unwind-protect
        (progn
          (elot-reasoning-test--write-inconsistent-fixture omn)
          (let* ((res (elot-robot-run
                       (list "reason"
                             "--reasoner" "hermit"
                             "--input"    omn
                             "--output"   out-path)))
                 (exit (plist-get res :exit))
                 (err-raw (or (plist-get res :stderr) ""))
                 (out-raw (or (plist-get res :stdout) ""))
                 (effective (if (string-empty-p (string-trim err-raw))
                                out-raw err-raw)))
            (should (numberp exit))
            (should-not (zerop exit))
            (should (string-match-p
                     "inconsistent\\|Inconsistent\\|INCONSISTENT"
                     effective))))
      (ignore-errors (delete-directory tmpdir t)))))

;;; ---------------------------------------------------------------------------
;;; elot_explain (Step 5.3)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-reasoning-test-explain-no-robot ()
  "When ROBOT is unavailable, `elot_explain' returns an ERROR line."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-explain
                "test/fixtures/minimal-ontology.org"
                "ex:Foo SubClassOf: ex:Animal" nil nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

(ert-deftest elot-reasoning-test-explain-missing-axiom ()
  "Missing axiom argument is refused with a structured ERROR."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-explain
                "test/fixtures/minimal-ontology.org" "" nil nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "axiom" out)))))

(ert-deftest elot-reasoning-test-explain-unknown-reasoner ()
  "An unknown reasoner argument is refused before ROBOT is invoked."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-explain
                "test/fixtures/minimal-ontology.org"
                "ex:A SubClassOf: ex:B" "pellet" nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown reasoner" out)))))

(ert-deftest elot-reasoning-test-explain-bad-max ()
  "Non-positive MAX is refused."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-explain
                "test/fixtures/minimal-ontology.org"
                "ex:A SubClassOf: ex:B" nil 0)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "max" out)))))

(ert-deftest elot-reasoning-test-explain-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-reasoning-test--repo-root))
    (let ((out (elot-gptel-tool-explain
                "../../etc/passwd" "ex:A SubClassOf: ex:B" nil nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out)))))

;;; ---------------------------------------------------------------------------
;;; Step 5.3 follow-up -- axiom rewriting and "No explanations" hint
;;; ---------------------------------------------------------------------------

(defun elot-reasoning-test--label-ht (pairs)
  "Build a CURIE -> rdfs:label hash-table from PAIRS.
PAIRS is a list of (CURIE LABEL) lists, matching the shape
`elot-codelist-from-slurp' produces (modulo the flatten)."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (p pairs ht)
      (puthash (car p) (cadr p) ht))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-passthrough ()
  "When no label is known, AXIOM passes through unchanged."
  (let* ((ht (elot-reasoning-test--label-ht nil))
         (res (elot-gptel--axiom-translate
               "ex:dog SubClassOf: ex:animal" ht nil)))
    (should (equal (car res) "ex:dog SubClassOf: ex:animal"))
    (should (null (cdr res)))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-curies ()
  "Known CURIEs are rewritten to their labels; SubClassOf: is left alone."
  (let* ((ht (elot-reasoning-test--label-ht
              '(("ex:dog" "Dog") ("ex:animal" "Animal"))))
         (res (elot-gptel--axiom-translate
               "ex:dog SubClassOf: ex:animal" ht nil)))
    (should (equal (car res) "Dog SubClassOf: Animal"))
    (should (equal (cdr res)
                   '(("ex:dog" . "Dog") ("ex:animal" . "Animal"))))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-iri ()
  "Angle-bracket IRIs are rewritten when a label is known.

The IRI is contracted via the prefix alist to its CURIE form,
then looked up in the label HT -- the same path `elot-slurp'
+ `org-link-abbrev-alist-local' provide in a live buffer."
  (let* ((ht (elot-reasoning-test--label-ht
              '(("ex:dog" "Dog"))))
         (prefixes '(("ex" . "http://example.org/")))
         (res (elot-gptel--axiom-translate
               "<http://example.org/dog> SubClassOf: <http://example.org/animal>"
               ht prefixes)))
    (should (equal (car res)
                   "Dog SubClassOf: <http://example.org/animal>"))
    (should (equal (cdr res)
                   '(("<http://example.org/dog>" . "Dog"))))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-keep-as-is ()
  "owl:Thing and owl:Nothing are passed through even when a label exists."
  (let* ((ht (elot-reasoning-test--label-ht
              '(("owl:Thing" "BOGUS") ("owl:Nothing" "BOGUS"))))
         (res (elot-gptel--axiom-translate
               "owl:Thing SubClassOf: owl:Nothing" ht nil)))
    (should (equal (car res) "owl:Thing SubClassOf: owl:Nothing"))
    (should (null (cdr res)))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-iri-default-prefix ()
  "A bare IRI is contracted via the default (empty) prefix and looked up.

Mirrors the pizza.org case where entities live under the
default prefix (`:IceCream') but the axiom mentions the full
http IRI."
  (let* ((ht (elot-reasoning-test--label-ht
              '((":IceCream" "IceCream"))))
         (prefixes '(("" . "https://example.org/pizza.owl#")))
         (res (elot-gptel--axiom-translate
               "<https://example.org/pizza.owl#IceCream> SubClassOf: owl:Nothing"
               ht prefixes)))
    (should (equal (car res) "IceCream SubClassOf: owl:Nothing"))
    (should (equal (cdr res)
                   '(("<https://example.org/pizza.owl#IceCream>"
                      . "IceCream"))))))

(ert-deftest elot-reasoning-test-explain-axiom-translate-quotes-label ()
  "Labels containing whitespace are single-quoted in the rewritten axiom."
  (let* ((ht (elot-reasoning-test--label-ht
              '(("ex:dog" "Working Dog"))))
         (res (elot-gptel--axiom-translate
               "ex:dog SubClassOf: Animal" ht nil)))
    (should (equal (car res) "'Working Dog' SubClassOf: Animal"))
    (should (equal (cdr res) '(("ex:dog" . "'Working Dog'"))))))

(ert-deftest elot-reasoning-test-explain-format-subs ()
  "`elot-gptel--explain-format-subs' renders a NOTE block; nil -> empty."
  (should (string-empty-p (elot-gptel--explain-format-subs nil)))
  (let ((s (elot-gptel--explain-format-subs
            '(("ex:dog" . "Dog") ("ex:animal" . "Animal")))))
    (should (string-match-p "NOTE:" s))
    (should (string-match-p "ex:dog -> Dog" s))
    (should (string-match-p "ex:animal -> Animal" s))))

(ert-deftest elot-reasoning-test-explain-augment-hint ()
  "`No explanations found.' gets a disambiguation HINT appended."
  (let ((md "## axiom ##\n\nNo explanations found.\n"))
    (let ((aug (elot-gptel--explain-augment md)))
      (should (string-match-p "No explanations found" aug))
      (should (string-match-p "HINT:" aug))
      (should (string-match-p "elot_consistency" aug))
      (should (string-match-p "elot_unsatisfiable" aug))))
  (let ((md "## axiom ##\n\nactual justification\n"))
    (should (equal md (elot-gptel--explain-augment md)))))

(provide 'elot-reasoning-test)
;;; elot-reasoning-test.el ends here
