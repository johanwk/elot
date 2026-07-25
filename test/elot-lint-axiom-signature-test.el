;;; elot-lint-axiom-signature-test.el --- Tests for 9.2.b.0  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-lint-axiom-signature-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.b.0 -- pin the
;; existing signature/keyword lint diagnostics so 9.2.b can
;; rely on them.  Covers:
;;
;;  9.2.b.0.i   -- shared `elot-omn-kind-to-section-suffix' bridge
;;                 and `elot-omn-keywords-for-kind' helper.  The
;;                 keyword set for every kind must agree with the
;;                 gptel-side `elot-gptel--axiom-frame-keywords'
;;                 table from 9.2.a.
;;  9.2.b.0.ii  -- the undeclared-CURIE diagnostic includes the
;;                 actionable-fix suggestion.
;;  9.2.b.0.iii -- the fixture under test/fixtures exercises both
;;                 checkers (keyword-kind ERROR + undeclared-CURIE
;;                 WARNING) at the expected rows; `examples/pets.org'
;;                 emits no signature diagnostics.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-lint-axiom-signature-test--repo-root nil)
(defvar elot-lint-axiom-signature-test--fixtures nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-lint-axiom-signature-test--repo-root repo-root
        elot-lint-axiom-signature-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(require 'elot-gptel)
(require 'elot-lint)

(defun elot-lint-axiom-signature-test--lint-fixture ()
  "Run elot_lint on the signature fixture; return the report."
  (let* ((file (expand-file-name "lint-axiom-signature.org"
                                 elot-lint-axiom-signature-test--fixtures))
         (default-directory elot-lint-axiom-signature-test--repo-root))
    (elot-gptel-tool-lint file "all" nil)))


;;; ---------------------------------------------------------------------------
;;; 9.2.b.0.i -- shared kind <-> section bridge
;;; ---------------------------------------------------------------------------

(ert-deftest elot-lint-axiom-signature-test-bridge-exists ()
  (should (boundp 'elot-omn-kind-to-section-suffix))
  (should (fboundp 'elot-omn-keywords-for-kind))
  ;; Every gptel kind must resolve via the bridge to a non-empty
  ;; keyword set.  Order-independent equality.
  (dolist (kv elot-gptel--axiom-frame-keywords)
    (let* ((kind (car kv))
           (gptel-keys (mapcar #'car (cdr kv)))
           (lint-keys (elot-omn-keywords-for-kind kind)))
      (should lint-keys)
      (should (equal (sort (copy-sequence gptel-keys) #'string<)
                     (sort (copy-sequence lint-keys) #'string<))))))

(ert-deftest elot-lint-axiom-signature-test-bridge-unknown-kind ()
  (should-not (elot-omn-keywords-for-kind "owl:Bogus"))
  (should-not (elot-omn-keywords-for-kind nil)))


;;; ---------------------------------------------------------------------------
;;; 9.2.b.0.ii -- actionable diagnostic wording
;;; ---------------------------------------------------------------------------

(ert-deftest elot-lint-axiom-signature-test-undeclared-curie-message ()
  "ex:wolf is undeclared; the WARNING includes the suggested fix."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture)))
    (should (stringp out))
    (should (string-match-p "elot/axiom-value-curies" out))
    (should (string-match-p "Unknown CURIE in axiom: ex:wolf" out))
    (should (string-match-p "M-x elot-id-insert-sibling-resource" out))))


;;; ---------------------------------------------------------------------------
;;; 9.2.b.0.iii -- crafted-fixture diagnostics
;;; ---------------------------------------------------------------------------

(ert-deftest elot-lint-axiom-signature-test-domain-on-class ()
  "Domain :: ex:Fruit on a class -> keyword-appropriateness ERROR."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture)))
    (should (string-match-p "elot/omn-keyword-appropriateness" out))
    (should (string-match-p
             "\"Domain\" is not valid in Classes section" out))))

(ert-deftest elot-lint-axiom-signature-test-subpropchain-on-dp ()
  "SubPropertyChain on a data property -> keyword-appropriateness ERROR."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture)))
    (should (string-match-p
             "\"SubPropertyChain\" is not valid in Data properties section"
             out))))

(ert-deftest elot-lint-axiom-signature-test-xsd-string-clean ()
  "Range :: xsd:string on a DP must NOT raise an axiom-value-curies
warning (xsd:string is a built-in datatype CURIE)."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture))
        seen)
    (dolist (line (split-string out "\n" t))
      (when (and (string-match-p "elot/axiom-value-curies" line)
                 (string-match-p "xsd:string" line))
        (setq seen line)))
    (should-not seen)))

(ert-deftest elot-lint-axiom-signature-test-declared-ex-cat-clean ()
  "Range :: ex:cat on a DP must NOT raise axiom-value-curies
(ex:cat is declared).  The keyword-range refinement (9.2.b.0.iv)
catches the category mismatch separately -- see
`elot-lint-axiom-signature-test-keyword-range-ex-cat-on-dp-range'."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture))
        seen)
    (dolist (line (split-string out "\n" t))
      (when (and (string-match-p "elot/axiom-value-curies" line)
                 (string-match-p "Unknown CURIE in axiom: ex:cat" line))
        (setq seen line)))
    (should-not seen)))


;;; ---------------------------------------------------------------------------
;;; 9.2.b.0.iv -- elot/axiom-keyword-range value-kind checker
;;; ---------------------------------------------------------------------------

(ert-deftest elot-lint-axiom-signature-test-keyword-range-op-as-class ()
  "SubClassOf :: ex:isAfraidOf -- OP given where Class expected."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture)))
    (should (string-match-p "elot/axiom-keyword-range" out))
    (should (string-match-p
             "ex:isAfraidOf is an ObjectProperty but SubClassOf expects a Class"
             out))))

(ert-deftest elot-lint-axiom-signature-test-keyword-range-ex-cat-on-dp-range ()
  "Range :: ex:cat on a DataProperty -- Class given where Datatype expected."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture)))
    (should (string-match-p
             "ex:cat is a Class but Range expects a Datatype"
             out))))

(ert-deftest elot-lint-axiom-signature-test-keyword-range-xsd-clean ()
  "Range :: xsd:string on a DP must NOT raise keyword-range
(xsd:string is a built-in Datatype CURIE)."
  (let ((out (elot-lint-axiom-signature-test--lint-fixture))
        seen)
    (dolist (line (split-string out "\n" t))
      (when (and (string-match-p "elot/axiom-keyword-range" line)
                 (string-match-p "xsd:string" line))
        (setq seen line)))
    (should-not seen)))

(ert-deftest elot-lint-axiom-signature-test-keyword-range-pets-clean ()
  "examples/pets.org has no category mismatches."
  (let* ((pets (expand-file-name
                "examples/pets.org"
                elot-lint-axiom-signature-test--repo-root))
         (default-directory elot-lint-axiom-signature-test--repo-root)
         (out (elot-gptel-tool-lint pets "all" nil)))
    (should (stringp out))
    (should-not (string-match-p "elot/axiom-keyword-range" out))))


;;; ---------------------------------------------------------------------------
;;; Regression: examples/pets.org stays clean wrt signature diagnostics
;;; ---------------------------------------------------------------------------

(ert-deftest elot-lint-axiom-signature-test-pets-org-clean ()
  "examples/pets.org should not emit elot/omn-keyword-appropriateness
or undeclared-CURIE diagnostics from elot/axiom-value-curies."
  (let* ((pets (expand-file-name
                "examples/pets.org"
                elot-lint-axiom-signature-test--repo-root))
         (default-directory elot-lint-axiom-signature-test--repo-root)
         (out (elot-gptel-tool-lint pets "all" nil)))
    (should (stringp out))
    (should-not (string-match-p "elot/omn-keyword-appropriateness" out))
    (dolist (line (split-string out "\n" t))
      (when (string-match-p "elot/axiom-value-curies" line)
        (should-not (string-match-p "Unknown CURIE in axiom:" line))))))

(provide 'elot-lint-axiom-signature-test)
;;; elot-lint-axiom-signature-test.el ends here
