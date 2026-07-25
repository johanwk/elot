;;; elot-lint-subclass-in-dl-test.el --- Tests for elot/subclass-in-description-list  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-lint-subclass-in-dl-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.4:
;; Tests for the `elot/subclass-in-description-list' lint checker.
;;
;; Drives `elot-gptel-tool-lint' against
;; test/fixtures/subclass-in-description-list.org and asserts:
;;
;;  - The warning fires on `ex:Apple' (SubClassOf :: ex:Fruit) where
;;    ex:Fruit is declared elsewhere as a heading.
;;  - It does NOT fire on `ex:Sour' (anonymous class expression).
;;  - It does NOT fire on `ex:Pear' (SubClassOf :: ex:Unknown -- not
;;    declared, so out of scope of THIS checker; axiom-value-curies
;;    handles that).
;;  - It does NOT fire on `ex:Pippin' (level-4, not a direct child of
;;    a :resourcedefs: heading).

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-lint-subclass-in-dl-test--repo-root nil)
(defvar elot-lint-subclass-in-dl-test--fixtures nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-lint-subclass-in-dl-test--repo-root repo-root
        elot-lint-subclass-in-dl-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(require 'elot-gptel)

(defun elot-lint-subclass-in-dl-test--lint ()
  "Run elot_lint on the fixture; return the report string."
  (let* ((file (expand-file-name "subclass-in-description-list.org"
                                 elot-lint-subclass-in-dl-test--fixtures))
         (default-directory elot-lint-subclass-in-dl-test--repo-root))
    (elot-gptel-tool-lint file "all" nil)))

(ert-deftest elot-lint-subclass-in-dl-test-fires-for-named-parent ()
  "The warning fires on `ex:Apple' (SubClassOf :: ex:Fruit)."
  (let ((out (elot-lint-subclass-in-dl-test--lint)))
    (should (stringp out))
    (should (string-match-p "elot/subclass-in-description-list" out))
    (should (string-match-p "SubClassOf :: ex:Fruit" out))
    (should (string-match-p "heading nesting" out))))

(ert-deftest elot-lint-subclass-in-dl-test-skips-anonymous-expression ()
  "No warning on `ex:Sour' (anonymous class expression)."
  (let ((out (elot-lint-subclass-in-dl-test--lint)))
    (should (stringp out))
    ;; The fixture's only `ex:Fruit and ...' axiom is on ex:Sour.
    ;; Asserting that pattern is absent from the lint output is the
    ;; cleanest way to express "ex:Sour's SubClassOf row was not flagged".
    (should-not (string-match-p "ex:Fruit and ex:hasTaste" out))))

(ert-deftest elot-lint-subclass-in-dl-test-skips-unknown-curie ()
  "No subclass-in-DL warning on `ex:Pear' (SubClassOf :: ex:Unknown);
ex:Unknown is not declared anywhere as a heading, so this checker is
out of scope.  (axiom-value-curies handles the unknown-CURIE case.)"
  (let ((out (elot-lint-subclass-in-dl-test--lint)))
    (should (stringp out))
    ;; The literal substring "SubClassOf :: ex:Unknown" must NOT appear
    ;; in any elot/subclass-in-description-list line.  Other checkers
    ;; may legitimately mention ex:Unknown; we only suppress on the
    ;; checker name appearing together with the Unknown CURIE.
    (let ((lines (split-string out "\n" t)))
      (dolist (line lines)
        (when (string-match-p "elot/subclass-in-description-list" line)
          (should-not (string-match-p "ex:Unknown" line)))))))

(ert-deftest elot-lint-subclass-in-dl-test-skips-deeper-nesting ()
  "No warning on `ex:Pippin' -- a level-4 heading under ex:Apple, not a
direct child of the level-2 :resourcedefs: Classes section.  Its
`SubClassOf :: ex:Apple' row is out of scope for this checker."
  (let ((out (elot-lint-subclass-in-dl-test--lint)))
    (should (stringp out))
    (let ((lines (split-string out "\n" t)))
      (dolist (line lines)
        (when (string-match-p "elot/subclass-in-description-list" line)
          (should-not (string-match-p "SubClassOf :: ex:Apple" line)))))))

(ert-deftest elot-lint-subclass-in-dl-test-checker-registered ()
  "The new checker is actually registered with org-lint."
  (require 'elot-lint)
  (require 'org-lint)
  (should (seq-find (lambda (c)
                      (eq (org-lint-checker-name c)
                          'elot/subclass-in-description-list))
                    org-lint--checkers)))

(provide 'elot-lint-subclass-in-dl-test)
;;; elot-lint-subclass-in-dl-test.el ends here
