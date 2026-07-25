;;; elot-lint-blank-omn-row-test.el --- Tests for elot/blank-omn-axiom-row  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-lint-blank-omn-row-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F4:
;; Tests for the `elot/blank-omn-axiom-row' lint checker.
;;
;; Drives `elot-gptel-tool-lint' against
;; test/fixtures/blank-omn-axiom-row.org and asserts:
;;
;;  - The warning fires on `ex:chases' (Domain :: <blank>),
;;    `ex:chases' (Range :: <blank>), `ex:animal'
;;    (DisjointClasses :: <blank>), and `ex:dog'
;;    (SubClassOf :: <blank>).
;;  - It does NOT fire on `ex:bites' (populated Domain / Range).
;;  - It does NOT fire on `ex:cat' (blank rdfs:comment / skos:example
;;    -- annotation rows are out of scope).
;;  - The checker is registered with org-lint.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-lint-blank-omn-row-test--repo-root nil)
(defvar elot-lint-blank-omn-row-test--fixtures nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-lint-blank-omn-row-test--repo-root repo-root
        elot-lint-blank-omn-row-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(require 'elot-gptel)

(defun elot-lint-blank-omn-row-test--lint ()
  "Run elot_lint on the fixture; return the report string."
  (let* ((file (expand-file-name "blank-omn-axiom-row.org"
                                 elot-lint-blank-omn-row-test--fixtures))
         (default-directory elot-lint-blank-omn-row-test--repo-root))
    (elot-gptel-tool-lint file "all" nil)))

(defun elot-lint-blank-omn-row-test--blank-omn-lines (out)
  "Return only the lines of OUT mentioning the new checker."
  (let ((lines (split-string out "\n" t)))
    (seq-filter (lambda (line)
                  (string-match-p "elot/blank-omn-axiom-row" line))
                lines)))

(ert-deftest elot-lint-blank-omn-row-test-checker-registered ()
  "The new checker is actually registered with org-lint."
  (require 'elot-lint)
  (require 'org-lint)
  (should (seq-find (lambda (c)
                      (eq (org-lint-checker-name c)
                          'elot/blank-omn-axiom-row))
                    org-lint--checkers)))

(ert-deftest elot-lint-blank-omn-row-test-fires-for-blank-domain ()
  "Warning fires for `ex:chases' / Domain :: <blank>."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (stringp out))
    (should (seq-some (lambda (l)
                        (string-match-p "Domain" l))
                      lines))))

(ert-deftest elot-lint-blank-omn-row-test-fires-for-blank-range ()
  "Warning fires for `ex:chases' / Range :: <blank>."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (seq-some (lambda (l)
                        (string-match-p "Range" l))
                      lines))))

(ert-deftest elot-lint-blank-omn-row-test-fires-for-blank-disjointclasses ()
  "Warning fires for `ex:animal' / DisjointClasses :: <blank> (misc keyword)."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (seq-some (lambda (l)
                        (string-match-p "DisjointClasses" l))
                      lines))))

(ert-deftest elot-lint-blank-omn-row-test-fires-for-blank-subclassof ()
  "Warning fires for `ex:dog' / SubClassOf :: <blank>."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (seq-some (lambda (l)
                        (string-match-p "SubClassOf" l))
                      lines))))

(ert-deftest elot-lint-blank-omn-row-test-message-shape ()
  "The warning message names the keyword and suggests populate/remove."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (seq-some (lambda (l)
                        (and (string-match-p "Blank OMN axiom row" l)
                             (string-match-p "populate" l)))
                      lines))))

(ert-deftest elot-lint-blank-omn-row-test-silent-on-populated-rows ()
  "No warning for populated `Domain :: ex:dog' / `Range :: ex:human'."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    ;; The literal CURIE values from the populated rows must not
    ;; appear in any blank-omn-axiom-row line.
    (dolist (line lines)
      (should-not (string-match-p "ex:dog\\|ex:human" line)))))

(ert-deftest elot-lint-blank-omn-row-test-silent-on-blank-annotation ()
  "No warning for blank `rdfs:comment ::' or `skos:example ::' rows."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (dolist (line lines)
      (should-not (string-match-p "rdfs:comment\\|skos:example" line)))))

(ert-deftest elot-lint-blank-omn-row-test-fires-four-times ()
  "Exactly four blank-OMN-axiom-row warnings fire on the fixture."
  (let* ((out (elot-lint-blank-omn-row-test--lint))
         (lines (elot-lint-blank-omn-row-test--blank-omn-lines out)))
    (should (= 4 (length lines)))))

(provide 'elot-lint-blank-omn-row-test)
;;; elot-lint-blank-omn-row-test.el ends here
