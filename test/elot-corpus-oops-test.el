;;; elot-corpus-oops-test.el --- End-to-end harness for OOPS! corpus fixtures  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-corpus-oops-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 8 Step 8.2 -- generic, citation-bearing
;; harness over `test/fixtures/corpus/oops/'.
;;
;; For every fixture under the OOPS! sub-tree whose `:corpus-expects:'
;; is an `elot/oops-*' checker symbol, this harness:
;;
;;   1. Runs `elot-gptel-tool-lint' on the fixture.
;;   2. Asserts the expected checker name appears in the report.
;;   3. Asserts NO other `elot/oops-*' checker fires on the fixture
;;      (catches "lucky overlap" where one fixture accidentally
;;      demonstrates several pitfalls at once).
;;
;; The harness deliberately does NOT replace per-checker unit tests
;; under `test/elot-lint-oops-test.el' -- those exist to pin
;; edge-case behaviour (auto-derived label suppression, word-
;; boundary CURIE matching, language-tagged label collisions) that
;; a citation-bearing fixture would clutter.  See the plan's
;; "OOPS! fixtures for elot_lint" notes on the corpus vs. unit-test
;; layering.
;;
;; Fixtures arrive incrementally with their checkers:
;;
;;   - Sliver shipped already (Milestone 8 OOPS-for-lint sliver):
;;     P08 (elot/oops-missing-annotations),
;;     P24 (elot/oops-recursive-definition),
;;     P32 (elot/oops-duplicate-labels).
;;   - Planned alongside their (not-yet-implemented) checkers:
;;     P04, P11, P13, P19, P22 (see plan Step 8.2 stretch list).
;;
;; The harness is generic over fixture count; adding a new
;; `oops/PNN-*.org' fixture whose `:corpus-expects:' names an
;; already-registered checker symbol picks up automatically with
;; no Elisp change.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(defvar elot-corpus-oops-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (here (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path here)
  (setq elot-corpus-oops-test--repo-root repo-root))

(require 'elot-corpus-support)
(require 'elot-gptel)
(require 'elot-lint)
(require 'org-lint)

(defconst elot-corpus-oops-test--all-oops-checkers
  '(elot/oops-missing-annotations
    elot/oops-recursive-definition
    elot/oops-duplicate-labels)
  "The full set of `elot/oops-*' checkers currently shipped.
The harness asserts that for each fixture exactly the expected one
fires; every other checker in this list must stay silent.")

(defun elot-corpus-oops-test--lint (file)
  "Run elot_lint on FILE; return the report string."
  (let ((default-directory elot-corpus-oops-test--repo-root))
    (elot-gptel-tool-lint file "all" nil)))

(defun elot-corpus-oops-test--checker-line-p (line checker)
  "Return non-nil if LINE mentions CHECKER (a symbol)."
  (string-match-p (regexp-quote (symbol-name checker)) line))

(defun elot-corpus-oops-test--lines-for (out checker)
  "Return only the lines of OUT mentioning CHECKER (a symbol)."
  (seq-filter (lambda (l) (elot-corpus-oops-test--checker-line-p l checker))
              (split-string out "\n" t)))

(defun elot-corpus-oops-test--collect-cases ()
  "Return a list of (FILE . EXPECTED-CHECKER) for each oops/ fixture
whose `:corpus-expects:' is an `elot/oops-*' checker symbol."
  (let (cases)
    (dolist (file (elot-corpus-fixtures "oops"))
      (let* ((meta (elot-corpus-fixture-meta file))
             (exp  (plist-get meta :corpus-expects)))
        (when (and (stringp exp)
                   (eq (elot-corpus-expects-shape exp) 'checker-symbol)
                   (string-prefix-p "elot/oops-" exp))
          (push (cons file (intern exp)) cases))))
    (nreverse cases)))

;; -- Sanity --------------------------------------------------------------

(ert-deftest elot-corpus-oops-test-cases-present ()
  "At least one OOPS! fixture is wired up to a shipped checker."
  (let ((cases (elot-corpus-oops-test--collect-cases)))
    (should (>= (length cases) 1))
    (dolist (cell cases)
      (let ((expected (cdr cell)))
        (should (memq expected
                      elot-corpus-oops-test--all-oops-checkers))
        ;; Sanity: the symbol resolves to a registered org-lint checker.
        (should (seq-find (lambda (c)
                            (eq (org-lint-checker-name c) expected))
                          org-lint--checkers))))))

;; -- Generic harness ------------------------------------------------------

(defun elot-corpus-oops-test--assert-fixture (file expected)
  "Lint FILE; assert EXPECTED fires and no other elot/oops-* checker does.
Embeds the corpus citation in failure messages so the source of
the smell is visible at a glance."
  (let* ((meta (elot-corpus-fixture-meta file))
         (cite (elot-corpus-cite meta))
         (out  (elot-corpus-oops-test--lint file))
         (hits (elot-corpus-oops-test--lines-for out expected)))
    (should
     (or hits
         (ert-fail
          (format "expected checker %s did not fire on fixture %s\nreport:\n%s"
                  expected cite out))))
    (dolist (other elot-corpus-oops-test--all-oops-checkers)
      (unless (eq other expected)
        (let ((other-hits (elot-corpus-oops-test--lines-for out other)))
          (should
           (or (null other-hits)
               (ert-fail
                (format "unexpected checker %s also fired on fixture %s\nlines:\n%s\nfull report:\n%s"
                        other cite
                        (mapconcat #'identity other-hits "\n")
                        out)))))))))

(ert-deftest elot-corpus-oops-test-fixtures ()
  "Every shipped OOPS! fixture's expected checker fires, and only that
one fires (no lucky-overlap firings from sibling OOPS! checkers)."
  (dolist (cell (elot-corpus-oops-test--collect-cases))
    (let ((file (car cell))
          (expected (cdr cell)))
      (elot-corpus-oops-test--assert-fixture file expected))))

(provide 'elot-corpus-oops-test)
;;; elot-corpus-oops-test.el ends here
