;;; elot-corpus-test.el --- Meta-test for the ELOT external-quality corpus  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-corpus-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 8 Step 8.1 -- meta-test for
;; `test/fixtures/corpus/'.  Asserts that:
;;
;;   1. Every fixture under the corpus tree has a well-formed
;;      provenance drawer (:corpus-source, :corpus-id,
;;      :corpus-url, :corpus-expects all present).
;;
;;   2. Every :corpus-expects value either is "none" (clean
;;      control), names a registered org-lint checker, or
;;      matches a known pitfall id BACKEND/ID registered with
;;      `elot-corpus-register-pitfall'.
;;
;;   3. :corpus-source is one of `elot-corpus-known-sources'.
;;
;; The test tolerates an initially-empty tree without failing:
;; if no fixtures are discovered, only the support API is
;; exercised.  Per Step 8.1 we ship two oops/ fixtures and one
;; clean control, so the meta-test runs against a non-empty
;; list from first commit.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(let* ((this-file (or load-file-name buffer-file-name))
       (here (file-name-directory this-file))
       (repo-root (file-name-directory (directory-file-name here))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path here))

(require 'elot-corpus-support)
;; Pull in `org-lint--checkers' so checker-symbol expectations resolve.
(require 'elot-lint nil t)
(require 'org-lint)

;; -- Support API smoke tests ----------------------------------------------

(ert-deftest elot-corpus-test-support-fixtures-returns-list ()
  "`elot-corpus-fixtures' returns a (possibly empty) list of strings."
  (let ((all (elot-corpus-fixtures)))
    (should (listp all))
    (dolist (f all)
      (should (stringp f))
      (should (file-readable-p f)))))

(ert-deftest elot-corpus-test-support-subdir-filter ()
  "`elot-corpus-fixtures' with SUBDIR restricts to that sub-tree."
  (let* ((all (elot-corpus-fixtures))
         (oops (elot-corpus-fixtures "oops")))
    (dolist (f oops)
      (should (string-match-p "/corpus/oops/" f))
      (should (member f all)))))

(ert-deftest elot-corpus-test-support-readme-excluded ()
  "README.org is never returned by `elot-corpus-fixtures'."
  (dolist (f (elot-corpus-fixtures))
    (should-not (string-equal "README.org" (file-name-nondirectory f)))))

(ert-deftest elot-corpus-test-support-meta-roundtrip ()
  "`elot-corpus-fixture-meta' returns a plist with :file set."
  (let ((fixtures (elot-corpus-fixtures)))
    (when fixtures
      (let ((meta (elot-corpus-fixture-meta (car fixtures))))
        (should (plistp meta))
        (should (stringp (plist-get meta :file)))))))

(ert-deftest elot-corpus-test-cite-non-empty ()
  "`elot-corpus-cite' produces a non-empty citation string."
  (let ((s (elot-corpus-cite
            '(:corpus-source "OOPS!" :corpus-id "P32"
              :corpus-severity "Minor"
              :corpus-url "https://oops.linkeddata.es/catalogue.jsp#P32"
              :file "/tmp/x.org"))))
    (should (stringp s))
    (should (string-match-p "OOPS!" s))
    (should (string-match-p "P32" s))))

;; -- Per-fixture meta-test -------------------------------------------------

(defun elot-corpus-test--assert-fixture (file)
  "Assert FILE has a well-formed drawer and resolvable :corpus-expects."
  (let* ((meta (elot-corpus-fixture-meta file))
         (cite (elot-corpus-cite meta))
         (src  (plist-get meta :corpus-source))
         (exp  (plist-get meta :corpus-expects)))
    ;; (1) well-formedness
    (should
     (or (elot-corpus-meta-well-formed-p meta)
         (ert-fail (format "corpus drawer incomplete: %s  meta=%S"
                           cite meta))))
    ;; (2) :corpus-source is a recognised authority
    (should
     (or (member src elot-corpus-known-sources)
         (ert-fail (format "corpus-source %S not in %S  %s"
                           src elot-corpus-known-sources cite))))
    ;; (3) :corpus-expects resolves
    (should
     (or (elot-corpus-expects-valid-p exp)
         (ert-fail (format "corpus-expects %S does not resolve  %s"
                           exp cite))))))

(ert-deftest elot-corpus-test-every-fixture-well-formed ()
  "Every corpus fixture has a parseable, well-formed provenance drawer
and a resolvable `:corpus-expects:' value."
  (let ((fixtures (elot-corpus-fixtures)))
    ;; Tolerate empty tree; otherwise check each file.
    (dolist (f fixtures)
      (elot-corpus-test--assert-fixture f))))

(ert-deftest elot-corpus-test-step-8-1-baseline-present ()
  "Step 8.1 ships at least one OOPS! fixture and one clean control."
  (let ((oops (elot-corpus-fixtures "oops"))
        (clean (elot-corpus-fixtures "clean")))
    (should (>= (length oops) 1))
    (should (>= (length clean) 1))))

(provide 'elot-corpus-test)
;;; elot-corpus-test.el ends here
