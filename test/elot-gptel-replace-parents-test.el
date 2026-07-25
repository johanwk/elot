;;; elot-gptel-replace-parents-test.el --- M9.9.F1 parent-enum tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-replace-parents-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9.F1 -- tests for the
;; parent-enumeration helper `elot-gptel--replace--immediate-parents'.
;;
;; The helper answers "what are SUBJECT's immediate parents?" -- the
;; guard at the heart of `elot_replace_with_parent'.  Pure read; no
;; tool spec, no dispatcher entry, no rename call yet.
;;
;; Covers:
;;   - unknown subject -> nil
;;   - leaf class with heading-nested parent only -> single CURIE
;;   - explicit `SubClassOf :: ex:foo' row + different heading-nested
;;     parent -> two CURIEs, heading-nested first
;;   - SubClassOf row value equals heading-nested parent -> dedup
;;   - top-level class directly under section root -> empty list
;;   - class expressions on a SubClassOf row -> rejected
;;     (bare-CURIE matcher; mirrors the lint tokeniser stance)

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-rwp-test--repo-root nil)
(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this))
  (setq elot-gptel-rwp-test--repo-root root))

(require 'elot-tangle)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-rwp-test--fixture ()
  "Pets-shaped fixture exercising every parent-enumeration branch.

  ex:animal      -- top-level under section root: no parents
  ex:dog         -- nested under ex:animal: heading-nested only
  ex:beagle      -- nested under ex:dog AND `SubClassOf :: ex:dog':
                   dedup, single CURIE
  ex:puppy       -- top-level under section root + `SubClassOf ::
                   ex:dog': row-only parent
  ex:hybrid      -- nested under ex:animal AND `SubClassOf :: ex:dog':
                   two parents, heading-nested first
  ex:expr        -- top-level + `SubClassOf :: ex:foo and ex:bar':
                   class-expression rejected -> nil
  ex:chases      -- object property with no parent rows"
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: slug\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Prefixes\n"
   ":PROPERTIES:\n:prefixdefs: yes\n:END:\n"
   "#+name: prefix-table\n"
   "| prefix | uri                                   |\n"
   "|--------+---------------------------------------|\n"
   "| owl:   | http://www.w3.org/2002/07/owl#        |\n"
   "| rdfs:  | http://www.w3.org/2000/01/rdf-schema# |\n"
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "**** Dog (ex:dog)\n"
   "***** Beagle (ex:beagle)\n"
   " - SubClassOf :: ex:dog\n"
   "**** Hybrid (ex:hybrid)\n"
   " - SubClassOf :: ex:dog\n"
   "*** Puppy (ex:puppy)\n"
   " - SubClassOf :: ex:dog\n"
   "*** Expr (ex:expr)\n"
   " - SubClassOf :: ex:foo and ex:bar\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:animal\n"))

(defmacro elot-gptel-rwp-test--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-gptel-rwp-test--fixture))
     (org-mode)
     (elot-update-headline-hierarchy)
     (goto-char (point-min))
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rwp-test-defined ()
  (should (fboundp 'elot-gptel--replace--immediate-parents)))

(ert-deftest elot-gptel-rwp-test-unknown-subject ()
  (elot-gptel-rwp-test--with
    (should (null (elot-gptel--replace--immediate-parents "ex:nope")))
    (should (null (elot-gptel--replace--immediate-parents "")))
    (should (null (elot-gptel--replace--immediate-parents nil)))))

(ert-deftest elot-gptel-rwp-test-top-level-no-parents ()
  "ex:animal sits directly under the section root, which carries no
`:uri' -- so it has no heading-nested resource parent, and no
SubClassOf row -- so no parents at all."
  (elot-gptel-rwp-test--with
    (should (null (elot-gptel--replace--immediate-parents "ex:animal")))))

(ert-deftest elot-gptel-rwp-test-heading-nested-only ()
  "ex:dog is nested under ex:animal; no SubClassOf row.  Single
parent from heading nesting."
  (elot-gptel-rwp-test--with
    (should (equal '("ex:animal")
                   (elot-gptel--replace--immediate-parents "ex:dog")))))

(ert-deftest elot-gptel-rwp-test-row-only-parent ()
  "ex:puppy sits directly under the section root (no resource
heading-parent) but carries `SubClassOf :: ex:dog'.  Single
parent from the row."
  (elot-gptel-rwp-test--with
    (should (equal '("ex:dog")
                   (elot-gptel--replace--immediate-parents "ex:puppy")))))

(ert-deftest elot-gptel-rwp-test-heading-and-row-distinct ()
  "ex:hybrid is nested under ex:animal AND declares `SubClassOf ::
ex:dog'.  Two parents; heading-nested first, then row."
  (elot-gptel-rwp-test--with
    (should (equal '("ex:animal" "ex:dog")
                   (elot-gptel--replace--immediate-parents "ex:hybrid")))))

(ert-deftest elot-gptel-rwp-test-dedup-row-equals-heading ()
  "ex:beagle is nested under ex:dog AND declares `SubClassOf ::
ex:dog'.  Single CURIE after dedup."
  (elot-gptel-rwp-test--with
    (should (equal '("ex:dog")
                   (elot-gptel--replace--immediate-parents "ex:beagle")))))

(ert-deftest elot-gptel-rwp-test-class-expression-rejected ()
  "ex:expr declares `SubClassOf :: ex:foo and ex:bar' -- a class
expression, not a bare CURIE.  The conservative bare-CURIE
matcher rejects it; ex:expr ends up with no parents at all
(since it also has no resource heading-parent)."
  (elot-gptel-rwp-test--with
    (should (null (elot-gptel--replace--immediate-parents "ex:expr")))))


;;; elot-gptel-replace-parents-test.el ends here
