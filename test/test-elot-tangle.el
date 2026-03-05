;;; test-elot-tangle.el --- Regression tests for the ELOT tangle-to-OMN pipeline  -*- lexical-binding: t; -*-

;; These tests exercise the tangle-to-OMN functions as they exist in
;; elot-package/elot.el, which is the tangled output of elot-defs.org.
;;
;; Tests are designed to produce identical results both BEFORE and AFTER the
;; Phase 1 extraction of these functions into elot-tangle.org, making them
;; suitable as a diff/regression guard throughout the refactoring effort.
;;
;; Run in batch mode with:
;;   emacs --batch -L elot-package -l elot -l test/test-elot-tangle.el \
;;         -f ert-run-tests-batch-and-exit

;;; Setup

(require 'ert)

;; Add the elot-package directory to the load path so that (require 'elot) works
;; when running from the repository root.
(let ((pkg-dir (expand-file-name
                "elot-package"
                (or (and load-file-name
                         (file-name-directory load-file-name)
                         (expand-file-name ".." (file-name-directory load-file-name)))
                    default-directory))))
  (when (file-directory-p pkg-dir)
    (add-to-list 'load-path pkg-dir)))

(require 'elot)

;;; Tests for `elot-entity-from-header'
;;
;; This function extracts the OWL entity identifier (CURIE or full URI) from
;; an Org heading string.

(ert-deftest elot-test-tangle/entity-from-header-curie-in-parens ()
  "Prefixed name inside parentheses after a label."
  (should (equal (elot-entity-from-header "MyClass (ex:MyClass)")
                 "ex:MyClass")))

(ert-deftest elot-test-tangle/entity-from-header-uri-in-parens ()
  "Full HTTP URI inside parentheses after a label."
  (should (equal (elot-entity-from-header "My Class (<http://example.org/MyClass>)")
                 "<http://example.org/MyClass>")))

(ert-deftest elot-test-tangle/entity-from-header-bare-curie ()
  "Prefixed name at the start of the string, no label."
  (should (equal (elot-entity-from-header "ex:MyClass")
                 "ex:MyClass")))

(ert-deftest elot-test-tangle/entity-from-header-urn ()
  "URN wrapped in angle brackets at the start of the string."
  (should (equal (elot-entity-from-header "<urn:example:something>")
                 "<urn:example:something>")))

(ert-deftest elot-test-tangle/entity-from-header-urn-in-parens ()
  "URN wrapped in angle brackets inside parentheses."
  (should (equal (elot-entity-from-header "My Resource (<urn:example:something>)")
                 "<urn:example:something>")))

(ert-deftest elot-test-tangle/entity-from-header-noerror-nil ()
  "Malformed heading returns nil when NOERROR is non-nil."
  (should (null (elot-entity-from-header "No identifier here" :noerror))))

;;; Tests for `elot-omn-declare'
;;
;; This function produces the Manchester Syntax entity-type declaration line,
;; e.g. \"Class: ex:MyClass\".

(ert-deftest elot-test-tangle/omn-declare-class ()
  "Class declaration from a heading with a CURIE in parentheses."
  (should (equal (elot-omn-declare "MyClass (ex:MyClass)" "Class")
                 "Class: ex:MyClass")))

(ert-deftest elot-test-tangle/omn-declare-object-property ()
  "ObjectProperty declaration from a heading with a CURIE in parentheses."
  (should (equal (elot-omn-declare "MyProp (ex:MyProp)" "ObjectProperty")
                 "ObjectProperty: ex:MyProp")))

(ert-deftest elot-test-tangle/omn-declare-bare-curie ()
  "Declaration when the heading is just a CURIE with no label."
  (should (equal (elot-omn-declare "ex:Thing" "Class")
                 "Class: ex:Thing")))

;;; Tests for `elot-org-tags-in-string'
;;
;; This function returns the list of Org tags found in a heading string.

(ert-deftest elot-test-tangle/tags-in-string-multiple ()
  "Two tags are returned as a list."
  (should (equal (elot-org-tags-in-string "Heading :tag1:tag2:")
                 '("tag1" "tag2"))))

(ert-deftest elot-test-tangle/tags-in-string-none ()
  "No tags returns nil."
  (should (null (elot-org-tags-in-string "Heading without tags"))))

(ert-deftest elot-test-tangle/tags-in-string-single ()
  "A single tag is returned as a one-element list."
  (should (equal (elot-org-tags-in-string "Heading :oneof:")
                 '("oneof"))))

;;; Tests for `elot-resource-taxonomy-from-l'
;;
;; This is the recursive function that converts a nested list (built from Org
;; outline headings) into Manchester Syntax SubClassOf / SubPropertyOf axioms.

(ert-deftest elot-test-tangle/taxonomy-from-l-simple-hierarchy ()
  "A parent with two direct children produces two SubClassOf axioms."
  (let ((result (elot-resource-taxonomy-from-l
                 '("ex:Parent" ("ex:Child1") ("ex:Child2"))
                 "Class" "SubClassOf")))
    (should (stringp result))
    (should (string-match-p "Class: ex:Child1" result))
    (should (string-match-p "SubClassOf: ex:Parent" result))
    (should (string-match-p "Class: ex:Child2" result))))

(ert-deftest elot-test-tangle/taxonomy-from-l-single-child ()
  "A parent with a single child produces one SubClassOf axiom."
  (let ((result (elot-resource-taxonomy-from-l
                 '("ex:Animal" ("ex:Dog"))
                 "Class" "SubClassOf")))
    (should (stringp result))
    (should (string-match-p "Class: ex:Dog" result))
    (should (string-match-p "SubClassOf: ex:Animal" result))))

(ert-deftest elot-test-tangle/taxonomy-from-l-sub-property ()
  "Works with ObjectProperty and SubPropertyOf."
  (let ((result (elot-resource-taxonomy-from-l
                 '("ex:parentProp" ("ex:childProp"))
                 "ObjectProperty" "SubPropertyOf")))
    (should (stringp result))
    (should (string-match-p "ObjectProperty: ex:childProp" result))
    (should (string-match-p "SubPropertyOf: ex:parentProp" result))))

;;; Tests for `elot-annotation-entries'
;;
;; This function formats a list of property-value pairs as a Manchester Syntax
;; Annotations block.  It filters entries by prefixes registered in
;; `org-link-abbrev-alist-local'.

(ert-deftest elot-test-tangle/annotation-entries-basic ()
  "A single annotation pair is formatted as an Annotations block."
  (let ((org-link-abbrev-alist-local
         '(("rdfs" . "http://www.w3.org/2000/01/rdf-schema#"))))
    (let ((result (elot-annotation-entries '(("rdfs:label" "Test class")))))
      (should (stringp result))
      (should (string-match-p "Annotations:" result))
      (should (string-match-p "rdfs:label" result))
      (should (string-match-p "Test class" result)))))

(ert-deftest elot-test-tangle/annotation-entries-unknown-prefix ()
  "Entries with an unregistered prefix are filtered out, returning empty string."
  (let ((org-link-abbrev-alist-local nil))
    (let ((result (elot-annotation-entries '(("unknown:label" "Test")))))
      ;; When all entries are filtered, the result should be an empty string
      (should (stringp result))
      (should (string-equal result "")))))

(ert-deftest elot-test-tangle/annotation-entries-multiple ()
  "Multiple annotation pairs all appear in the output."
  (let ((org-link-abbrev-alist-local
         '(("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")
           ("skos" . "http://www.w3.org/2004/02/skos/core#"))))
    (let ((result (elot-annotation-entries
                   '(("rdfs:label" "My label")
                     ("skos:definition" "A definition")))))
      (should (stringp result))
      (should (string-match-p "rdfs:label" result))
      (should (string-match-p "skos:definition" result)))))

;;; test-elot-tangle.el ends here
