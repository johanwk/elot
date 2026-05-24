;;; elot-tangle-blank-omn-row-test.el --- Tests for OMN-axiom blank-row skip  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-tangle-blank-omn-row-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F2 -- `elot-tangle' must
;; skip description-list rows whose KEY is an OMN frame keyword
;; (`Domain', `Range', `SubClassOf', `DisjointClasses', ...) and
;; whose VALUE is blank, so blank inherited axiom rows don't tangle
;; to malformed OMN.  Tests cover the pure predicate
;; `elot--omn-row-blank-axiom-p' and end-to-end OMN emission via
;; `elot-omn-resource-frame' / `elot-omn-misc-frames'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-tangle)

;; `org-link-abbrev-alist-local' is a buffer-local Org variable; in
;; batch mode outside an Org buffer it is unbound.  Declare it
;; special so the test's `let*' binding is dynamic and reaches
;; `elot-unprefix-uri' (called from another file).
(defvar org-link-abbrev-alist-local nil)

;;; ---------------------------------------------------------------------------
;;; Pure predicate
;;; ---------------------------------------------------------------------------

(ert-deftest elot-tangle-blank-omn-row-test-predicate-positive ()
  "Blank OMN-axiom rows are detected."
  (should (elot--omn-row-blank-axiom-p '("Domain" "")))
  (should (elot--omn-row-blank-axiom-p '("Domain" "   ")))
  (should (elot--omn-row-blank-axiom-p '("Domain" nil)))
  (should (elot--omn-row-blank-axiom-p '("Range" "\t  \n")))
  (should (elot--omn-row-blank-axiom-p '("SubClassOf" "")))
  (should (elot--omn-row-blank-axiom-p '("SubPropertyOf" "")))
  (should (elot--omn-row-blank-axiom-p '("Types" "")))
  (should (elot--omn-row-blank-axiom-p '("Facts" "")))
  (should (elot--omn-row-blank-axiom-p '("Characteristics" "")))
  (should (elot--omn-row-blank-axiom-p '("DisjointClasses" "")))
  (should (elot--omn-row-blank-axiom-p '("EquivalentClasses" "")))
  (should (elot--omn-row-blank-axiom-p '("InverseOf" ""))))

(ert-deftest elot-tangle-blank-omn-row-test-predicate-negative-populated ()
  "OMN-axiom rows with content are NOT skipped."
  (should-not (elot--omn-row-blank-axiom-p '("Domain" "ex:dog")))
  (should-not (elot--omn-row-blank-axiom-p '("Range" "ex:cat")))
  (should-not (elot--omn-row-blank-axiom-p '("SubClassOf" "ex:animal")))
  (should-not (elot--omn-row-blank-axiom-p '("Characteristics" "Asymmetric"))))

(ert-deftest elot-tangle-blank-omn-row-test-predicate-negative-annotation ()
  "Blank annotation rows are NOT skipped (only OMN axiom keywords trigger)."
  (should-not (elot--omn-row-blank-axiom-p '("rdfs:comment" "")))
  (should-not (elot--omn-row-blank-axiom-p '("rdfs:comment" nil)))
  (should-not (elot--omn-row-blank-axiom-p '("skos:example" "  ")))
  (should-not (elot--omn-row-blank-axiom-p '("iof-av:naturalLanguageDefinition" ""))))

;;; ---------------------------------------------------------------------------
;;; Tangle emission via elot-omn-resource-frame
;;; ---------------------------------------------------------------------------

(defun elot-tangle-blank-omn-row-test--op-node (descs)
  "Return a hierarchy-shape node for ex:chases with DESCS as descriptions."
  (list :uri "ex:chases"
        :tags nil
        :descriptions (cons '("rdf:type" "owl:ObjectProperty") descs)
        :children nil))

(ert-deftest elot-tangle-blank-omn-row-test-skips-blank-domain ()
  "A blank `Domain ::' row does NOT appear in the emitted OMN frame."
  (let* ((node (elot-tangle-blank-omn-row-test--op-node
                '(("Domain" ""))))
         (out (elot-omn-resource-frame node nil)))
    (should (stringp out))
    (should (string-match-p "ObjectProperty: ex:chases" out))
    (should-not (string-match-p "Domain:" out))))

(ert-deftest elot-tangle-blank-omn-row-test-keeps-populated-domain ()
  "A populated `Domain :: ex:dog' row IS emitted."
  (let* ((node (elot-tangle-blank-omn-row-test--op-node
                '(("Domain" "ex:dog"))))
         (out (elot-omn-resource-frame node nil)))
    (should (string-match-p "Domain: ex:dog" out))))

(ert-deftest elot-tangle-blank-omn-row-test-skips-mixed ()
  "Mixed blank+populated rows: blank dropped, populated kept."
  (let* ((node (elot-tangle-blank-omn-row-test--op-node
                '(("Domain" "ex:dog")
                  ("Range" "")
                  ("Characteristics" "Asymmetric"))))
         (out (elot-omn-resource-frame node nil)))
    (should (string-match-p "Domain: ex:dog" out))
    (should-not (string-match-p "Range:" out))
    (should (string-match-p "Characteristics: Asymmetric" out))))

(ert-deftest elot-tangle-blank-omn-row-test-keeps-blank-annotation ()
  "A blank annotation row (e.g. rdfs:comment ::) still tangles
through (the partition path leaves annotation rows alone -- only
OMN axiom rows are filtered)."
  (let* ((org-link-abbrev-alist-local nil)
         (node (elot-tangle-blank-omn-row-test--op-node
                '(("rdfs:comment" ""))))
         (out (elot-omn-resource-frame node nil)))
    ;; The Annotations: block is emitted with the empty comment value.
    (should (string-match-p "Annotations:" out))
    (should (string-match-p "rdfs:comment" out))))

;;; ---------------------------------------------------------------------------
;;; Tangle emission via elot-omn-misc-frames
;;; ---------------------------------------------------------------------------

(ert-deftest elot-tangle-blank-omn-row-test-misc-skips-blank-disjointclasses ()
  "A blank `DisjointClasses ::' row produces no misc frame."
  (let* ((node (list :uri "ex:animal"
                     :tags nil
                     :descriptions
                     '(("rdf:type" "owl:Class")
                       ("DisjointClasses" ""))
                     :children nil))
         (frames (elot-omn-misc-frames node)))
    (should (null frames))))

(ert-deftest elot-tangle-blank-omn-row-test-misc-keeps-populated-disjointclasses ()
  "A populated `DisjointClasses :: ex:dog, ex:cat' row IS emitted."
  (let* ((node (list :uri "ex:animal"
                     :tags nil
                     :descriptions
                     '(("rdf:type" "owl:Class")
                       ("DisjointClasses" "ex:dog, ex:cat"))
                     :children nil))
         (frames (elot-omn-misc-frames node)))
    (should (= 1 (length frames)))
    (should (string-match-p "DisjointClasses: ex:dog, ex:cat" (car frames)))))

(provide 'elot-tangle-blank-omn-row-test)
;;; elot-tangle-blank-omn-row-test.el ends here
