;;; elot-test-export.el --- ERT tests for ELOT OMN export  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Keywords: languages outlines tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ERT test suite for the ELOT OMN export pipeline.
;;
;; The test suite is structured in two layers:
;;
;; 1. Golden-file regression tests.  For each of the three benchmark
;;    ontologies (pizza, DOLCE-Lite, reasoner-example) the test opens
;;    the corresponding .org file, calls `elot-export-omn-for-ontology',
;;    and checks the result against a pre-generated golden .omn file.
;;    These tests are initially skipped when the golden files contain
;;    only the placeholder comment header.
;;
;; 2. Corner-case unit tests.  Each test focuses on a single OMN
;;    construct that is known to be tricky (typed literals, multiline
;;    annotations, :oneof:/:disjoint: tags, etc.).  They are initially
;;    marked :expected-result :failed so they document intent without
;;    blocking CI.
;;
;; To run the tests:
;;
;;   emacs --batch \
;;         -l <path-to-elot-package>/elot.el \
;;         -l <path-to-elot-package>/elot-export.el \
;;         -l test/elot-test-export.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; To regenerate golden files, see test/README.org.

;;; Code:

(require 'ert)
(require 'elot)
(require 'elot-export)

;;;; Paths

(defconst elot-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Absolute path to the test/ directory.")

(defconst elot-test--benchmark-dir
  (expand-file-name "benchmark-ontologies" elot-test--dir)
  "Directory containing the benchmark .org files.")

(defconst elot-test--golden-dir
  (expand-file-name "golden" elot-test--dir)
  "Directory containing the golden .omn files.")

;;;; Helper functions

(defun elot-test--read-golden-file (filename)
  "Read FILENAME from the golden directory and return its contents as a string.
FILENAME should be a base name like \"pizza.omn\"; the golden directory
prefix is added automatically."
  (let ((path (expand-file-name filename elot-test--golden-dir)))
    (unless (file-exists-p path)
      (error "Golden file not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun elot-test--golden-is-placeholder-p (contents)
  "Return non-nil when CONTENTS is the placeholder text, not real OMN.
A file is a placeholder when its first non-empty line starts with
\"## PLACEHOLDER\"."
  (string-match-p "\\`[[:space:]]*## PLACEHOLDER" contents))

(defun elot-test--export-and-compare (org-file golden-file)
  "Export OMN from ORG-FILE and compare the result with GOLDEN-FILE.
ORG-FILE is a base name relative to the benchmark-ontologies directory.
GOLDEN-FILE is a base name relative to the golden directory.

When the golden file still contains only the placeholder comment the
test is skipped with `ert-skip'.  When the golden file has real OMN
content the exported string must match it exactly."
  (let* ((org-path    (expand-file-name org-file elot-test--benchmark-dir))
         (golden-str  (elot-test--read-golden-file golden-file)))
    (when (elot-test--golden-is-placeholder-p golden-str)
      (ert-skip
       (format "Golden file %s is a placeholder; regenerate it first \
(see test/README.org)" golden-file)))
    (unless (file-exists-p org-path)
      (ert-fail (format "Benchmark org file not found: %s" org-path)))
    (let ((result
           (with-temp-buffer
             (insert-file-contents org-path)
             (org-mode)
             ;; Navigate to the ontology heading.
             (goto-char (point-min))
             (re-search-forward ":ELOT-context-type: ontology")
             (org-back-to-heading)
             (elot-export-omn-for-ontology (point)))))
      (should (string= golden-str result)))))

;;;; Golden-file regression tests

(ert-deftest elot-test-export-pizza ()
  "Golden-file regression test for the pizza benchmark ontology."
  (elot-test--export-and-compare "pizza.org" "pizza.omn"))

(ert-deftest elot-test-export-dolce-lite ()
  "Golden-file regression test for the DOLCE-Lite benchmark ontology."
  (elot-test--export-and-compare "DOLCE-Lite.org" "DOLCE-Lite.owl.omn"))

(ert-deftest elot-test-export-reasoner-example ()
  "Golden-file regression test for the reasoner-example benchmark ontology."
  (elot-test--export-and-compare "reasoner-example.org" "reasoner-example.omn"))

;;;; Corner-case unit tests
;;
;; These tests are marked :expected-result :failed so that they
;; document the intended behaviour without blocking CI until the
;; infrastructure is in place to run them.

(ert-deftest elot-test-cc-curie-only-heading ()
  "A heading with only a CURIE and no label, e.g. \"*** xsd:integer\".
`elot-entity-from-header' should return \"xsd:integer\"."
  :expected-result :failed
  (should
   (string= "xsd:integer"
            (elot-entity-from-header "xsd:integer"))))

(ert-deftest elot-test-cc-label-plus-curie-heading ()
  "A heading with a label followed by a CURIE, e.g. \"*** particular (dol:particular)\".
`elot-entity-from-header' should return \"dol:particular\"."
  :expected-result :failed
  (should
   (string= "dol:particular"
            (elot-entity-from-header "particular (dol:particular)"))))

(ert-deftest elot-test-cc-two-curie-ontology-header ()
  "Ontology heading with ontology IRI and version IRI as two CURIEs.
e.g. \"*** pizza ontology (coo:pizza coo:pizza/2.0.0)\".
`elot-entity-from-header' should return \"coo:pizza coo:pizza/2.0.0\"."
  :expected-result :failed
  (should
   (string= "coo:pizza coo:pizza/2.0.0"
            (elot-entity-from-header "pizza ontology (coo:pizza coo:pizza/2.0.0)"))))

(ert-deftest elot-test-cc-typed-literal ()
  "Annotation value with a typed literal, e.g. \"\\\"397\\\"^^xsd:integer\".
The literal should be passed through verbatim in OMN output."
  :expected-result :failed
  ;; This is a documentation stub.  A real test would check that
  ;; elot-resource-declarations-from-header emits the typed literal correctly.
  (should nil))

(ert-deftest elot-test-cc-language-tagged-string ()
  "Annotation value with a language tag, e.g. \"\\\"text\\\"@en\".
The language tag should be preserved verbatim in OMN output."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-multiline-annotation ()
  "Annotation value that spans multiple lines (continuation lines in Org).
The newlines should be handled correctly and not produce invalid OMN."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-rdfs-see-also-uri ()
  "rdfs:seeAlso annotation with a full URI in angle brackets.
e.g. \"rdfs:seeAlso :: <https://example.org/>\".
The URI should appear in the OMN output as <https://example.org/>."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-nodeclare-tag ()
  "A heading tagged :nodeclare: is excluded from declarations.
Its misc axioms (DisjointClasses, etc.) should still appear in output."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-oneof-tag ()
  "A heading tagged :oneof: generates an EquivalentTo: ... or ... axiom.
`elot-class-oneof-from-header' should produce the correct pattern."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-disjoint-tag ()
  "A heading tagged :disjoint: generates a DisjointClasses: axiom.
`elot-class-disjoint-from-header' should produce the correct pattern."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-multiple-characteristics ()
  "A property with multiple Characteristics values, e.g. Functional, InverseFunctional.
Both values should appear in the OMN output."
  :expected-result :failed
  (should nil))

(ert-deftest elot-test-cc-empty-resource-section ()
  "An ontology section with no child headings under :resourcedefs: yes.
`elot-resource-declarations-from-header' should return \"## (none)\" or empty."
  :expected-result :failed
  (should nil))

;;;; Smoke test: function exists and returns a string

(ert-deftest elot-test-export-function-exists ()
  "Verify that `elot-export-omn-for-ontology' is defined and callable.
This test passes as long as the function is loaded."
  (should (fboundp 'elot-export-omn-for-ontology)))

(ert-deftest elot-test-tangle-function-exists ()
  "Verify that `elot-tangle-ontology' is defined and callable.
This test passes as long as the function is loaded."
  (should (fboundp 'elot-tangle-ontology)))

(provide 'elot-test-export)
;;; elot-test-export.el ends here
