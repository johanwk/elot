;;; elot-sparql-merge-prefixes-test.el --- Tests for Step 1.2  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.2:
;; Pure-helper tests for `elot--sparql-merge-prefixes' covering
;;
;;   1. empty body;
;;   2. body with no inline PREFIX lines;
;;   3. body with inline prefixes fully covered by the alist
;;      (identical URIs -> silent dedup, no warning);
;;   4. body with inline prefixes that override the alist
;;      (different URIs -> warning, last wins);
;;   5. body with inline-only prefixes not in the alist
;;      (preserved verbatim);
;;   6. inline lines are stripped from the returned body, but
;;      only when they appear at the top before any non-PREFIX
;;      content.
;;
;; These tests do not require ROBOT.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot)

(defmacro elot-spx-test--capture-warnings (&rest body)
  "Run BODY capturing any `display-warning' calls.
Return a list of (TYPE MESSAGE LEVEL) triples in call order."
  (declare (indent 0))
  `(let ((elot-spx-test--warnings nil))
     (cl-letf (((symbol-function 'display-warning)
                (lambda (type message &optional level &rest _)
                  (push (list type message level)
                        elot-spx-test--warnings))))
       ,@body)
     (nreverse elot-spx-test--warnings)))

(ert-deftest elot-sparql-merge-prefixes/empty-body ()
  "An empty body produces just the alist's prefix block."
  (let* ((alist '(("ex" . "http://example.org/")))
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist ""))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "^PREFIX ex:[ \t]+<http://example.org/>$" block))
    (should (equal rest ""))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/no-inline-prefixes ()
  "A body without any inline PREFIX lines is left untouched."
  (let* ((alist '(("ex" . "http://example.org/")))
         (body  "SELECT ?s WHERE { ?s a ex:Thing }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://example.org/>" block))
    (should (equal rest body))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/inline-covered-by-alist ()
  "Inline PREFIX with identical URI is silently de-duplicated."
  (let* ((alist '(("ex" . "http://example.org/")))
         (body  "PREFIX ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Thing }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    ;; Exactly one occurrence of the prefix in the final block.
    (should (= 1 (cl-count-if (lambda (l)
                                (string-match-p "^PREFIX ex:" l))
                              (split-string block "\n"))))
    ;; Inline line stripped from body.
    (should-not (string-match-p "PREFIX ex:" rest))
    (should (string-match-p "SELECT \\?s WHERE" rest))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/inline-overrides-alist ()
  "Conflicting inline PREFIX wins and a single warning is emitted."
  (let* ((alist '(("ex" . "http://example.org/")))
         (body  "PREFIX ex: <http://other.example/>\nSELECT ?s WHERE { ?s a ex:Thing }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://other.example/>" block))
    (should-not (string-match-p "<http://example.org/>" block))
    (should-not (string-match-p "PREFIX ex:" rest))
    (should (= 1 (length warnings)))
    (let ((w (car warnings)))
      (should (eq 'elot-sparql (nth 0 w)))
      (should (string-match-p "ex" (nth 1 w)))
      (should (string-match-p "http://example.org/" (nth 1 w)))
      (should (string-match-p "http://other.example/" (nth 1 w)))
      (should (eq :warning (nth 2 w))))))

(ert-deftest elot-sparql-merge-prefixes/inline-only-preserved ()
  "Inline PREFIX not present in the alist is preserved in the block."
  (let* ((alist '(("ex" . "http://example.org/")))
         (body  "PREFIX foo: <http://foo.example/>\nSELECT ?s WHERE { ?s a foo:Bar }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://example.org/>" block))
    (should (string-match-p "PREFIX foo:[ \t]+<http://foo.example/>" block))
    ;; Order: injected (ex) before inline-only (foo).
    (should (< (string-match "PREFIX ex:" block)
               (string-match "PREFIX foo:" block)))
    (should-not (string-match-p "PREFIX foo:" rest))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/case-insensitive-keyword ()
  "The PREFIX keyword is recognised case-insensitively."
  (let* ((alist nil)
         (body  "prefix ex: <http://example.org/>\nSELECT ?s WHERE { ?s a ex:Thing }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://example.org/>" block))
    (should-not (string-match-p "(?i)prefix ex:" rest))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/stops-at-first-non-prefix ()
  "A PREFIX line buried below a SELECT keyword is not stripped."
  (let* ((alist nil)
         ;; The second PREFIX line follows non-PREFIX content and
         ;; therefore must be left in the body verbatim.
         (body  "PREFIX a: <http://a.example/>\nSELECT ?s\nPREFIX b: <http://b.example/>")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    (should (string-match-p "PREFIX a:[ \t]+<http://a.example/>" block))
    (should-not (string-match-p "PREFIX b:" block))
    (should (string-match-p "PREFIX b: <http://b.example/>" rest))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/identical-redeclaration-no-warning ()
  "Two identical inline PREFIX lines are silently de-duplicated."
  (let* ((alist nil)
         (body  "PREFIX ex: <http://example.org/>\nPREFIX ex: <http://example.org/>\nSELECT ?s")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result)))
    (should (= 1 (cl-count-if (lambda (l)
                                (string-match-p "^PREFIX ex:" l))
                              (split-string block "\n"))))
    (should (null warnings))))

(ert-deftest elot-sparql-merge-prefixes/alist-header-row-skipped ()
  "An (\"prefix\" . \"uri\") header row from an Org table is ignored."
  (let* ((alist '(("prefix" . "uri")
                  ("ex" . "http://example.org/")))
         (body  "")
         (result (elot--sparql-merge-prefixes alist body))
         (block (car result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://example.org/>" block))
    (should-not (string-match-p "PREFIX prefix:" block))))

(ert-deftest elot-sparql-merge-prefixes/alist-list-cdr-shape ()
  "Org-table alist entries whose cdr is a one-element list are accepted."
  (let* ((alist '(("ex" "http://example.org/")))
         (result (elot--sparql-merge-prefixes alist ""))
         (block (car result)))
    (should (string-match-p "PREFIX ex:[ \t]+<http://example.org/>" block))))

(ert-deftest elot-sparql-compute-merged-prefixes/returns-pairs ()
  "`elot--sparql-compute-merged-prefixes' returns an alist of pairs
with inline overrides applied, alist-first order preserved."
  (let* ((alist '(("ex"   . "http://example.org/")
                  ("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")))
         (body  "PREFIX rdfs: <http://override.example/>\nPREFIX new: <http://new.example/>\nSELECT ?s")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-compute-merged-prefixes alist body))))
         (pairs (car result))
         (rest  (cdr result)))
    ;; All three labels present, each exactly once.
    (should (equal '("ex" "rdfs" "new") (mapcar #'car pairs)))
    ;; Inline override wins.
    (should (equal "http://override.example/"
                   (cdr (assoc "rdfs" pairs))))
    ;; Inline-only label preserved.
    (should (equal "http://new.example/"
                   (cdr (assoc "new" pairs))))
    ;; Body returned with PREFIX lines stripped.
    (should-not (string-match-p "PREFIX " rest))
    ;; The conflicting rdfs: redeclaration emits exactly one warning.
    (should (= 1 (length warnings)))))

(ert-deftest elot-sparql-compute-merged-prefixes/empty-alist-and-body ()
  "An empty alist and empty body yield empty pairs and empty rest."
  (let* ((result (elot--sparql-compute-merged-prefixes nil ""))
         (pairs (car result))
         (rest  (cdr result)))
    (should (null pairs))
    (should (equal "" rest))))

(ert-deftest elot-sparql-merge-prefixes/inline-with-trailing-comment ()
  "Inline PREFIX with a trailing SPARQL line-comment is recognised.
Regression for the Block 3 round-trip failure: when the regexp
did not tolerate a trailing `# ...' comment, the inline line was
left in the body verbatim and no conflict warning was emitted."
  (let* ((alist '(("rdfs" . "http://www.w3.org/2000/01/rdf-schema#")))
         (body  "PREFIX rdfs: <http://example.com/>    # existing prefix, different uri\nSELECT ?s WHERE { ?s a rdfs:Class }")
         (warnings (elot-spx-test--capture-warnings
                     (setq result (elot--sparql-merge-prefixes alist body))))
         (block (car result))
         (rest  (cdr result)))
    ;; Inline override must win.
    (should (string-match-p "PREFIX rdfs:[ \t]+<http://example.com/>" block))
    (should-not (string-match-p "rdf-schema#" block))
    ;; The inline line (and its trailing comment) is stripped.
    (should-not (string-match-p "PREFIX rdfs:" rest))
    (should-not (string-match-p "existing prefix" rest))
    ;; And the conflict surfaces as exactly one warning.
    (should (= 1 (length warnings)))
    (should (eq 'elot-sparql (nth 0 (car warnings))))))

(provide 'elot-sparql-merge-prefixes-test)
;;; elot-sparql-merge-prefixes-test.el ends here
