;;; elot-gptel-borrow-tests.el --- Borrow / definition_from / rollback-echo tests -*- lexical-binding: t; -*-

;; Usage:  cd test && make elot-gptel-borrow-tests.el
;;
;; Covers the Step-2 borrow work:
;;   - candidate ranking helpers (exact-match preference, IRI-keyed
;;     de-duplication, source preference);
;;   - `definition_from' plumbing (arg normalisation, ordered
;;     first-hit-wins probe, declared-AP enumeration);
;;   - the `NEXT:' trailer wording (precondition-gated, no
;;     `skos:definition');
;;   - the `== ROWS WRITTEN (rolled back) ==' diagnostic echo;
;;   - tool-spec / dispatcher arity for the new `definition_from' arg.
;;
;; Pure Elisp: no ROBOT, no reasoner, no on-disk ontology mutation.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-gptel)

;;;; Helpers ------------------------------------------------------------

(defun elot-gptel-borrow-tests--row (id label &optional source)
  "Build a minimal seven-tuple search row."
  (list id label "owl:Class" "ont:x" (or source "src") nil
        "label-substring"))

;;;; Label / local-name normalisation -----------------------------------

(ert-deftest test-elot-gptel-borrow-normalise-label ()
  (should (equal "denoter"
                 (elot-gptel--borrow-normalise-label
                  "\"denoter\"@en-us")))
  (should (equal "denoter"
                 (elot-gptel--borrow-normalise-label "\"denoter\"")))
  (should (equal "denoter"
                 (elot-gptel--borrow-normalise-label
                  "\"denoter\"^^xsd:string")))
  (should (equal "denoter"
                 (elot-gptel--borrow-normalise-label "  denoter  ")))
  ;; A bare string with an unmatched quote is left alone (trimmed).
  (should (equal "\"denoter"
                 (elot-gptel--borrow-normalise-label "\"denoter")))
  (should (equal "" (elot-gptel--borrow-normalise-label "")))
  (should-not (elot-gptel--borrow-normalise-label nil))
  (should-not (elot-gptel--borrow-normalise-label 42)))

(ert-deftest test-elot-gptel-borrow-local-name ()
  (should (equal "Denoter"
                 (elot-gptel--borrow-local-name "iof-constr:Denoter")))
  (should (equal "Denoter"
                 (elot-gptel--borrow-local-name
                  "https://example.org/core/Denoter")))
  (should (equal "Denoter"
                 (elot-gptel--borrow-local-name
                  "https://example.org/core#Denoter")))
  ;; No separator: the id is its own local name.
  (should (equal "Denoter" (elot-gptel--borrow-local-name "Denoter")))
  (should-not (elot-gptel--borrow-local-name nil)))

;;;; Exact-match predicate ----------------------------------------------

(ert-deftest test-elot-gptel-borrow-exact-p ()
  (let ((row (elot-gptel-borrow-tests--row
              "iof-constr:Denoter" "\"denoter\"@en-us")))
    ;; Normalised label, case-insensitively.
    (should (elot-gptel--borrow-exact-p row "denoter"))
    (should (elot-gptel--borrow-exact-p row "DENOTER"))
    ;; Local name of the id.
    (should (elot-gptel--borrow-exact-p row "Denoter"))
    ;; Full id.
    (should (elot-gptel--borrow-exact-p row "iof-constr:Denoter"))
    ;; A mere substring is NOT exact -- this is the whole point.
    (should-not (elot-gptel--borrow-exact-p row "denot"))
    (should-not (elot-gptel--borrow-exact-p
                 (elot-gptel-borrow-tests--row
                  "iof-constr:OrganizationIdentifier"
                  "\"organization identifier\"@en-us")
                 "identifier"))))

;;;; Row preference + IRI-keyed collapse --------------------------------

(ert-deftest test-elot-gptel-borrow-row-preference ()
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda () '("active-src"))))
    (let ((explicit (elot-gptel-borrow-tests--row "x:a" "a" "chosen"))
          (active   (elot-gptel-borrow-tests--row "x:a" "a" "active-src"))
          (other    (elot-gptel-borrow-tests--row "x:a" "a" "elsewhere")))
      (should (= 0 (elot-gptel--borrow-row-preference explicit "chosen")))
      (should (= 1 (elot-gptel--borrow-row-preference active nil)))
      (should (= 2 (elot-gptel--borrow-row-preference other nil)))
      ;; Without an explicit SOURCE the "chosen" row is just another row.
      (should (= 2 (elot-gptel--borrow-row-preference explicit nil))))))

(ert-deftest test-elot-gptel-borrow-collapse-same-iri ()
  "Three attestations of one entity collapse to a single group."
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda () '("active-src")))
            ((symbol-function 'elot-db-expand-curie)
             (lambda (curie)
               (when (string-prefix-p "iof-constr:" curie)
                 (concat "https://example.org/core/"
                         (substring curie (length "iof-constr:")))))))
    (let* ((rows (list (elot-gptel-borrow-tests--row
                        "iof-constr:Denoter" "Denoter" "pattern-file")
                       (elot-gptel-borrow-tests--row
                        "iof-constr:Denoter" "\"denoter\"@en-us" "active-src")
                       (elot-gptel-borrow-tests--row
                        "iof-constr:Denoter" "\"denoter\"@en-us" "elsewhere")))
           (groups (elot-gptel--borrow-collapse rows nil)))
      (should (= 1 (length groups)))
      ;; All three attestations retained in the group tail.
      (should (= 3 (length (cdr (car groups)))))
      ;; The active source wins as the representative row.
      (should (equal "active-src" (nth 4 (car (car groups))))))))

(ert-deftest test-elot-gptel-borrow-collapse-source-wins ()
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda () '("active-src")))
            ((symbol-function 'elot-db-expand-curie)
             (lambda (_curie) "https://example.org/core/Denoter")))
    (let* ((rows (list (elot-gptel-borrow-tests--row
                        "iof-constr:Denoter" "Denoter" "active-src")
                       (elot-gptel-borrow-tests--row
                        "iof-constr:Denoter" "Denoter" "chosen")))
           (groups (elot-gptel--borrow-collapse rows "chosen")))
      (should (= 1 (length groups)))
      (should (equal "chosen" (nth 4 (car (car groups))))))))

(ert-deftest test-elot-gptel-borrow-collapse-keeps-prefix-twins-apart ()
  "`iof-constr:' and `iof-construct:' are DIFFERENT namespaces."
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda () nil))
            ((symbol-function 'elot-db-expand-curie)
             (lambda (curie)
               (cond
                ((string-prefix-p "iof-constr:" curie)
                 "https://example.org/core/designates")
                ((string-prefix-p "iof-construct:" curie)
                 "https://example.org/biopharma/designates")))))
    (let* ((rows (list (elot-gptel-borrow-tests--row
                        "iof-constr:designates" "designates" "a")
                       (elot-gptel-borrow-tests--row
                        "iof-construct:designates" "designates" "b")))
           (groups (elot-gptel--borrow-collapse rows nil)))
      ;; Same local name, same label -- but genuinely distinct entities.
      (should (= 2 (length groups))))))

(ert-deftest test-elot-gptel-borrow-collapse-preserves-order ()
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda () nil))
            ((symbol-function 'elot-db-expand-curie)
             (lambda (curie) curie)))
    (let* ((rows (list (elot-gptel-borrow-tests--row "x:a" "a" "s")
                       (elot-gptel-borrow-tests--row "x:b" "b" "s")
                       (elot-gptel-borrow-tests--row "x:a" "a" "t")))
           (groups (elot-gptel--borrow-collapse rows nil)))
      (should (= 2 (length groups)))
      (should (equal '("x:a" "x:b")
                     (mapcar (lambda (g) (nth 0 (car g))) groups))))))

;;;; definition_from argument normalisation ------------------------------

(ert-deftest test-elot-gptel-declare-normalise-props ()
  (should-not (elot-gptel--declare-normalise-props nil))
  (should (equal '("skos:definition")
                 (elot-gptel--declare-normalise-props ["skos:definition"])))
  (should (equal '("skos:definition" "rdfs:comment")
                 (elot-gptel--declare-normalise-props
                  ["skos:definition" "rdfs:comment"])))
  (should (equal '("skos:definition" "rdfs:comment")
                 (elot-gptel--declare-normalise-props
                  '("skos:definition" "rdfs:comment"))))
  ;; Comma-separated string, with slack whitespace.
  (should (equal '("skos:definition" "rdfs:comment")
                 (elot-gptel--declare-normalise-props
                  "skos:definition, rdfs:comment")))
  ;; Order is preserved -- first hit wins downstream.
  (should (equal '("b" "a")
                 (elot-gptel--declare-normalise-props ["b" "a"])))
  ;; Empty entries dropped.
  (should (equal '("a") (elot-gptel--declare-normalise-props '("a" "" "  ")))))

;;;; Ordered, first-hit-wins definition probe ---------------------------

(defmacro elot-gptel-borrow-tests--with-annotation-rows (rows &rest body)
  "Run BODY with the DB annotation-row probe stubbed to ROWS."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'elot-gptel--db-ensure-open) (lambda () t))
             ((symbol-function 'elot-gptel--citation-preferring-active)
              (lambda (&rest _) '(:source "iof-core")))
             ((symbol-function 'elot-db-entity-annotation-rows)
              (lambda (&rest _) ,rows)))
     ,@body))

(ert-deftest test-elot-gptel-declare-definition-probe-first-hit-wins ()
  (elot-gptel-borrow-tests--with-annotation-rows
      '(("iof-av:explanatoryNote" . "note")
        ("iof-av:naturalLanguageDefinition" . "the definition")
        ("skos:example" . "an example"))
    ;; skos:definition is absent -> the second prop in the ordered list wins.
    (let ((p (elot-gptel--declare-definition-probe
              "iof-constr:Denoter"
              '("skos:definition" "iof-av:naturalLanguageDefinition"))))
      (should (equal "iof-av:naturalLanguageDefinition" (plist-get p :prop)))
      (should (equal "the definition" (plist-get p :value)))
      ;; The written prop is excluded from :available.
      (should (equal '("iof-av:explanatoryNote" "skos:example")
                     (plist-get p :available))))))

(ert-deftest test-elot-gptel-declare-definition-probe-order-matters ()
  (elot-gptel-borrow-tests--with-annotation-rows
      '(("skos:definition" . "skos value")
        ("iof-av:naturalLanguageDefinition" . "iof value"))
    (should (equal "skos:definition"
                   (plist-get (elot-gptel--declare-definition-probe
                               "x:a" '("skos:definition"
                                       "iof-av:naturalLanguageDefinition"))
                              :prop)))
    (should (equal "iof-av:naturalLanguageDefinition"
                   (plist-get (elot-gptel--declare-definition-probe
                               "x:a" '("iof-av:naturalLanguageDefinition"
                                       "skos:definition"))
                              :prop)))))

(ert-deftest test-elot-gptel-declare-definition-probe-no-hit ()
  "Rows exist but none match: `:prop' is nil, `:available' is not."
  (elot-gptel-borrow-tests--with-annotation-rows
      '(("iof-av:explanatoryNote" . "note"))
    (let ((p (elot-gptel--declare-definition-probe
              "x:a" '("skos:definition"))))
      (should-not (plist-get p :prop))
      (should-not (plist-get p :value))
      (should (equal '("iof-av:explanatoryNote") (plist-get p :available))))))

(ert-deftest test-elot-gptel-declare-definition-probe-unknown-term ()
  (elot-gptel-borrow-tests--with-annotation-rows nil
    (let ((p (elot-gptel--declare-definition-probe
              "x:nope" '("skos:definition"))))
      (should-not (plist-get p :prop))
      (should-not (plist-get p :available)))))

;;;; Rollback-echo diagnostic -------------------------------------------

(ert-deftest test-elot-gptel-mutation-rows-block-empty ()
  (let ((elot-gptel--mutation-rows nil))
    (should-not (elot-gptel--mutation-rows-block))))

(ert-deftest test-elot-gptel-mutation-rows-block-renders-in-order ()
  (let ((elot-gptel--mutation-rows nil))
    (elot-gptel-note-mutation-rows "first row")
    (elot-gptel-note-mutation-rows "second row")
    (let ((block (elot-gptel--mutation-rows-block)))
      (should (string-prefix-p "== ROWS WRITTEN (rolled back) ==" block))
      ;; Recording order, not push order.
      (should (< (string-match-p "first row" block)
                 (string-match-p "second row" block)))
      (should (string-match-p "^  first row$" block)))))

(ert-deftest test-elot-gptel-note-mutation-rows-flattens-and-filters ()
  (let ((elot-gptel--mutation-rows nil))
    (elot-gptel-note-mutation-rows (list "a" nil "") "b" nil)
    (should (equal '("a" "b") (reverse elot-gptel--mutation-rows)))))

;;;; NEXT: trailer wording ----------------------------------------------

(ert-deftest test-elot-gptel-borrow-next-block-wording ()
  (cl-letf (((symbol-function 'elot-gptel--borrow-parent-preferring-active)
             (lambda (&rest _) "iof-constr:InformationContentEntity"))
            ((symbol-function 'elot-db-entity-annotation-rows)
             (lambda (&rest _)
               '(("iof-av:naturalLanguageDefinition" . "d")
                 ("skos:example" . "e")))))
    (let ((out (elot-gptel--borrow-next-block
                '(:id "iof-constr:Denoter" :label "denoter"
                      :ontology-iri "<https://example.org/core/>"
                      :source "iof-core"))))
      (should (stringp out))
      (should (string-match-p "^NEXT: elot_declare_resource" out))
      (should (string-match-p "curie=iof-constr:Denoter" out))
      (should (string-match-p
               "anchor=iof-constr:InformationContentEntity as=child" out))
      ;; borrow=true advertises ONLY the provenance row.
      (should (string-match-p
               "borrow=true writes: rdfs:isDefinedBy" out))
      (should-not (string-match-p "borrow=true writes:.*skos:definition" out))
      ;; Definition APs are offered as a precondition-gated option.
      (should (string-match-p "definition APs available" out))
      (should (string-match-p "ONLY when the source ontology is NOT imported"
                              out))
      (should (string-match-p "iof-av:naturalLanguageDefinition" out))
      ;; skos:example is not definition-bearing -- filtered out.
      (should-not (string-match-p "skos:example" out)))))

(ert-deftest test-elot-gptel-borrow-next-block-no-definitions ()
  (cl-letf (((symbol-function 'elot-gptel--borrow-parent-preferring-active)
             (lambda (&rest _) nil))
            ((symbol-function 'elot-db-entity-annotation-rows)
             (lambda (&rest _) nil)))
    (let ((out (elot-gptel--borrow-next-block
                '(:id "x:a" :label "a" :source "s"))))
      (should (string-match-p "anchor=<ANCHOR>" out))
      (should-not (string-match-p "definition APs available" out)))))

;;;; Declared annotation properties -------------------------------------

(ert-deftest test-elot-gptel-declared-annotation-properties ()
  (cl-letf (((symbol-function 'elot-gptel--axiom-slurp-for-file)
             (lambda (_file) 'stub-slurp))
            ((symbol-function 'elot-gptel--axiom-collect-by-kind)
             (lambda (slurp kind)
               (should (eq slurp 'stub-slurp))
               (should (equal "owl:AnnotationProperty" kind))
               '(("skos:definition" . "definition")
                 ("iof-av:naturalLanguageDefinition" . "nld")))))
    (should (equal '("skos:definition" "iof-av:naturalLanguageDefinition")
                   (elot-gptel--declared-annotation-properties
                    "some/file.org")))))

;;;; Tool spec + dispatcher arity ---------------------------------------

(ert-deftest test-elot-gptel-declare-resource-spec-has-definition-from ()
  (let* ((spec (assoc "elot_declare_resource" elot-gptel--tool-specs))
         (args (plist-get (cdr spec) :args)))
    (should spec)
    (let ((names (mapcar (lambda (a) (plist-get a :name)) args)))
      (should (member "borrow" names))
      (should (member "definition_from" names))
      ;; definition_from is positionally last, matching the function
      ;; signature the dispatcher applies.
      (should (equal "definition_from" (car (last names)))))
    (let ((df (cl-find "definition_from" args
                       :key (lambda (a) (plist-get a :name))
                       :test #'equal)))
      (should (eq 'array (plist-get df :type)))
      (should (plist-get df :optional))
      ;; The description states the precondition, not a menu.
      (should (string-match-p "PRECONDITION" (plist-get df :description))))))

(ert-deftest test-elot-gptel-declare-resource-dispatcher-arity ()
  "The dispatcher lambda must accept all 8 spec arguments.
Regression: it was written with 7 parameters after `definition_from'
was added to the spec, so every borrow chain died at its terminal
step with `wrong-number-of-arguments'."
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-declare-resource)))
    (should (functionp thunk))
    (should-not
     (condition-case _
         (progn (funcall thunk "no-such-file.org" "anchor" "label"
                         "ex:thing" nil "child" nil ["skos:definition"])
                nil)
       (wrong-number-of-arguments t)
       (error nil)))))

;;;; prefer-source plumbing (work item 4) --------------------------------

(defun elot-gptel-borrow-tests--citation-table (table)
  "Return a stub `elot-db-entity-citation' driven by TABLE.
TABLE is an alist of (SOURCE . ONTOLOGY-IRI); a nil SOURCE key
supplies the unrestricted-lookup answer."
  (lambda (token &optional source)
    (let ((hit (assoc source table)))
      (when hit
        (list :id token :label "denoter"
              :ontology-iri (cdr hit)
              :ontology-iri-from 'explicit
              :source (or source "any"))))))

(ert-deftest test-elot-gptel-citation-prefer-source-wins ()
  "PREFER-SOURCE beats an active source that also attests the id."
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda (&optional _exclude) '("active-src")))
            ((symbol-function 'elot-db-entity-citation)
             (elot-gptel-borrow-tests--citation-table
              '(("chosen"     . "<https://example.org/core/Core/>")
                ("active-src" . "<https://example.org/pattern/0.0>")
                (nil          . "<https://example.org/anything>")))))
    (let ((cit (elot-gptel--citation-preferring-active
                "iof-constr:Denoter" nil "chosen")))
      (should (equal "<https://example.org/core/Core/>"
                     (plist-get cit :ontology-iri)))
      (should (equal "chosen" (plist-get cit :source))))))

(ert-deftest test-elot-gptel-citation-prefer-source-miss-falls-back ()
  "A PREFER-SOURCE that knows nothing of the id must not swallow the result."
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda (&optional _exclude) '("active-src")))
            ((symbol-function 'elot-db-entity-citation)
             (elot-gptel-borrow-tests--citation-table
              '(("active-src" . "<https://example.org/active/>")
                (nil          . "<https://example.org/anything>")))))
    (let ((cit (elot-gptel--citation-preferring-active
                "x:a" nil "knows-nothing")))
      (should (equal "<https://example.org/active/>"
                     (plist-get cit :ontology-iri)))
      (should (equal "active-src" (plist-get cit :source))))))

(ert-deftest test-elot-gptel-citation-exclusion-beats-prefer-source ()
  "When PREFER-SOURCE names EXCLUDE-FILE, exclusion wins."
  (cl-letf (((symbol-function 'elot-gptel--active-source-names)
             (lambda (&optional _exclude) '("active-src")))
            ((symbol-function 'elot-db-entity-citation)
             (elot-gptel-borrow-tests--citation-table
              '(("target.org"  . "<https://example.org/target/>")
                ("active-src"  . "<https://example.org/active/>")
                (nil           . "<https://example.org/anything>")))))
    (let ((cit (elot-gptel--citation-preferring-active
                "x:a" "target.org" "target.org")))
      ;; The borrowing file is never its own citation target.
      (should-not (equal "target.org" (plist-get cit :source)))
      (should (equal "<https://example.org/active/>"
                     (plist-get cit :ontology-iri))))))

(ert-deftest test-elot-gptel-borrow-parent-prefer-source-wins ()
  "The parent lookup honours PREFER-SOURCE before the active list."
  (let ((elot-active-label-sources '("active-src")))
    (cl-letf (((symbol-function 'elot-gptel--borrow-source-parent)
               (lambda (_id &optional source)
                 (cond ((equal source "chosen")     "iof-constr:Parent")
                       ((equal source "active-src") "other:Parent")
                       ((null source)               "any:Parent")))))
      (should (equal "iof-constr:Parent"
                     (elot-gptel--borrow-parent-preferring-active
                      "iof-constr:Denoter" "cit-src" "chosen")))
      ;; Without PREFER-SOURCE the active list still wins (no regression).
      (should (equal "other:Parent"
                     (elot-gptel--borrow-parent-preferring-active
                      "iof-constr:Denoter" "cit-src"))))))

(ert-deftest test-elot-gptel-borrow-collision-note-fires ()
  "Two prefixes expanding to the same namespace produce a NOTE."
  (cl-letf (((symbol-function 'elot-db--expansion-in-source-only)
             (lambda (prefix _src _ds)
               (cond ((equal prefix "iof-constr")  "https://ex.org/construct/")
                     ((equal prefix "iof-construct") "https://ex.org/construct/")
                     ((equal prefix "other")       "https://ex.org/other/")))))
    (let* ((rows '(("iof-constr:ICE" "ice" "owl:Class" nil "a.org" "" "x")
                   ("iof-construct:ICE" "ice" "owl:Class" nil "b.org" "" "x")))
           (note (elot-gptel--borrow-collision-note rows)))
      (should (stringp note))
      (should (string-match-p "same namespace" note))
      (should (string-match-p "iof-constr:" note))
      (should (string-match-p "iof-construct:" note)))
    ;; Different namespaces: no note.
    (should-not
     (elot-gptel--borrow-collision-note
      '(("iof-constr:ICE" "ice" "owl:Class" nil "a.org" "" "x")
        ("other:ICE" "ice" "owl:Class" nil "b.org" "" "x"))))
    ;; Single row: no note.
    (should-not
     (elot-gptel--borrow-collision-note
      '(("iof-constr:ICE" "ice" "owl:Class" nil "a.org" "" "x"))))))

(ert-deftest test-elot-gptel-borrow-row-namespace-unresolvable ()
  "An unresolvable prefix yields nil rather than a guess."
  (cl-letf (((symbol-function 'elot-db--expansion-in-source-only)
             (lambda (&rest _) nil)))
    (should-not
     (elot-gptel--borrow-row-namespace
      '("zz:Thing" "thing" "owl:Class" nil "a.org" "" "x"))))
  ;; A full IRI id is not a CURIE; no lookup attempted.
  (should-not
   (elot-gptel--borrow-row-namespace
    '("http://ex.org/Thing" "thing" "owl:Class" nil "a.org" "" "x"))))

;;; Guardrail 1: blank filter arguments are treated as "unset".

(ert-deftest test-elot-gptel-db-blank-to-nil ()
  "Empty / whitespace-only / non-string filter values normalise to nil."
  (should-not (elot-gptel--db-blank-to-nil ""))
  (should-not (elot-gptel--db-blank-to-nil "   "))
  (should-not (elot-gptel--db-blank-to-nil "\t\n"))
  (should-not (elot-gptel--db-blank-to-nil nil))
  (should-not (elot-gptel--db-blank-to-nil 42))
  (should (equal (elot-gptel--db-blank-to-nil "en") "en"))
  (should (equal (elot-gptel--db-blank-to-nil " en ") " en ")))

;;; Guardrail 2: a zero-row result names the filter that emptied it.

(ert-deftest test-elot-gptel-db-search-zero-hint-blames-kind ()
  "A kind filter that removed rows is reported, with a re-run hint."
  (cl-letf (((symbol-function 'elot-db-search-entities)
             (lambda (_q _lim kind _src _lang _exact)
               ;; Rows exist, but none of them has a recorded kind.
               (if kind nil '(("prov:value" "value" nil nil "p.ttl" "" "exact"))))))
    (let ((hint (elot-gptel--db-search-zero-hint
                 "value" 50 "data-property" nil nil nil)))
      (should (string-match-p "HINT:" hint))
      (should (string-match-p "kind=data-property" hint))
      (should (string-match-p "dropped 1 row" hint)))))

(ert-deftest test-elot-gptel-db-search-zero-hint-blames-exact-only ()
  "exact_only is reported when relaxing it would have produced rows."
  (cl-letf (((symbol-function 'elot-db-search-entities)
             (lambda (_q _lim _kind _src _lang exact)
               (if exact nil '(("ex:x" "x" "owl:Class" nil "a.org" "" "local-name"))))))
    (let ((hint (elot-gptel--db-search-zero-hint "ex:x" 50 nil nil nil t)))
      (should (string-match-p "exact_only" hint)))))

(ert-deftest test-elot-gptel-db-search-zero-hint-silent-when-unfiltered ()
  "A genuine zero-candidate result carries no HINT block."
  (cl-letf (((symbol-function 'elot-db-search-entities)
             (lambda (&rest _) nil)))
    (should (equal (elot-gptel--db-search-zero-hint
                    "nothing" 50 "class" "s.org" "en" t)
                   ""))
    ;; No filters at all: nothing to blame.
    (should (equal (elot-gptel--db-search-zero-hint
                    "nothing" 50 nil nil nil nil)
                   ""))))

(provide 'elot-gptel-borrow-tests)
;;; elot-gptel-borrow-tests.el ends here
