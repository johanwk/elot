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

(provide 'elot-gptel-borrow-tests)
;;; elot-gptel-borrow-tests.el ends here
