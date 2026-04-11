;;; elot-owl-grammar.el --- OWL Manchester Syntax grammar for peg.el  -*- lexical-binding: t; -*-

;;; Based on owl-manchester.peggy, with manual additions for:
;;;   - BareName (unprefixed identifiers with keyword exclusion)
;;;   - Recursive "not" (not not A)
;;;   - Merged object/data property restrictions (value Literal)
;;;   - Full data range support with faceted datatype restrictions

(require 'peg)

(define-peg-ruleset elot-owl-grammar
  (class-expression () or-expression)
  (or-expression () (and and-expression (* (and ws "or" ws and-expression))))
  (and-expression () (and primary-expression (* (and ws "and" ws primary-expression))))
  ;; "not" is recursive: not not A = not(not(A))
  (primary-expression () (or (and "not" ws primary-expression) atomic-expression))
  (atomic-expression () (or restriction class-iri "Thing" "Nothing"
                             (and "{" ws individual-list ws "}")
                             (and "(" ows class-expression ows ")")))
  ;; Merged restriction: object and data property expressions both resolve to IRI,
  ;; so we use a single set of alternatives.  "value" tries Literal first
  ;; (data property), then IndividualIRI (object property) via PEG ordered choice.
  (restriction () (or (and object-property-expression ws "some" ws primary-expression)
                      (and object-property-expression ws "only" ws primary-expression)
                      (and object-property-expression ws "value" ws literal)
                      (and object-property-expression ws "value" ws individual-iri)
                      (and object-property-expression ws "min" ws integer (opt (and ws primary-expression)))
                      (and object-property-expression ws "max" ws integer (opt (and ws primary-expression)))
                      (and object-property-expression ws "exactly" ws integer (opt (and ws primary-expression)))
                      (and object-property-expression ws "Self")))
  (object-property-expression () (or (and "inverse" ws object-property-iri) object-property-iri))
  ;; SubPropertyChain: objectPropertyExpression o objectPropertyExpression { o objectPropertyExpression }
  (sub-property-chain () (and object-property-expression (+ (and ws "o" (not name-char) ws object-property-expression))))
  ;; Full data range: supports conjunction, disjunction, negation, and faceted restrictions.
  (data-range () (and data-conjunction (* (and ws "or" ws data-conjunction))))
  (data-conjunction () (and data-primary (* (and ws "and" ws data-primary))))
  (data-primary () (or (and "not" ws data-primary) data-atomic))
  (data-atomic () (or datatype-restriction
                      datatype-iri
                      (and "{" ws literal-list ws "}")
                      (and "(" ows data-range ows ")")))
  ;; Faceted datatype restriction: Datatype [ facet value, ... ]
  (datatype-restriction () (and datatype-iri ws "[" ows facet-restriction (* (and ows "," ows facet-restriction)) ows "]"))
  (facet-restriction () (and facet ws literal))
  (facet () (or "minLength" "maxLength" "length" "pattern" "langRange" "<=" ">=" "<" ">"))
  (individual-list () (and individual-iri (* (and ws "," ws individual-iri))))
  (literal-list () (and literal (* (and ws "," ws literal))))
  (class-iri () iri)
  (object-property-iri () iri)
  (datatype-iri () iri)
  (individual-iri () iri)
  (iri () (or full-iri prefixed-name bare-name))
  (full-iri () (and "<" (+ (and (not ">") (any))) ">"))
  (prefixed-name () (or (and prefix ":" local-name) (and ":" local-name)))
  ;; BareName: a local-name that is NOT an OWL keyword.
  ;; We use negative lookahead: (not (and keyword-token ...))
  ;; keyword-token matches a keyword followed by a non-name char (or end of input).
  (bare-name () (and (not keyword-boundary) local-name))
  ;; keyword-boundary: a keyword that is a complete token (not a prefix of a longer name).
  ;; We check: keyword followed by NOT [a-zA-Z0-9_-].
  ;; peg.el (not X) succeeds without consuming input if X fails.
  (keyword-boundary ()
    (or (and "and" (not name-char))
        (and "or" (not name-char))
        (and "not" (not name-char))
        (and "some" (not name-char))
        (and "only" (not name-char))
        (and "value" (not name-char))
        (and "min" (not name-char))
        (and "max" (not name-char))
        (and "exactly" (not name-char))
        (and "Self" (not name-char))
        (and "Thing" (not name-char))
        (and "Nothing" (not name-char))
        (and "inverse" (not name-char))
        (and "o" (not name-char))
        (and "length" (not name-char))
        (and "minLength" (not name-char))
        (and "maxLength" (not name-char))
        (and "pattern" (not name-char))
        (and "langRange" (not name-char))))
  (name-char () (or (range ?a ?z) (range ?A ?Z) (range ?0 ?9) "_" "-"))
  (prefix () (+ (or (range ?a ?z) (range ?A ?Z) (range ?0 ?9) "_" "-")))
  (local-name () (and (or (range ?a ?z) (range ?A ?Z) "_") (* (or (range ?a ?z) (range ?A ?Z) (range ?0 ?9) "_" "-"))))
  (literal () (and "\"" (* (and (not "\"") (any))) "\"" (opt (or (and "^^" datatype-iri) (and "@" language-tag)))))
  (language-tag () (and (+ (or (range ?a ?z) (range ?A ?Z))) (* (and "-" (+ (or (range ?a ?z) (range ?A ?Z) (range ?0 ?9)))))))
  (integer () (+ (range ?0 ?9)))
  (ws () (+ (or " " "\t" "\n" "\r")))
  (ows () (* (or " " "\t" "\n" "\r")))
)

(provide 'elot-owl-grammar)

;;; elot-owl-grammar.el ends here
