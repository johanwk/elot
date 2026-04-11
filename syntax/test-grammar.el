;;; test-grammar.el --- Test the OWL Manchester Syntax PEG grammar  -*- lexical-binding: t; -*-

;; Usage:  cd syntax && emacs --batch -l test-grammar.el

(require 'cl-lib)

;; 1. Load the grammar (hand-maintained; see owl-manchester.peggy for the
;;    original Peggy source and elot-owl-grammar.el for the peg.el version)
(load-file (expand-file-name "elot-owl-grammar.el"))

;; 3. Utility functions: parse INPUT starting from a specific grammar rule.
;;    Return t on full match, nil otherwise.

(defun elot-parse-class-expression (input)
  "Try to parse INPUT as an OWL Manchester Syntax class expression.
Return t if the entire string is consumed, nil otherwise."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((result (peg-run (peg class-expression))))
            (and result (eobp))))
      (error
       (message "  PARSE ERROR: %S" err)
       nil))))

(defun elot-parse-property-expression (input)
  "Try to parse INPUT as an OWL Manchester Syntax object property expression.
Return t if the entire string is consumed, nil otherwise."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((result (peg-run (peg object-property-expression))))
            (and result (eobp))))
      (error
       (message "  PARSE ERROR: %S" err)
       nil))))

(defun elot-parse-sub-property-chain (input)
  "Try to parse INPUT as an OWL Manchester Syntax sub-property chain.
A chain is two or more objectPropertyExpressions joined by \='o\='.
Return t if the entire string is consumed, nil otherwise."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((result (peg-run (peg sub-property-chain))))
            (and result (eobp))))
      (error
       (message "  PARSE ERROR: %S" err)
       nil))))

(defun elot-parse-data-range (input)
  "Try to parse INPUT as an OWL Manchester Syntax data range.
Supports datatype IRIs, faceted restrictions (e.g. xsd:integer[>= 0]),
data conjunctions/disjunctions, and negation.
Return t if the entire string is consumed, nil otherwise."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((result (peg-run (peg data-range))))
            (and result (eobp))))
      (error
       (message "  PARSE ERROR: %S" err)
       nil))))

;; 3b. Test helper (used by the test runner below).
(defun elot-test-parse (input)
  "Try to parse INPUT as an OWL Manchester Syntax class expression.
Return t if the entire string is consumed, nil otherwise.
Delegates to `elot-parse-class-expression'."
  (elot-parse-class-expression input))

;; 4. Test cases — three typical OMN axioms
(defvar elot-test-cases
  '(;; --- SubClassOf: conjunction with existential restriction ---
    ;; "A Pizza that has some MozzarellaTopping and has some TomatoTopping"
    ("SubClassOf: conjunction + existential"
     "pizza:MargheritaPizza and pizza:hasTopping some pizza:MozzarellaTopping and pizza:hasTopping some pizza:TomatoTopping")
    ("SubClassOf: conjunction + existential"
     "pizza:MargheritaPizza and ( pizza:hasTopping some pizza:MozzarellaTopping ) and ( pizza:hasTopping some pizza:TomatoTopping )")
    ("SubClassOf: conjunction + existential"
     "pizza:MargheritaPizza and (pizza:hasTopping some pizza:MozzarellaTopping) and (pizza:hasTopping some pizza:TomatoTopping)")

    ;; --- SubClassOf: universal restriction (only) ---
    ;; "hasTopping only (MozzarellaTopping or TomatoTopping)"
    ("SubClassOf: universal restriction with union"
     "pizza:hasTopping only (pizza:MozzarellaTopping or pizza:TomatoTopping)")

    ;; --- SubClassOf: cardinality restriction ---
    ;; "hasAuthor min 1 Person"
    ("SubClassOf: min cardinality"
     "ex:hasAuthor min 1 ex:Person")

    ;; --- Simple class reference ---
    ("Simple prefixed class IRI"
     "owl:Thing")

    ;; --- Negation ---
    ("Negated class expression"
     "not pizza:MeatTopping")

    ;; --- Inverse property ---
    ("Inverse property restriction"
     "inverse ex:partOf some ex:Engine")

    ;; --- Enumeration (individual set) ---
    ("OneOf individuals"
     "{ ex:Monday , ex:Tuesday , ex:Wednesday }")

    ;; --- Full IRI ---
    ("Full IRI expression"
     "<http://example.org/ns#Foo> and <http://example.org/ns#Bar>")

    ;; ================================================================
    ;; Hard cases extracted from examples/*.omn
    ;; ================================================================

    ;; --- BFO: universal restriction with full IRIs ---
    ("BFO: full-IRI only full-IRI"
     "<http://purl.obolibrary.org/obo/BFO_0000176> only <http://purl.obolibrary.org/obo/BFO_0000002>")

    ;; --- BFO: universal restriction whose filler is a union (2-way) ---
    ("BFO: only (A or B) — 2-way union filler"
     "<http://purl.obolibrary.org/obo/BFO_0000178> only (<http://purl.obolibrary.org/obo/BFO_0000142> or <http://purl.obolibrary.org/obo/BFO_0000147>)")

    ;; --- BFO: universal restriction whose filler is a 3-way union ---
    ("BFO: only (A or B or C) — 3-way union filler"
     "<http://purl.obolibrary.org/obo/BFO_0000178> only (<http://purl.obolibrary.org/obo/BFO_0000029> or <http://purl.obolibrary.org/obo/BFO_0000040> or <http://purl.obolibrary.org/obo/BFO_0000140>)")

    ;; --- BFO: conjunction with parenthesized negation (Domain/Range style) ---
    ("BFO: A and (not (B)) — conjunction with negation"
     "<http://purl.obolibrary.org/obo/BFO_0000004> and (not (<http://purl.obolibrary.org/obo/BFO_0000006>))")

    ;; --- BFO: 3-way disjunction where last disjunct is conjunction+negation ---
    ("BFO: A or B or (C and (not (D))) — complex disjunction"
     "<http://purl.obolibrary.org/obo/BFO_0000020> or <http://purl.obolibrary.org/obo/BFO_0000031> or (<http://purl.obolibrary.org/obo/BFO_0000004> and (not (<http://purl.obolibrary.org/obo/BFO_0000006>)))")

    ;; --- Quick Ref: cardinality + universal restriction in conjunction ---
    ;; (hasAge exactly 1) and hasAge only not NegInt
    ("QR: (prop exactly N) and prop only not Class"
     "(hasAge exactly 1) and hasAge only not NegInt")

    ;; --- Quick Ref: cardinality + universal with enumeration filler ---
    ;; hasGender exactly 1 and hasGender only {female , male}
    ("QR: prop exactly N and prop only {a , b}"
     "hasGender exactly 1 and hasGender only { female , male }")

    ;; --- Quick Ref: bare max cardinality (no filler class) ---
    ("QR: prop max N (no filler)"
     "hasSSN max 1")

    ;; --- Quick Ref: bare min cardinality (no filler class) ---
    ("QR: prop min N (no filler)"
     "hasSSN min 1")

    ;; --- Quick Ref: negated Self restriction ---
    ;; not (hates Self)
    ("QR: not (prop Self)"
     "not (hates Self)")

    ;; --- Quick Ref: data property value with plain literal ---
    ("QR: dataprop value \"literal\""
     "hasFirstName value \"John\"")

    ;; --- Quick Ref: data property value with typed literal (^^datatype) ---
    ("QR: dataprop value \"literal\"^^datatype"
     "hasFirstName value \"Jack\"^^xsd:string")

    ;; --- Quick Ref: disjunction of data property value restrictions ---
    ("QR: dataprop value \"x\" or dataprop value \"y\"^^type"
     "hasFirstName value \"John\" or hasFirstName value \"Jack\"^^xsd:string")

    ;; --- Quick Ref: (prop exactly 1) and prop only datatype ---
    ;; Simplified from: (hasFirstName exactly 1) and hasFirstName only xsd:string
    ("QR: (prop exactly 1) and prop only datatype"
     "(hasFirstName exactly 1) and hasFirstName only xsd:string")

    ;; --- Deeply nested: restriction inside conjunction inside disjunction ---
    ("Deep nesting: (A and (prop some B)) or (C and (prop only D))"
     "(ex:ClassA and (ex:rel some ex:ClassB)) or (ex:ClassC and (ex:rel only ex:ClassD))")

    ;; --- Double negation ---
    ("Double negation: not not A"
     "not not ex:ClassA")

    ;; --- Inverse property in complex restriction ---
    ("Inverse in universal with union filler"
     "inverse ex:partOf only (ex:Engine or ex:Transmission)")

    ;; --- Nested existential inside universal ---
    ("Nested: prop only (prop2 some A)"
     "ex:hasPart only (ex:madeOf some ex:Metal)")

    ;; --- Multiple restrictions in conjunction ---
    ("Multiple restrictions: prop some A and prop2 only B and prop3 min 2 C"
     "ex:hasPart some ex:Wheel and ex:madeOf only ex:Metal and ex:hasColor min 2 ex:Color")

    ;; --- Bare keyword classes ---
    ("Thing keyword"
     "Thing")
    ("Nothing keyword"
     "Nothing")
    ("Thing in conjunction"
     "ex:Foo and Thing")
    ("Nothing in disjunction"
     "ex:Foo or Nothing")

    ;; --- Exactly with filler ---
    ("Exactly with filler class"
     "ex:hasWheel exactly 4 ex:Wheel")

    ;; --- Existential with parenthesized conjunction filler ---
    ("some (A and B)"
     "ex:hasPart some (ex:Component and ex:Structural)")

    ;; --- Restriction on colon-only prefixed name (default namespace) ---
    ("Default namespace (colon prefix): :localName"
     ":hasPart some :Component")

    ;; --- Enumeration of one individual ---
    ("Singleton enumeration"
     "{ ex:Monday }")

    ;; --- Literal with language tag ---
    ("Data property value with language tag"
     "ex:hasLabel value \"hello\"@en")

    ;; --- Literal with complex language tag (e.g. en-US) ---
    ("Data property value with regional language tag"
     "ex:hasLabel value \"color\"@en-US")
    )
  "List of (DESCRIPTION INPUT) test cases.")

;; 4b. Negative test cases — inputs that must NOT parse as valid class expressions.
(defvar elot-negative-test-cases
  '(;; Empty string
    ("Reject: empty string"
     "")
    ;; Dangling keyword
    ("Reject: bare 'some'"
     "some")
    ;; Missing filler after 'some'
    ("Reject: prop some (no filler)"
     "ex:hasPart some")
    ;; Unmatched parenthesis
    ("Reject: unmatched open paren"
     "(ex:Foo and ex:Bar")
    ;; Bare integer
    ("Reject: bare integer"
     "42")
    ;; Double 'or' connective
    ("Reject: A or or B"
     "ex:Foo or or ex:Bar")
    ;; 'and' with nothing after
    ("Reject: trailing 'and'"
     "ex:Foo and")
    )
  "List of (DESCRIPTION INPUT) negative test cases (must fail to parse).")

;; 4c. SubPropertyChain positive test cases.
(defvar elot-sub-property-chain-test-cases
  '(("SubPropertyChain: two properties"
     "lis:hasActivityPart o lis:hasParticipant")
    ("SubPropertyChain: three properties"
     "lis:profileOfQuality o lis:qualityOf o lis:somethingElse")
    ("SubPropertyChain: full IRIs"
     "<http://example.org/p1> o <http://example.org/p2>")
    ("SubPropertyChain: inverse in chain"
     "inverse ex:partOf o ex:contains")
    ("SubPropertyChain: default namespace"
     ":prop1 o :prop2 o :prop3")
    )
  "List of (DESCRIPTION INPUT) SubPropertyChain positive test cases.")

;; 4d. SubPropertyChain negative test cases.
(defvar elot-sub-property-chain-negative-test-cases
  '(("Reject chain: single property (no 'o')"
     "ex:hasPart")
    ("Reject chain: missing second operand"
     "ex:hasPart o")
    ("Reject chain: empty string"
     "")
    ("Reject chain: bare 'o'"
     "o")
    )
  "List of (DESCRIPTION INPUT) SubPropertyChain negative test cases.")

;; 4e. Data range positive test cases.
(defvar elot-data-range-test-cases
  '(("DataRange: simple datatype"
     "xsd:string")
    ("DataRange: full IRI datatype"
     "<http://www.w3.org/2001/XMLSchema#integer>")
    ("DataRange: faceted restriction (single facet)"
     "xsd:integer [>= \"0\"^^xsd:integer]")
    ("DataRange: faceted restriction (two facets)"
     "xsd:integer [>= \"0\"^^xsd:integer , <= \"100\"^^xsd:integer]")
    ("DataRange: faceted restriction (minLength)"
     "xsd:string [minLength \"1\"^^xsd:integer]")
    ("DataRange: faceted restriction (pattern)"
     "xsd:string [pattern \"[A-Z]+\"]")
    ("DataRange: data conjunction"
     "xsd:integer and xsd:nonNegativeInteger")
    ("DataRange: data disjunction"
     "xsd:string or xsd:integer")
    ("DataRange: data negation"
     "not xsd:negativeInteger")
    ("DataRange: literal enumeration"
     "{ \"red\" , \"green\" , \"blue\" }")
    ("DataRange: parenthesized data range"
     "(xsd:integer or xsd:float)")
    ("DataRange: complex — not (A or B)"
     "not (xsd:string or xsd:integer)")
    ("DataRange: conjunction with negation"
     "xsd:integer and not xsd:negativeInteger")
    )
  "List of (DESCRIPTION INPUT) data range positive test cases.")

;; 4f. Data range negative test cases.
(defvar elot-data-range-negative-test-cases
  '(("Reject data range: empty string"
     "")
    ("Reject data range: bare bracket"
     "[>= \"0\"]")
    ("Reject data range: unclosed bracket"
     "xsd:integer [>= \"0\"")
    )
  "List of (DESCRIPTION INPUT) data range negative test cases.")

;; 5. Run the tests
(message "\n=== OWL Manchester Syntax Grammar Tests ===\n")
(let ((pass 0) (fail 0))
  ;; Positive cases: must parse successfully
  (dolist (tc elot-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-test-parse input)))
      (if ok
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input))))
  ;; Negative cases: must NOT parse successfully
  (dolist (tc elot-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-test-parse input)))
      (if (not ok)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; SubPropertyChain positive cases
  (dolist (tc elot-sub-property-chain-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-parse-sub-property-chain input)))
      (if ok
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input))))
  ;; SubPropertyChain negative cases: must NOT parse
  (dolist (tc elot-sub-property-chain-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-parse-sub-property-chain input)))
      (if (not ok)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Data range positive cases
  (dolist (tc elot-data-range-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-parse-data-range input)))
      (if ok
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input))))
  ;; Data range negative cases: must NOT parse
  (dolist (tc elot-data-range-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (ok    (elot-parse-data-range input)))
      (if (not ok)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  (message "\n%d passed, %d failed (of %d total)\n"
           pass fail (+ pass fail)))

;;; test-grammar.el ends here
