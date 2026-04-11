;;; test-grammar.el --- Test the OWL Manchester Syntax PEG grammar  -*- lexical-binding: t; -*-

;; Usage:  cd syntax && emacs --batch -l test-grammar.el

(require 'cl-lib)

;; 1. Load the grammar (hand-maintained; see owl-manchester.peggy for the
;;    original Peggy source and elot-owl-grammar.el for the peg.el version)
(load-file (expand-file-name "elot-owl-grammar.el"))

;; 3. Utility functions: parse INPUT starting from a specific grammar rule.
;;    Return t on full match, or the 1-based failure position otherwise.

(defun elot-parse-class-expression-list (input)
  "Try to parse INPUT as a comma-separated list of OWL Manchester Syntax class expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg class-expression-list))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-property-expression-list (input)
  "Try to parse INPUT as a comma-separated list of object property expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg object-property-expression-list))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-class-expression (input)
  "Try to parse INPUT as an OWL Manchester Syntax class expression.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg class-expression))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-property-expression (input)
  "Try to parse INPUT as an OWL Manchester Syntax object property expression.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg object-property-expression))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-sub-property-chain (input)
  "Try to parse INPUT as an OWL Manchester Syntax sub-property chain.
A chain is two or more objectPropertyExpressions joined by \='o\='.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg sub-property-chain))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-data-range (input)
  "Try to parse INPUT as an OWL Manchester Syntax data range.
Supports datatype IRIs, faceted restrictions (e.g. xsd:integer[>= 0]),
data conjunctions/disjunctions, and negation.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg data-range))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-fact (input)
  "Try to parse INPUT as an OWL Manchester Syntax fact.
A fact is [ `not' ] objectPropertyIRI (Literal | Individual).
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg fact))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

(defun elot-parse-individual-iri-list (input)
  "Try to parse INPUT as a comma-separated list of individuals.
Each individual is an IRI or a blank-node ID (_:name).
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case err
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg individual-iri-list))))
            (if (and ok (eobp)) t (point))))
      (error
       (message "  PARSE ERROR: %S" err)
       (point)))))

;; 3b. Test helper (used by the test runner below).
(defun elot-test-parse (input)
  "Try to parse INPUT as an OWL Manchester Syntax class expression.
Return t if the entire string is consumed, or the failure position.
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

;; 4g. Fact positive test cases.
(defvar elot-fact-test-cases
  '(("Fact: object property assertion"
     "hasWife Mary")
    ("Fact: negated object property assertion"
     "not hasChild Susan")
    ("Fact: data property with integer literal"
     "hasAge \"33\"^^xsd:integer")
    ("Fact: data property with plain literal"
     "hasFirstName \"John\"")
    ("Fact: data property with typed literal"
     "hasFirstName \"Jack\"^^xsd:string")
    ("Fact: object property with prefixed IRI"
     "ex:locatedInRegion ex:UnitedStates")
    ("Fact: data property with decimal literal"
     "ex:acidity_pH \"2.5\"^^xsd:decimal")
    ("Fact: object property with blank node"
     "hasChild _:child1")
    ("Fact: negated with prefixed names"
     "not ex:hasPart ex:wheel1")
    ("Fact: data property with language-tagged literal"
     "ex:hasLabel \"hello\"@en")
    ("Fact: full IRI object property"
     "<http://example.org/r> <http://example.org/b>")
    ("Fact: default namespace"
     ":hasPart :component1")
    )
  "List of (DESCRIPTION INPUT) Fact positive test cases.")

;; 4h. Fact negative test cases.
(defvar elot-fact-negative-test-cases
  '(("Reject fact: empty string"
     "")
    ("Reject fact: bare property (no object)"
     "hasWife")
    ("Reject fact: bare keyword 'not'"
     "not")
    ("Reject fact: class expression (not a fact)"
     "ex:A and ex:B")
    ("Reject fact: restriction syntax"
     "ex:hasPart some ex:Wheel")
    )
  "List of (DESCRIPTION INPUT) Fact negative test cases.")

;; 4i. Class expression list positive test cases.
(defvar elot-class-expression-list-test-cases
  '(("ClassExprList: single expression"
     "ex:Person")
    ("ClassExprList: two expressions"
     "ex:Person , ex:Agent")
    ("ClassExprList: three expressions with restrictions"
     "ex:Person , ex:hasAge some xsd:integer , ex:Agent")
    ("ClassExprList: complex expressions"
     "ex:A and ex:B , ex:C or ex:D")
    ("ClassExprList: from BFO — SubClassOf with comma"
     "hasSSN max 1, hasSSN min 1")
    ("ClassExprList: DisjointWith with comma"
     "g:Rock , g:Mineral")
    ("ClassExprList: Domain with comma (from quick ref)"
     "Person , Man")
    ("ClassExprList: Range with comma"
     "Person, Woman")
    ("ClassExprList: full IRIs comma-separated"
     "<http://purl.obolibrary.org/obo/BFO_0000004>, <http://purl.obolibrary.org/obo/BFO_0000020>, <http://purl.obolibrary.org/obo/BFO_0000031>")
    )
  "List of (DESCRIPTION INPUT) class expression list positive test cases.")

;; 4i-neg. Class expression list negative test cases.
(defvar elot-class-expression-list-negative-test-cases
  '(("Reject class expr list: empty string"
     "")
    ("Reject class expr list: trailing comma"
     "ex:A ,")
    ("Reject class expr list: double comma"
     "ex:A , , ex:B")
    )
  "List of (DESCRIPTION INPUT) class expression list negative test cases.")

;; 4i2. Object property expression list positive test cases.
(defvar elot-property-expression-list-test-cases
  '(("PropExprList: single property"
     "ex:hasPart")
    ("PropExprList: two properties"
     "hasSpouse, loves")
    ("PropExprList: SubPropertyOf with comma (from quick ref)"
     "hasSpouse , loves")
    ("PropExprList: InverseOf with comma (from quick ref)"
     "hasSpouse, inverse hasSpouse")
    ("PropExprList: DisjointWith properties"
     "hates , hates")
    )
  "List of (DESCRIPTION INPUT) object property expression list positive test cases.")

;; 4i2-neg. Object property expression list negative test cases.
(defvar elot-property-expression-list-negative-test-cases
  '(("Reject prop expr list: empty string"
     "")
    ("Reject prop expr list: trailing comma"
     "ex:hasPart ,")
    )
  "List of (DESCRIPTION INPUT) object property expression list negative test cases.")

;; 4j. Individual IRI list positive test cases.
(defvar elot-individual-iri-list-test-cases
  '(("IndividualList: single IRI"
     "ex:John")
    ("IndividualList: two IRIs"
     "Jack , Susan")
    ("IndividualList: three IRIs with prefixes"
     "ex:John , ex:Jack , ex:Joe")
    ("IndividualList: full IRIs"
     "<http://example.org/John> , <http://example.org/Mary>")
    ("IndividualList: mixed prefixed and bare"
     "John , ex:Mary , Susan")
    ("IndividualList: blank nodes"
     "_:child1 , _:child2")
    ("IndividualList: mixed IRIs and blank nodes"
     "ex:John , _:child1 , Mary")
    ("IndividualList: default namespace"
     ":a , :b , :c")
    )
  "List of (DESCRIPTION INPUT) individual IRI list positive test cases.")

;; 4j. Individual IRI list negative test cases.
(defvar elot-individual-iri-list-negative-test-cases
  '(("Reject individual list: empty string"
     "")
    ("Reject individual list: trailing comma"
     "ex:John ,")
    ("Reject individual list: double comma"
     "ex:John , , ex:Mary")
    )
  "List of (DESCRIPTION INPUT) individual IRI list negative test cases.")

;; 5. Run the tests
(message "\n=== OWL Manchester Syntax Grammar Tests ===\n")
(let ((pass 0) (fail 0))
  ;; Positive cases: must parse successfully
  (dolist (tc elot-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-test-parse input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Negative cases: must NOT parse successfully
  (dolist (tc elot-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-test-parse input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; SubPropertyChain positive cases
  (dolist (tc elot-sub-property-chain-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-sub-property-chain input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; SubPropertyChain negative cases: must NOT parse
  (dolist (tc elot-sub-property-chain-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-sub-property-chain input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Data range positive cases
  (dolist (tc elot-data-range-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-data-range input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Data range negative cases: must NOT parse
  (dolist (tc elot-data-range-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-data-range input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Fact positive cases
  (dolist (tc elot-fact-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-fact input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Fact negative cases: must NOT parse
  (dolist (tc elot-fact-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-fact input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Individual IRI list positive cases
  (dolist (tc elot-individual-iri-list-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-individual-iri-list input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Individual IRI list negative cases: must NOT parse
  (dolist (tc elot-individual-iri-list-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-individual-iri-list input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Class expression list positive cases
  (dolist (tc elot-class-expression-list-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-class-expression-list input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Class expression list negative cases: must NOT parse
  (dolist (tc elot-class-expression-list-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-class-expression-list input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  ;; Object property expression list positive cases
  (dolist (tc elot-property-expression-list-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-property-expression-list input)))
      (if (eq result t)
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s" desc)
        (message "        input: %s" input)
        (when (integerp result)
          (message "        stopped at column: %d" result)))))
  ;; Object property expression list negative cases: must NOT parse
  (dolist (tc elot-property-expression-list-negative-test-cases)
    (let* ((desc  (car tc))
           (input (cadr tc))
           (result (elot-parse-property-expression-list input)))
      (if (not (eq result t))
          (progn (cl-incf pass)
                 (message "  PASS: %s" desc))
        (cl-incf fail)
        (message "  FAIL: %s  (should not have parsed!)" desc)
        (message "        input: %s" input))))
  (message "\n%d passed, %d failed (of %d total)\n"
           pass fail (+ pass fail)))

;;; test-grammar.el ends here
