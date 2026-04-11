;;; test-grammar.el --- Test the OWL Manchester Syntax PEG grammar  -*- lexical-binding: t; -*-

;; Usage:  cd syntax && emacs --batch -l test-grammar.el
;;
;; Test cases are loaded from test-cases.json (the single source of truth
;; shared with the TypeScript / Peggy test runner).

(require 'cl-lib)
(require 'json)

;; 1. Load the grammar (hand-maintained; see owl-manchester.peggy for the
;;    original Peggy source and elot-owl-grammar.el for the peg.el version)
(load-file (expand-file-name "elot-owl-grammar.el"))

;; 2. Utility functions: parse INPUT starting from a specific grammar rule.
;;    Return t on full match, or the 1-based failure position otherwise.

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

;; 3. Mapping from JSON startRule names (PascalCase, Peggy convention) to
;;    the Elisp parser functions defined above.

(defvar elot-test-start-rule-parsers
  '(("ClassExpression"                . elot-parse-class-expression)
    ("ClassExpressionList"            . elot-parse-class-expression-list)
    ("ObjectPropertyExpressionList"   . elot-parse-property-expression-list)
    ("SubPropertyChain"               . elot-parse-sub-property-chain)
    ("DataRange"                      . elot-parse-data-range)
    ("Fact"                           . elot-parse-fact)
    ("IndividualIRIList"              . elot-parse-individual-iri-list))
  "Alist mapping JSON startRule names to Elisp parser functions.")

;; 4. Load test cases from the shared JSON file.

(defvar elot-test-json-file
  (expand-file-name "test-cases.json")
  "Path to the shared JSON test-case file.")

(defun elot-load-test-cases ()
  "Load test cases from `elot-test-json-file'.
Return the parsed JSON as a list of test groups."
  (let* ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'string)
         (data (json-read-file elot-test-json-file)))
    (cdr (assoc "testGroups" data))))

;; 5. Run the tests.

(message "\n=== OWL Manchester Syntax Grammar Tests ===\n")
(let ((pass 0) (fail 0)
      (groups (elot-load-test-cases)))
  (dolist (group groups)
    (let* ((start-rule (cdr (assoc "startRule" group)))
           (parser-fn  (cdr (assoc start-rule elot-test-start-rule-parsers)))
           (positives  (cdr (assoc "positive" group)))
           (negatives  (cdr (assoc "negative" group))))
      (unless parser-fn
        (error "No parser function for startRule: %s" start-rule))
      ;; Positive cases: must parse successfully
      (dolist (tc positives)
        (let* ((desc   (car tc))
               (input  (cadr tc))
               (result (funcall parser-fn input)))
          (if (eq result t)
              (progn (cl-incf pass)
                     (message "  PASS: %s" desc))
            (cl-incf fail)
            (message "  FAIL: %s" desc)
            (message "        input: %s" input)
            (when (integerp result)
              (message "        stopped at column: %d" result)))))
      ;; Negative cases: must NOT parse successfully
      (dolist (tc negatives)
        (let* ((desc   (car tc))
               (input  (cadr tc))
               (result (funcall parser-fn input)))
          (if (not (eq result t))
              (progn (cl-incf pass)
                     (message "  PASS: %s" desc))
            (cl-incf fail)
            (message "  FAIL: %s  (should not have parsed!)" desc)
            (message "        input: %s" input))))))
  (message "\n%d passed, %d failed (of %d total)\n"
           pass fail (+ pass fail)))

;;; test-grammar.el ends here
