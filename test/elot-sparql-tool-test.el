;;; elot-sparql-tool-test.el --- Tests for the elot_sparql tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-sparql-tool-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 4 Step 4.1.  Covers:
;;
;;   Pure tests (always run):
;;     - mutating-p detects INSERT / DELETE / ... and is robust to
;;       leading whitespace and mixed case
;;     - sparql-truncate-text preserves header, drops surplus rows,
;;       appends the `... N more rows omitted' trailer
;;     - tool refuses missing/empty query, unknown format, mutating
;;       SPARQL when side-effects are disabled
;;     - tool refuses unsupported input extensions
;;     - path traversal still applies
;;     - no-ROBOT path produces an `ERROR:' line
;;
;;   Live tests (skipped when ROBOT unavailable):
;;     - SELECT against the Turtle fixture returns the expected
;;       binding for a known triple
;;     - LIMIT truncates and appends the trailer
;;     - a malformed query yields a structured error string, not
;;       a backtrace
;;
;; The live tests use `test/fixtures/smallttl.ttl', a tiny Turtle
;; fixture shipped with the repository.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-sparql-tool-test--repo-root repo-root))

(defvar elot-sparql-tool-test--repo-root nil)

(require 'elot-gptel)
(require 'elot-robot)

(defun elot-sparql-tool-test--live-or-skip ()
  (elot-robot-reset-cache)
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))

;;; ---------------------------------------------------------------------------
;;; Pure tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-sparql-tool-test-mutating-p ()
  "`elot-gptel--sparql-mutating-p' recognises SPARQL Update keywords."
  (should (elot-gptel--sparql-mutating-p "INSERT DATA { <a> <b> <c> }"))
  (should (elot-gptel--sparql-mutating-p "  delete where { ?s ?p ?o }"))
  (should (elot-gptel--sparql-mutating-p
           "PREFIX ex: <http://example.org/>\nINSERT {?s a ex:X} WHERE {?s ?p ?o}"))
  (should (elot-gptel--sparql-mutating-p "LOAD <http://example.org/>"))
  (should-not (elot-gptel--sparql-mutating-p "SELECT * WHERE { ?s ?p ?o }"))
  (should-not (elot-gptel--sparql-mutating-p
               "PREFIX ex: <http://example.org/>\nSELECT ?x WHERE { ?x a ex:Thing }"))
  ;; Note: the detector is deliberately lexical -- an UPDATE keyword in a
  ;; SPARQL comment is treated as mutating (false positive), erring on the
  ;; side of safety.  See `elot-gptel--sparql-mutating-p' docstring.
  ;; Empty / nil tolerated.
  (should-not (elot-gptel--sparql-mutating-p ""))
  (should-not (elot-gptel--sparql-mutating-p nil)))

(ert-deftest elot-sparql-tool-test-truncate-preserves-header ()
  "Header row survives row-limit truncation; trailer counts dropped rows."
  (let* ((text "id\tlabel\nA\tone\nB\ttwo\nC\tthree\nD\tfour\n")
         (out (elot-gptel--sparql-truncate-text text 2)))
    (should (string-prefix-p "id\tlabel\n" out))
    (should (string-match-p "^A\tone$" out))
    (should (string-match-p "^B\ttwo$" out))
    (should-not (string-match-p "^C\tthree$" out))
    (should (string-match-p "\\.\\.\\. 2 more rows omitted" out))))

(ert-deftest elot-sparql-tool-test-truncate-noop-under-limit ()
  "When data rows <= LIMIT, no trailer is appended."
  (let* ((text "id\tlabel\nA\tone\nB\ttwo\n")
         (out (elot-gptel--sparql-truncate-text text 5)))
    (should-not (string-match-p "more rows omitted" out))
    (should (string-match-p "^A\tone$" out))
    (should (string-match-p "^B\ttwo$" out))))

(ert-deftest elot-sparql-tool-test-truncate-empty ()
  "Empty input yields the empty string."
  (should (equal "" (elot-gptel--sparql-truncate-text "" 10)))
  (should (equal "" (elot-gptel--sparql-truncate-text nil 10))))

(ert-deftest elot-sparql-tool-test-org-table-basic ()
  "TSV is rendered as a well-formed Org table."
  (let* ((tsv "id\tlabel\nA\tone\nB\ttwo\n")
         (out (elot-gptel--sparql-tsv-to-org-table tsv)))
    (should (stringp out))
    ;; Header row present.
    (should (string-match-p "^| *id *| *label *|" out))
    ;; Separator line right after header (Org-aligned uses |--+--|).
    (should (string-match-p "|[-+]+|" out))
    ;; Data rows present.
    (should (string-match-p "| *A *| *one *|" out))
    (should (string-match-p "| *B *| *two *|" out))))

(ert-deftest elot-sparql-tool-test-org-table-escapes-pipe ()
  "Pipe characters in cells are replaced so the table grid is preserved."
  (let* ((tsv "id\tnote\nA\thas | pipe\n")
         (out (elot-gptel--sparql-tsv-to-org-table tsv)))
    ;; The literal '|' between 'has' and 'pipe' must not appear unescaped
    ;; (i.e. only the cell-boundary pipes remain).  The replacement char is
    ;; U+2502 BOX DRAWINGS LIGHT VERTICAL.
    (should (string-match-p "has \u2502 pipe" out))
    (should-not (string-match-p "has | pipe" out))))

(ert-deftest elot-sparql-tool-test-org-table-handles-empty ()
  "Empty or single-line TSV does not crash the renderer."
  (should (equal "" (elot-gptel--sparql-tsv-to-org-table "")))
  (should (equal "" (elot-gptel--sparql-tsv-to-org-table nil)))
  (should (equal "scalar"
                 (elot-gptel--sparql-tsv-to-org-table "scalar"))))

(ert-deftest elot-sparql-tool-test-format-result-table-with-trailer ()
  "`table' format keeps the truncation trailer outside the table body."
  (let* ((tmp (make-temp-file "elot-sparql-table-" nil ".tsv"))
         (text "id\tlabel\nA\tone\nB\ttwo\nC\tthree\n"))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert text))
          (let ((out (elot-gptel--sparql-format-result tmp "table" 1)))
            (should (stringp out))
            ;; Header rendered as an Org table.
            (should (string-match-p "^| *id *| *label *|" out))
            ;; Trailer is preserved and on its own line, not inside a row.
            (should (string-match-p "\\.\\.\\. 2 more rows omitted" out))
            ;; The trailer line is NOT wrapped in pipes.
            (should-not (string-match-p
                         "| *\\.\\.\\. 2 more rows omitted *|" out))))
      (ignore-errors (delete-file tmp)))))

(ert-deftest elot-sparql-tool-test-empty-query-refused ()
  "Empty / whitespace query produces an ERROR line, not a crash."
  (let ((default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/smallttl.ttl" "   " nil nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "missing or empty query" out)))))

(ert-deftest elot-sparql-tool-test-unknown-format-refused ()
  "Unknown FORMAT is rejected before ROBOT runs."
  (let ((default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/smallttl.ttl"
                "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
                "bogus" nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unknown format" out)))))

(ert-deftest elot-sparql-tool-test-mutating-refused ()
  "Mutating SPARQL is refused unless `elot-gptel-allow-side-effects' is set."
  (let ((default-directory elot-sparql-tool-test--repo-root)
        (elot-gptel-allow-side-effects nil))
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/smallttl.ttl"
                "INSERT DATA { <http://x> <http://p> <http://o> }"
                nil nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "mutating SPARQL refused" out)))))

(ert-deftest elot-sparql-tool-test-bad-extension-refused ()
  "Unsupported input extensions yield a structured error."
  (let ((default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/labels.csv"
                "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
                nil nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "unsupported input extension" out)))))

(ert-deftest elot-sparql-tool-test-traversal ()
  "Paths escaping the project root are refused."
  (let ((default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql
                "../../etc/passwd"
                "SELECT * WHERE { ?s ?p ?o }"
                nil nil)))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest elot-sparql-tool-test-no-robot ()
  "When ROBOT is unavailable, the tool returns a structured ERROR line."
  (let ((elot-robot-jar-path "/nonexistent/robot.jar")
        (elot-robot--available-cache 'unset)
        (elot-robot--invocation-cache nil)
        (exec-path nil)
        (default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/smallttl.ttl"
                "SELECT * WHERE { ?s ?p ?o }"
                nil nil)))
      (should (stringp out))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ROBOT not available" out)))))

;;; ---------------------------------------------------------------------------
;;; Live tests
;;; ---------------------------------------------------------------------------

(ert-deftest elot-sparql-tool-test-live-select ()
  "SELECT against the Turtle fixture returns a known binding."
  (elot-sparql-tool-test--live-or-skip)
  (let* ((default-directory elot-sparql-tool-test--repo-root)
         (out (elot-gptel-tool-sparql
               "test/fixtures/smallttl.ttl"
               (concat
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                "SELECT ?s ?label WHERE { ?s rdfs:label ?label } ORDER BY ?label")
               "tsv" nil)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    ;; Both labels from the fixture should appear.
    (should (string-match-p "thimble B" out))
    (should (string-match-p "rope C" out))))

(ert-deftest elot-sparql-tool-test-live-limit ()
  "LIMIT truncates the result and appends the trailer."
  (elot-sparql-tool-test--live-or-skip)
  (let* ((default-directory elot-sparql-tool-test--repo-root)
         (out (elot-gptel-tool-sparql
               "test/fixtures/smallttl.ttl"
               (concat
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                "SELECT ?s ?label WHERE { ?s rdfs:label ?label }")
               "tsv" 1)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (string-match-p "more row.* omitted" out))))

(ert-deftest elot-sparql-tool-test-live-table ()
  "Live SELECT with format=table returns an aligned Org table."
  (elot-sparql-tool-test--live-or-skip)
  (let* ((default-directory elot-sparql-tool-test--repo-root)
         (out (elot-gptel-tool-sparql
               "test/fixtures/smallttl.ttl"
               (concat
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                "SELECT ?s ?label WHERE { ?s rdfs:label ?label } ORDER BY ?label")
               "table" nil)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    ;; Header is rendered as an Org table row containing the variable names.
    ;; ROBOT preserves the leading `?' from SELECT variables in the TSV header.
    (should (string-match-p "| *\\?s *| *\\?label *|" out))
    ;; Both labels appear in cell positions.
    (should (string-match-p "thimble B" out))
    (should (string-match-p "rope C" out))
    ;; A separator row appears between header and data.
    (should (string-match-p "|[-+]+|" out))))

(ert-deftest elot-sparql-tool-test-live-malformed-query ()
  "A syntactically broken query yields a structured error string."
  (elot-sparql-tool-test--live-or-skip)
  (let* ((default-directory elot-sparql-tool-test--repo-root)
         (out (elot-gptel-tool-sparql
               "test/fixtures/smallttl.ttl"
               "this is not a sparql query"
               "tsv" nil)))
    (should (stringp out))
    (should (string-prefix-p "ERROR:" out))))

(ert-deftest elot-sparql-tool-test-query-form ()
  "`elot-gptel--sparql-query-form' classifies query forms after the prologue."
  (should (equal "select"
                 (elot-gptel--sparql-query-form
                  "SELECT * WHERE { ?s ?p ?o }")))
  (should (equal "ask"
                 (elot-gptel--sparql-query-form
                  "PREFIX ex: <http://example.org/>\nASK { ?s ?p ?o }")))
  (should (equal "construct"
                 (elot-gptel--sparql-query-form
                  "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")))
  (should (equal "describe"
                 (elot-gptel--sparql-query-form "DESCRIBE <http://x>")))
  (should (equal "insert"
                 (elot-gptel--sparql-query-form
                  "INSERT DATA { <a> <b> <c> }")))
  ;; Comments and prologue are stripped before classification.
  (should (equal "select"
                 (elot-gptel--sparql-query-form
                  "# a comment mentioning INSERT\nSELECT * WHERE { ?s ?p ?o }")))
  (should-not (elot-gptel--sparql-query-form ""))
  (should-not (elot-gptel--sparql-query-form nil)))

(ert-deftest elot-sparql-tool-test-select-or-ask-p ()
  "`elot-gptel--sparql-select-or-ask-p' accepts only SELECT and ASK."
  (should (elot-gptel--sparql-select-or-ask-p
           "SELECT * WHERE { ?s ?p ?o }"))
  (should (elot-gptel--sparql-select-or-ask-p
           "PREFIX ex: <http://x>\nASK { ?s ?p ?o }"))
  (should-not (elot-gptel--sparql-select-or-ask-p
               "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"))
  (should-not (elot-gptel--sparql-select-or-ask-p
               "DESCRIBE <http://x>"))
  (should-not (elot-gptel--sparql-select-or-ask-p
               "INSERT DATA { <a> <b> <c> }")))

(ert-deftest elot-sparql-tool-select-refuses-construct ()
  "`elot_sparql_select' refuses CONSTRUCT, even with side-effects enabled."
  (let ((default-directory elot-sparql-tool-test--repo-root)
        (elot-gptel-allow-side-effects t))
    (let ((out (elot-gptel-tool-sparql-select
                "test/fixtures/smallttl.ttl"
                "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"
                nil nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "SELECT or ASK" out)))))

(ert-deftest elot-sparql-tool-select-refuses-describe ()
  "`elot_sparql_select' refuses DESCRIBE."
  (let ((default-directory elot-sparql-tool-test--repo-root))
    (let ((out (elot-gptel-tool-sparql-select
                "test/fixtures/smallttl.ttl"
                "DESCRIBE <http://x>"
                nil nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "SELECT or ASK" out)))))

(ert-deftest elot-sparql-tool-select-refuses-update-with-flag ()
  "`elot_sparql_select' refuses mutating SPARQL even when side-effects allowed."
  (let ((default-directory elot-sparql-tool-test--repo-root)
        (elot-gptel-allow-side-effects t))
    (let ((out (elot-gptel-tool-sparql-select
                "test/fixtures/smallttl.ttl"
                "INSERT DATA { <a> <b> <c> }"
                nil nil)))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "SELECT or ASK" out)))))

(ert-deftest elot-sparql-tool-test-construct-permitted ()
  "`elot_sparql' permits CONSTRUCT without the side-effects flag (read-only form)."
  (let ((default-directory elot-sparql-tool-test--repo-root)
        (elot-gptel-allow-side-effects nil))
    ;; We can't run ROBOT in the pure-test path, but we can verify the
    ;; tool gets past the mutating-refusal gate and instead fails on
    ;; the ROBOT-availability check (or, if ROBOT is present, runs).
    (let ((out (elot-gptel-tool-sparql
                "test/fixtures/smallttl.ttl"
                "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } LIMIT 1"
                "tsv" nil)))
      (should (stringp out))
      ;; Whatever happens, it must not be the mutating-refusal message.
      (should-not (string-match-p "mutating SPARQL refused" out)))))

(ert-deftest elot-sparql-tool-test-live-sparql-select-ok ()
  "`elot_sparql_select' runs a SELECT happily."
  (elot-sparql-tool-test--live-or-skip)
  (let* ((default-directory elot-sparql-tool-test--repo-root)
         (out (elot-gptel-tool-sparql-select
               "test/fixtures/smallttl.ttl"
               (concat
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
                "SELECT ?s ?label WHERE { ?s rdfs:label ?label }")
               "tsv" nil)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (string-match-p "thimble B" out))))

(provide 'elot-sparql-tool-test)
;;; elot-sparql-tool-test.el ends here
