;;; elot-gptel-db-borrow-test.el --- Tests for M6.5 borrow-term  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-borrow-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-borrow-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.5 (second half):
;; tests for `elot-db-entity-citation' and the LLM-facing tool
;; `elot-gptel-tool-db-borrow-term'.
;;
;; Pure-Elisp; no ROBOT required.

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

;;;; Harness ------------------------------------------------------------

(defvar elot-gptel-db-borrow-test--tmpfile nil)

(defun elot-gptel-db-borrow-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-borrow-test--tmpfile
             (file-exists-p elot-gptel-db-borrow-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-borrow-test--tmpfile)))
  (setq elot-gptel-db-borrow-test--tmpfile
        (make-temp-file "elot-gptel-db-borrow-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-borrow-test--tmpfile))
  (elot-db-init elot-gptel-db-borrow-test--tmpfile))

(defun elot-gptel-db-borrow-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-borrow-test--tmpfile
             (file-exists-p elot-gptel-db-borrow-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-borrow-test--tmpfile)))
  (setq elot-gptel-db-borrow-test--tmpfile nil))

(defmacro elot-gptel-db-borrow-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-borrow-test--fresh-db) ,@body)
     (elot-gptel-db-borrow-test--teardown)))

(defun elot-gptel-db-borrow-test--seed ()
  "Seed two sources.

`transport' has a full picture: an `owl:Ontology' declaration
with a `dcterms:title', plus a Vehicle class with an
English-tagged label and a skos:definition.

`minimal' has just one class and no ontology declaration -- so
borrowing from it should fall back to a `(source: minimal)'
citation."
  (elot-db-update-source
   "transport" nil "org"
   '(("http://example.org/transport/ http://example.org/transport/1.0"
      "Transport ontology"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Transport Vocabulary"))
     ("trn:vehicle" "Vehicle"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Vehicle" "en")
       "skos:definition"
       "A means of carrying or transporting people or goods."))
     ("trn:drives" "drives"
      ("rdf:type" "owl:ObjectProperty"))))
  (elot-db-update-source
   "minimal" nil "org"
   '(("min:thing" "Thing"
      ("rdf:type" "owl:Class")))))

;;;; Pure: elot-db-entity-citation -------------------------------------

(ert-deftest test-elot-db-entity-citation-known ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((c (elot-db-entity-citation "trn:vehicle")))
      (should c)
      (should (equal "trn:vehicle"    (plist-get c :id)))
      (should (equal "Vehicle"        (plist-get c :label)))
      (should (equal "en"             (plist-get c :label-lang)))
      (should (equal "owl:Class"      (plist-get c :rdf-type)))
      (should (equal "transport"      (plist-get c :source)))
      ;; Composite "IRI VERSIONIRI" form -> the unversioned IRI.
      (should (equal "http://example.org/transport/"
                     (plist-get c :ontology-iri)))
      (should (equal "Transport Vocabulary"
                     (plist-get c :ontology-title)))
      (should (string-match-p "carrying or transporting"
                              (plist-get c :definition))))))

(ert-deftest test-elot-db-entity-citation-unknown ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (should-not (elot-db-entity-citation "ex:does-not-exist"))))

(ert-deftest test-elot-db-entity-citation-bracketed-iri ()
  ;; Angle brackets are stripped before lookup; only matters when the
  ;; stored id is the bare IRI form.  Here we use the CURIE form.
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((c (elot-db-entity-citation "<trn:vehicle>")))
      (should c)
      (should (equal "trn:vehicle" (plist-get c :id))))))

(ert-deftest test-elot-db-entity-citation-no-ontology ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((c (elot-db-entity-citation "min:thing")))
      (should c)
      (should-not (plist-get c :ontology-iri))
      (should-not (plist-get c :ontology-title))
      (should-not (plist-get c :definition))
      (should (equal "minimal" (plist-get c :source))))))

(ert-deftest test-elot-db-entity-citation-empty-token ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (should-error (elot-db-entity-citation "") :type 'user-error)
    (should-error (elot-db-entity-citation nil) :type 'user-error)))

;;;; Provenance: CURIE isDefinedBy, unversioned IRI, :ontology-iri-from -

(defun elot-gptel-db-borrow-test--seed-pattern ()
  "Seed a pattern-style source that merely *names* a foreign term.

`patternsrc' declares its own versioned ontology heading (ELOT's
composite \"IRI VERSIONIRI\" form) and carries `iof-core:Denoter'
with a bare-prefix `rdfs:isDefinedBy :: iof-core:' row -- the
exact shape that used to yield the pattern file's own version
IRI as the citation."
  (elot-db-update-source
   "patternsrc" nil "org"
   '(("http://example.org/pattern/p http://example.org/pattern/p/0.0"
      "Pattern ontology"
      ("rdf:type" "owl:Ontology"))
     ("iof-core:Denoter" "Denoter"
      ("rdf:type" "owl:Class"
       "rdfs:isDefinedBy" "iof-core:"))
     ("bad:Term" "Bad"
      ("rdf:type" "owl:Class"
       "rdfs:isDefinedBy" "nosuch:"))))
  (elot-db-add-prefix "patternsrc" nil "iof-core"
                      "https://example.org/ontology/core/Core/"))

(ert-deftest test-elot-db-citation-curie-defined-by-resolved ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    (let ((c (elot-db-entity-citation "iof-core:Denoter")))
      (should (equal "https://example.org/ontology/core/Core/"
                     (plist-get c :ontology-iri)))
      (should (eq 'explicit (plist-get c :ontology-iri-from))))))

(ert-deftest test-elot-db-citation-curie-defined-by-unresolvable ()
  ;; Unknown prefix -> treat as absent, fall back to the source's own
  ;; ontology declaration (unversioned).
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    (let ((c (elot-db-entity-citation "bad:Term")))
      (should (equal "http://example.org/pattern/p"
                     (plist-get c :ontology-iri)))
      (should (eq 'source-declaration (plist-get c :ontology-iri-from))))))

(ert-deftest test-elot-db-citation-prefix-scoped-to-source ()
  ;; `iof-core:' must not be answered by another source's prefix table.
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    (should-not (elot-db--expansion-in-source-only "iof-core" "minimal" nil))
    (should (equal "https://example.org/ontology/core/Core/"
                   (elot-db--expansion-in-source-only
                    "iof-core" "patternsrc" nil)))))

(ert-deftest test-elot-db-unversioned-ontology-iri ()
  ;; Token normalisation may consult the source's prefix rows (CURIE
  ;; branch), so an open connection is required even though `nosuch'
  ;; has none.
  (elot-gptel-db-borrow-test--with-fresh-db
    (should (equal "http://example.org/o"
                   (elot-db--unversioned-ontology-iri
                    "http://example.org/o http://example.org/o/1.2" "nosuch" nil)))
    (should (equal "http://example.org/o"
                   (elot-db--unversioned-ontology-iri
                    "http://example.org/o" "nosuch" nil)))
    ;; Angle-bracketed tokens are unwrapped; an unresolvable leading
    ;; token (here the default-prefix `:') is skipped, not emitted.
    (should (equal "https://example.org/ontology/core/Core/"
                   (elot-db--unversioned-ontology-iri
                    ": <https://example.org/ontology/core/Core/>" "nosuch" nil)))
    (should-not (elot-db--unversioned-ontology-iri nil "nosuch" nil))))

(ert-deftest test-elot-db-citation-absolute-iri-normalisation ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    ;; Quoted / typed literal.
    (should (equal "https://example.org/x/"
                   (elot-db--citation-absolute-iri
                    "\"https://example.org/x/\"^^xsd:anyURI" "patternsrc" nil)))
    ;; Angle brackets, bare IRI.
    (should (equal "https://example.org/x/"
                   (elot-db--citation-absolute-iri
                    "<https://example.org/x/>" "patternsrc" nil)))
    ;; CURIE resolved through the source's own prefix rows.
    (should (equal "https://example.org/ontology/core/Core/"
                   (elot-db--citation-absolute-iri
                    "iof-core:" "patternsrc" nil)))
    ;; Unresolvable CURIE -> nil, NEVER the CURIE itself.
    (should-not (elot-db--citation-absolute-iri "bad:Term" "patternsrc" nil))
    (should-not (elot-db--citation-absolute-iri "" "patternsrc" nil))))

;;;; Pure formatters ---------------------------------------------------

(ert-deftest test-elot-gptel-db-borrow-format-label-with-lang ()
  (should (equal "\"Vehicle\"@en"
                 (elot-gptel--db-borrow-format-label "Vehicle" "en"))))

(ert-deftest test-elot-gptel-db-borrow-format-label-no-lang ()
  (should (equal "\"Vehicle\""
                 (elot-gptel--db-borrow-format-label "Vehicle" nil)))
  (should (equal "\"Vehicle\""
                 (elot-gptel--db-borrow-format-label "Vehicle" ""))))

(ert-deftest test-elot-gptel-db-borrow-format-definition-strips-quotes ()
  (should (equal "\"hello\""
                 (elot-gptel--db-borrow-format-definition "\"hello\"")))
  (should (equal "\"hello\""
                 (elot-gptel--db-borrow-format-definition "hello"))))

;;;; Integration: elot-gptel-tool-db-borrow-term -----------------------

(ert-deftest test-elot-gptel-tool-db-borrow-term-full ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "trn:vehicle")))
      (should (stringp out))
      ;; Heading line with label and CURIE.
      (should (string-match-p "^\\* \"Vehicle\"@en (trn:vehicle)" out))
      ;; rdfs:isDefinedBy points at the ontology id.
      (should (string-match-p
               "^ - rdfs:isDefinedBy :: http://example.org/transport/$"
               out))
      ;; skos:definition appears in quoted form.
      (should (string-match-p
               "^ - skos:definition :: \"A means of carrying"
               out))
      ;; Provenance comment carries rdf:type / title / source.
      (should (string-match-p "rdf:type owl:Class" out))
      (should (string-match-p "source title: Transport Vocabulary" out))
      (should (string-match-p "source: transport" out))
      ;; Placeholder note is present.
      (should (string-match-p "NOTE: the leading `\\*' is a placeholder"
                              out)))))

(ert-deftest test-elot-gptel-tool-db-borrow-term-source-fallback ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "min:thing")))
      (should (string-match-p "^\\* \"Thing\" (min:thing)" out))
      ;; No ontology declaration -> source-fallback citation.
      (should (string-match-p
               "^ - rdfs:isDefinedBy :: (source: minimal)$"
               out))
      ;; No skos:definition line when the DB has none cached.
      (should-not (string-match-p "skos:definition" out)))))

(ert-deftest test-elot-gptel-tool-db-borrow-term-unknown ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "ex:does-not-exist")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "ex:does-not-exist" out)))))

(ert-deftest test-elot-gptel-tool-db-borrow-term-empty-token ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-borrow-term "")))
      (should (string-prefix-p "ERROR:" out)))))

;;;; Step 7.5.7: contingent NOTE detectors -----------------------------

(ert-deftest test-elot-gptel-db-borrow-default-prefix-detector ()
  (should (elot-gptel--db-borrow-default-prefix-p ":Food"))
  (should (elot-gptel--db-borrow-default-prefix-p ":Margherita"))
  (should-not (elot-gptel--db-borrow-default-prefix-p "pizza:Food"))
  (should-not (elot-gptel--db-borrow-default-prefix-p "trn:vehicle"))
  (should-not (elot-gptel--db-borrow-default-prefix-p
               "http://example.org/Food"))
  (should-not (elot-gptel--db-borrow-default-prefix-p "::weird"))
  (should-not (elot-gptel--db-borrow-default-prefix-p ""))
  (should-not (elot-gptel--db-borrow-default-prefix-p nil)))

(ert-deftest test-elot-gptel-db-borrow-label-missing-detector ()
  (should (elot-gptel--db-borrow-label-missing-p nil ":Food"))
  (should (elot-gptel--db-borrow-label-missing-p "" ":Food"))
  (should (elot-gptel--db-borrow-label-missing-p ":Food" ":Food"))
  (should-not (elot-gptel--db-borrow-label-missing-p "Vehicle"
                                                     "trn:vehicle"))
  (should-not (elot-gptel--db-borrow-label-missing-p "Food" ":Food")))

;;;; Step 7.5.7: contingent NOTEs in the rendered snippet --------------

(defun elot-gptel-db-borrow-test--seed-default-prefix ()
  "Seed a `pizza-mini' source where `:Food' is default-prefix
and the stored label equals the id (no human label)."
  (elot-db-update-source
   "pizza-mini" nil "org"
   '(("pizza-mini-ont" "Pizza Mini Ontology"
      ("rdf:type" "owl:Ontology"
       "dcterms:title" "Pizza Mini"))
     (":Food" ":Food"
      ("rdf:type" "owl:Class"))))
  (elot-db-add-prefix
   "pizza-mini" nil ""
   "https://raw.githubusercontent.com/owlcs/pizza-ontology/refs/heads/master/pizza.owl#"))

(ert-deftest test-elot-gptel-tool-db-borrow-term-default-prefix-notes ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (elot-gptel-db-borrow-test--seed-default-prefix)
    (let ((out (elot-gptel-tool-db-borrow-term ":Food")))
      (should (stringp out))
      ;; Heading uses TODO-label placeholder, not the echoed id.
      (should (string-match-p "^\\* \"TODO-label\" (:Food)" out))
      ;; Default-prefix NOTE fires and cites the localname.
      (should (string-match-p
               "NOTE: source uses default-prefix form `:Food'" out))
      ;; The suggested prefix expansion is the empty-prefix row.
      (should (string-match-p "pizza-ontology" out))
      ;; Label-missing NOTE fires.
      (should (string-match-p
               "NOTE: source has no human-readable rdfs:label" out))
      ;; Original placeholder NOTE still present.
      (should (string-match-p "NOTE: the leading `\\*' is a placeholder"
                              out)))))

(ert-deftest test-elot-gptel-tool-db-borrow-term-no-contingent-notes ()
  ;; For a well-shaped CURIE with a real label, neither contingent
  ;; NOTE should fire.
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "trn:vehicle")))
      (should-not (string-match-p "default-prefix form" out))
      (should-not (string-match-p "no human-readable rdfs:label" out))
      ;; Original placeholder NOTE still present.
      (should (string-match-p "NOTE: the leading `\\*' is a placeholder"
                              out)))))

;;;; Step 7.5.3: kind-note (heading-nesting role by rdf:type) ----------

(ert-deftest test-elot-gptel-db-borrow-kind-note-class ()
  (let ((note (elot-gptel--db-borrow-kind-note "owl:Class")))
    (should (stringp note))
    (should (string-match-p "SubClassOf" note))
    (should (string-match-p "CHILDREN of this heading" note))
    (should (string-match-p "do NOT use" note))))

(ert-deftest test-elot-gptel-db-borrow-kind-note-object-property ()
  (let ((note (elot-gptel--db-borrow-kind-note "owl:ObjectProperty")))
    (should (stringp note))
    (should (string-match-p "SubPropertyOf" note))
    (should (string-match-p "object-property headings" note))))

(ert-deftest test-elot-gptel-db-borrow-kind-note-data-property ()
  (let ((note (elot-gptel--db-borrow-kind-note "owl:DatatypeProperty")))
    (should (stringp note))
    (should (string-match-p "SubPropertyOf" note))
    (should (string-match-p "data-property headings" note))))

(ert-deftest test-elot-gptel-db-borrow-kind-note-annotation-property ()
  (let ((note (elot-gptel--db-borrow-kind-note
               "owl:AnnotationProperty")))
    (should (stringp note))
    (should (string-match-p "SubPropertyOf" note))
    (should (string-match-p "annotation-property headings" note))))

(ert-deftest test-elot-gptel-db-borrow-kind-note-individual ()
  ;; Individuals and datatypes get no kind-note -- there is no
  ;; sub-relationship for them.
  (should-not (elot-gptel--db-borrow-kind-note "owl:NamedIndividual"))
  (should-not (elot-gptel--db-borrow-kind-note "owl:Datatype"))
  (should-not (elot-gptel--db-borrow-kind-note nil))
  (should-not (elot-gptel--db-borrow-kind-note "")))

(ert-deftest test-elot-gptel-tool-db-borrow-term-kind-note-class ()
  ;; trn:vehicle is owl:Class -- kind-note must fire.
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "trn:vehicle")))
      (should (string-match-p "SubClassOf" out))
      (should (string-match-p "CHILDREN of this heading" out)))))

(ert-deftest test-elot-gptel-tool-db-borrow-term-kind-note-op ()
  ;; trn:drives is owl:ObjectProperty -- kind-note must fire.
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed)
    (let ((out (elot-gptel-tool-db-borrow-term "trn:drives")))
      (should (string-match-p "SubPropertyOf" out))
      (should (string-match-p "object-property headings" out)))))

;;;; Work item 5: advisory provenance NOTE ------------------------------

(ert-deftest test-elot-gptel-borrow-namespace-mismatch-p ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    ;; iof-core: expands outside the pattern file's own ontology IRI.
    (should (elot-gptel--db-borrow-namespace-mismatch-p
             "iof-core:Denoter" "http://example.org/pattern/p"
             "patternsrc" nil))
    ;; Same namespace -> no mismatch.
    (should-not (elot-gptel--db-borrow-namespace-mismatch-p
                 "iof-core:Denoter"
                 "https://example.org/ontology/core/Core/"
                 "patternsrc" nil))
    ;; Unresolvable prefix -> no signal (never guess).
    (should-not (elot-gptel--db-borrow-namespace-mismatch-p
                 "nosuch:Term" "http://example.org/pattern/p"
                 "patternsrc" nil))))

(ert-deftest test-elot-gptel-borrow-provenance-note-fires ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    (let ((note (elot-gptel--db-borrow-provenance-note
                 (list :id "iof-core:Denoter"
                       :ontology-iri "http://example.org/pattern/p"
                       :ontology-iri-from 'source-declaration
                       :source "patternsrc"))))
      (should (stringp note))
      (should (string-match-p "NOTE:" note))
      (should (string-match-p "merely NAME" note)))))

(ert-deftest test-elot-gptel-borrow-provenance-note-silent-when-explicit ()
  (elot-gptel-db-borrow-test--with-fresh-db
    (elot-gptel-db-borrow-test--seed-pattern)
    ;; Explicit rdfs:isDefinedBy -> never warn, whatever the namespace.
    (should-not (elot-gptel--db-borrow-provenance-note
                 (list :id "iof-core:Denoter"
                       :ontology-iri "http://example.org/pattern/p"
                       :ontology-iri-from 'explicit
                       :source "patternsrc")))))

(provide 'elot-gptel-db-borrow-test)
;;; elot-gptel-db-borrow-test.el ends here
