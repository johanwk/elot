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
   '(("transport-ont" "Transport ontology"
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
      (should (equal "transport-ont"  (plist-get c :ontology-iri)))
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
               "^ - rdfs:isDefinedBy :: transport-ont$"
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

(provide 'elot-gptel-db-borrow-test)
;;; elot-gptel-db-borrow-test.el ends here
