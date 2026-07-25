;;; elot-gptel-db-search-test.el --- Tests for M6.5 search-label  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-search-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-search-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.5 (first half):
;; tests for the reuse-before-mint search tool
;; `elot-gptel-tool-db-search-label' and its underlying
;; `elot-db-search-entities' helper.
;;
;; Two complementary sources are seeded to confirm that the search
;; spans the entire DB (not just the buffer's active sources):
;;   - `transport': a small "ontology" with vehicles/persons.
;;   - `animals':   a separate source with a Snake individual.
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

(defvar elot-gptel-db-search-test--tmpfile nil)

(defun elot-gptel-db-search-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-search-test--tmpfile
             (file-exists-p elot-gptel-db-search-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-search-test--tmpfile)))
  (setq elot-gptel-db-search-test--tmpfile
        (make-temp-file "elot-gptel-db-search-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-search-test--tmpfile))
  (elot-db-init elot-gptel-db-search-test--tmpfile))

(defun elot-gptel-db-search-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-search-test--tmpfile
             (file-exists-p elot-gptel-db-search-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-search-test--tmpfile)))
  (setq elot-gptel-db-search-test--tmpfile nil))

(defmacro elot-gptel-db-search-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-search-test--fresh-db) ,@body)
     (elot-gptel-db-search-test--teardown)))

(defun elot-gptel-db-search-test--seed ()
  "Seed two complementary sources for search testing.

`transport' carries an ontology declaration (Vehicle / Person)
plus an English-tagged Vehicle label.  `animals' carries a
Snake individual with a Norwegian-tagged label.  The two
ontologies live in separate source rows so a search must span
both to find a complete picture."
  (elot-db-update-source
   "transport" nil "org"
   '(("transport-ont" "Transport ontology"
      ("rdf:type" "owl:Ontology"))
     ("trn:vehicle" "Vehicle"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Vehicle" "en")
       "skos:definition" "A means of carrying or transporting"))
     ("trn:person"  "Person"
      ("rdf:type" "owl:Class"))
     ("trn:drives" "drives"
      ("rdf:type" "owl:ObjectProperty"))))
  (elot-db-update-source
   "animals" nil "org"
   '(("animals-ont" "Animals ontology"
      ("rdf:type" "owl:Ontology"))
     ("anm:snake" "Snake"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Slange" "nb")))
     ("anm:medusa" "Medusa"
      ("rdf:type" "owl:NamedIndividual"
       "Types" "anm:snake"))))
  ;; Default-prefix entity (M11.1): a class stored under `:Margherita'
  ;; rather than under any explicit prefix.  Mirrors the pizza.org
  ;; situation that motivates the cross-prefix local-name fallback.
  (elot-db-update-source
   "pizza-mini" nil "org"
   '(("pizza-mini-ont" "Pizza-mini ontology"
      ("rdf:type" "owl:Ontology"))
     (":Margherita" "Margherita"
      ("rdf:type" "owl:Class"
       "rdfs:label" ("Margherita" "pt"))))))

;;;; Pure: kind-curie resolver -----------------------------------------

(ert-deftest test-elot-db-search-kind-curie-known ()
  (should (equal "owl:Class" (elot-db--search-kind-curie "class")))
  (should (equal "owl:ObjectProperty"
                 (elot-db--search-kind-curie "object-property")))
  (should (equal "owl:NamedIndividual"
                 (elot-db--search-kind-curie "individual")))
  (should (equal "owl:Ontology"
                 (elot-db--search-kind-curie "ontology"))))

(ert-deftest test-elot-db-search-kind-curie-nil ()
  (should-not (elot-db--search-kind-curie nil))
  (should-not (elot-db--search-kind-curie "")))

(ert-deftest test-elot-db-search-kind-curie-unknown ()
  (should-error (elot-db--search-kind-curie "thingamajig")
                :type 'user-error))

;;;; Integration: elot-db-search-entities ------------------------------

(ert-deftest test-elot-db-search-entities-spans-all-sources ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((rows (elot-db-search-entities "e")))
      ;; Should find rows from BOTH sources.
      (let ((sources (mapcar (lambda (r) (nth 4 r)) rows)))
        (should (member "transport" sources))
        (should (member "animals" sources))))))

(ert-deftest test-elot-db-search-entities-substring ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let* ((rows (elot-db-search-entities "Vehicle")))
      (should (= 1 (length rows)))
      (should (equal "trn:vehicle" (nth 0 (car rows))))
      (should (equal "Vehicle"     (nth 1 (car rows))))
      ;; rdf_type column resolves to owl:Class.
      (should (equal "owl:Class"   (nth 2 (car rows))))
      ;; ontology_iri column is the source's owl:Ontology id.
      (should (equal "transport-ont" (nth 3 (car rows)))))))

(ert-deftest test-elot-db-search-entities-id-match ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; Searching for the CURIE local-name matches the id column.
    (let ((rows (elot-db-search-entities "drives")))
      (should (= 1 (length rows)))
      (should (equal "trn:drives" (nth 0 (car rows)))))))

(ert-deftest test-elot-db-search-entities-kind-filter ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; Filter to classes only: Vehicle, Person, Snake but not drives
    ;; (object-property) and not Medusa (individual).
    (let* ((rows (elot-db-search-entities "%" 50 "class"))
           (ids  (mapcar #'car rows)))
      (should (member "trn:vehicle" ids))
      (should (member "trn:person" ids))
      (should (member "anm:snake" ids))
      (should-not (member "trn:drives" ids))
      (should-not (member "anm:medusa" ids))
      (should-not (member "transport-ont" ids)))))

(ert-deftest test-elot-db-search-entities-source-filter ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((rows (elot-db-search-entities "%" 50 nil "animals")))
      (should (cl-every (lambda (r) (equal "animals" (nth 4 r))) rows))
      ;; Animals source has 3 entities (ontology + Snake + Medusa).
      (should (= 3 (length rows))))))

;;;; M11.1: cross-prefix local-name fallback ---------------------------

(ert-deftest test-elot-db-search-entities-curie-shape ()
  (should     (elot-db--search-curie-shaped-p "ex:Margherita"))
  (should     (elot-db--search-curie-shaped-p "pizza:IceCream"))
  (should-not (elot-db--search-curie-shaped-p ":Margherita"))
  (should-not (elot-db--search-curie-shaped-p "Margherita"))
  (should-not (elot-db--search-curie-shaped-p "%Margherita%"))
  (should-not (elot-db--search-curie-shaped-p "ex:%"))
  (should (equal "Margherita"
                 (elot-db--search-curie-localname "ex:Margherita"))))

(ert-deftest test-elot-db-search-entities-cross-prefix-fallback ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; `ex:Margherita' is not in the DB, but `:Margherita' is.
    ;; The fallback should surface the default-prefix row, marked
    ;; `local-name'.
    (let* ((rows (elot-db-search-entities "ex:Margherita"))
           (row  (car rows)))
      (should (= 1 (length rows)))
      (should (equal ":Margherita" (nth 0 row)))
      (should (equal "local-name"  (nth 6 row))))))

(ert-deftest test-elot-db-search-entities-fallback-disabled ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; With exact-only, the fallback is suppressed.
    (let ((rows (elot-db-search-entities
                 "ex:Margherita" nil nil nil nil t)))
      (should (null rows)))))

(ert-deftest test-elot-db-search-entities-fallback-no-overlap ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; Direct query for the actual prefix-bare id reaches the primary
    ;; pass; the fallback (if applied) must not duplicate the row.
    (let* ((rows (elot-db-search-entities "%Margherita%"))
           (vias (mapcar (lambda (r) (nth 6 r)) rows)))
      (should (= 1 (length rows)))
      ;; A `%' in the query suppresses the fallback entirely.
      (should (member (car vias) '("exact" "label-substring"))))))

(ert-deftest test-elot-db-search-entities-exact-via ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; Exact id match -> `via' = `exact'.
    (let* ((rows (elot-db-search-entities "trn:vehicle"))
           (row  (car rows)))
      (should (equal "trn:vehicle" (nth 0 row)))
      (should (equal "exact"       (nth 6 row))))))

(ert-deftest test-elot-db-search-entities-lang-filter ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    ;; Only `trn:vehicle' (en) and `anm:snake' (nb) carry tagged labels.
    (let* ((rows-en (elot-db-search-entities "%" 50 nil nil "en"))
           (rows-nb (elot-db-search-entities "%" 50 nil nil "nb")))
      (should (= 1 (length rows-en)))
      (should (equal "trn:vehicle" (caar rows-en)))
      (should (= 1 (length rows-nb)))
      (should (equal "anm:snake" (caar rows-nb))))))

(ert-deftest test-elot-db-search-entities-limit ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((rows (elot-db-search-entities "%" 2)))
      (should (= 2 (length rows))))))

(ert-deftest test-elot-db-search-entities-no-limit ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((rows (elot-db-search-entities "%" 0)))
      ;; 4 transport + 3 animals + 2 pizza-mini = 9 entities total.
      (should (= 9 (length rows))))))

(ert-deftest test-elot-db-search-entities-empty-query ()
  (elot-gptel-db-search-test--with-fresh-db
    (should-error (elot-db-search-entities "")
                  :type 'user-error)
    (should-error (elot-db-search-entities nil)
                  :type 'user-error)))

(ert-deftest test-elot-db-search-entities-unknown-kind ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (should-error (elot-db-search-entities "%" 50 "widget")
                  :type 'user-error)))

;;;; Integration: elot-gptel-tool-db-search-label ----------------------

(ert-deftest test-elot-gptel-tool-db-search-label-tsv-header ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label "Vehicle")))
      (should (stringp out))
      (should (string-match-p
               "^id\tlabel\tkind\tontology_iri\tsource\tdata_source\tvia\n"
               out))
      (should (string-match-p
               "^trn:vehicle\tVehicle\towl:Class\ttransport-ont\ttransport\t"
               (substring out (1+ (string-match-p "\n" out))))))))

(ert-deftest test-elot-gptel-tool-db-search-label-fallback ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label "ex:Margherita")))
      ;; Primary pass returns nothing (no `ex:Margherita' in DB), but
      ;; the M11.1 local-name fallback finds `:Margherita' under the
      ;; default prefix and surfaces it with via=local-name.
      (should (string-match-p ":Margherita" out))
      (should (string-match-p "local-name" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-fallback-disabled ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label
                "ex:Margherita" nil nil nil nil t)))
      (should (equal "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-kind-enum ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label "%" "individual")))
      ;; Only Medusa is asserted as a NamedIndividual.
      (should (string-match-p "anm:medusa\tMedusa" out))
      (should-not (string-match-p "trn:vehicle" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-no-rows ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label "no-such-thing")))
      (should (equal "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-empty-query ()
  (elot-gptel-db-search-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-search-label "")))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-bad-limit ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label
                "Vehicle" nil nil nil -1)))
      (should (string-prefix-p "ERROR:" out)))))

(ert-deftest test-elot-gptel-tool-db-search-label-source-filter ()
  (elot-gptel-db-search-test--with-fresh-db
    (elot-gptel-db-search-test--seed)
    (let ((out (elot-gptel-tool-db-search-label "%" nil "animals")))
      (should (string-match-p "anm:snake" out))
      (should-not (string-match-p "trn:vehicle" out)))))

(provide 'elot-gptel-db-search-test)
;;; elot-gptel-db-search-test.el ends here
