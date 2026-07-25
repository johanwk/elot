;;; elot-gptel-db-shims-wave-b-test.el --- Tests for M6.4 Wave B shims  -*- lexical-binding: t; -*-

;; Usage:  make -C test db-shims-wave-b-test  (or)
;;         cd test && emacs --batch -l elot-gptel-db-shims-wave-b-test.el \
;;              -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 6 Step 6.4 (Wave B): tests for the
;; three attribute-driven SQL-select shims:
;;
;;   `elot-gptel-tool-db-get-attributes'
;;   `elot-gptel-tool-db-supertypes'
;;   `elot-gptel-tool-db-individual-types'
;;
;; The fixture seeds two distinct sources to exercise the
;; OMN-frame-keyword / RDF-property dual-spelling support called out
;; in the plan: one TTL-style source uses `rdf:type' /
;; `rdfs:subClassOf', the other an OMN-style source uses `Types' /
;; `SubClassOf'.  Pure-Elisp; no ROBOT required.

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

;;;; Harness -------------------------------------------------------------

(defvar elot-gptel-db-shims-wb-test--tmpfile nil)

(defun elot-gptel-db-shims-wb-test--fresh-db ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-shims-wb-test--tmpfile
             (file-exists-p elot-gptel-db-shims-wb-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-shims-wb-test--tmpfile)))
  (setq elot-gptel-db-shims-wb-test--tmpfile
        (make-temp-file "elot-gptel-db-shims-wb-test-" nil ".sqlite"))
  (ignore-errors (delete-file elot-gptel-db-shims-wb-test--tmpfile))
  (elot-db-init elot-gptel-db-shims-wb-test--tmpfile))

(defun elot-gptel-db-shims-wb-test--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-gptel-db-shims-wb-test--tmpfile
             (file-exists-p elot-gptel-db-shims-wb-test--tmpfile))
    (ignore-errors (delete-file elot-gptel-db-shims-wb-test--tmpfile)))
  (setq elot-gptel-db-shims-wb-test--tmpfile nil))

(defmacro elot-gptel-db-shims-wb-test--with-fresh-db (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-gptel-db-shims-wb-test--fresh-db) ,@body)
     (elot-gptel-db-shims-wb-test--teardown)))

(defun elot-gptel-db-shims-wb-test--seed ()
  "Seed two sources with complementary RDF and OMN spellings.

Source `pets-ttl' uses RDF spellings (`rdf:type',
`rdfs:subClassOf') as a TTL ingest would.  Source `pets-omn'
uses OMN spellings (`Types', `SubClassOf') as an ELOT `.org'
tangle ingest would.  Both define `ex:dog' as a subclass of
`ex:animal' and assert `ex:spooky' as an individual of
`ex:dog' (plus, in the TTL source, the `owl:NamedIndividual'
marker)."
  ;; TTL-style source: RDF property CURIEs in attributes.prop
  (elot-db-update-source
   "pets-ttl" nil "ttl"
   '(("ex:animal" "Animal"
      ("rdf:type" "owl:Class"))
     ("ex:dog"    "Dog"
      ("rdf:type" "owl:Class"
       "rdfs:subClassOf" "ex:animal"))
     ("ex:cat"    "Cat"
      ("rdf:type" "owl:Class"
       "rdfs:subClassOf" "ex:animal"))
     ("ex:spooky" "spooky"
      ("rdf:type" "owl:NamedIndividual"
       "rdf:type" "ex:dog"))))
  ;; OMN-style source: frame keywords in attributes.prop
  (elot-db-update-source
   "pets-omn" nil "org"
   '(("ex:animal" "Animal"
      ("rdf:type" "owl:Class"))
     ("ex:dog"    "Dog"
      ("rdf:type" "owl:Class"
       "SubClassOf" "ex:animal"))
     ("ex:wolf"   "Wolf"
      ("rdf:type" "owl:Class"
       "SubClassOf" "ex:animal"))
     ("ex:fido"   "Fido"
      ("rdf:type" "owl:NamedIndividual"
       "Types" "ex:dog")))))

;;;; ---- elot_db_get_attributes ---------------------------------------

(ert-deftest test-elot-gptel-tool-db-get-attributes-all-sources ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-get-attributes "ex:dog")))
      (should (stringp out))
      ;; Header line.
      (should (string-match-p
               "^prop\tvalue\tlang\tsource\tdata_source" out))
      ;; Rows from both sources are present.
      (should (string-match-p "\tpets-ttl\t" out))
      (should (string-match-p "\tpets-omn\t" out))
      ;; The TTL-side rdfs:subClassOf row landed.
      (should (string-match-p "rdfs:subClassOf\tex:animal" out))
      ;; The OMN-side SubClassOf row landed.
      (should (string-match-p "SubClassOf\tex:animal" out)))))

(ert-deftest test-elot-gptel-tool-db-get-attributes-source-scope ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-get-attributes "ex:dog" "pets-omn")))
      (should (stringp out))
      (should (string-match-p "SubClassOf\tex:animal" out))
      ;; TTL-side spelling must NOT appear when restricted to pets-omn.
      (should-not (string-match-p "rdfs:subClassOf" out))
      (should-not (string-match-p "\tpets-ttl\t" out)))))

(ert-deftest test-elot-gptel-tool-db-get-attributes-unknown-id ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-get-attributes "ex:nope")))
      (should (string-match-p "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-get-attributes-rejects-empty ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-get-attributes "")))
      (should (string-prefix-p "ERROR:" out)))))

;;;; ---- elot_db_supertypes -------------------------------------------

(ert-deftest test-elot-gptel-tool-db-supertypes-dual-spellings ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-supertypes "ex:dog")))
      (should (string-match-p
               "^value\tprop\tsource\tdata_source" out))
      ;; Both the RDF and OMN spellings should produce hits.
      (should (string-match-p
               "ex:animal\trdfs:subClassOf\tpets-ttl" out))
      (should (string-match-p
               "ex:animal\tSubClassOf\tpets-omn" out)))))

(ert-deftest test-elot-gptel-tool-db-supertypes-source-scope ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-supertypes "ex:dog" "pets-ttl")))
      (should (string-match-p
               "ex:animal\trdfs:subClassOf\tpets-ttl" out))
      (should-not (string-match-p "SubClassOf\tex:animal" out)))))

(ert-deftest test-elot-gptel-tool-db-supertypes-no-parent ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-supertypes "ex:animal")))
      (should (string-match-p "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-supertypes-rejects-empty ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-supertypes "")))
      (should (string-prefix-p "ERROR:" out)))))

;;;; ---- elot_db_individual_types --------------------------------------

(ert-deftest test-elot-gptel-tool-db-individual-types-ttl ()
  "TTL-style source: type via rdf:type + NamedIndividual guard."
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-individual-types "ex:spooky")))
      (should (string-match-p
               "^value\tprop\tsource\tdata_source" out))
      ;; The real type is reported.
      (should (string-match-p "ex:dog\trdf:type\tpets-ttl" out))
      ;; The owl:NamedIndividual marker is excluded.
      (should-not (string-match-p "owl:NamedIndividual" out)))))

(ert-deftest test-elot-gptel-tool-db-individual-types-omn ()
  "OMN-style source: type via Types frame keyword."
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-individual-types "ex:fido")))
      (should (string-match-p "ex:dog\tTypes\tpets-omn" out)))))

(ert-deftest test-elot-gptel-tool-db-individual-types-excludes-classes ()
  "Classes carry rdf:type owl:Class but must yield no rows here:
the EXISTS guard requires either a Types row or an
rdf:type owl:NamedIndividual attestation."
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-individual-types "ex:dog")))
      (should (string-match-p "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-individual-types-unknown ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (elot-gptel-db-shims-wb-test--seed)
    (let ((out (elot-gptel-tool-db-individual-types "ex:nobody")))
      (should (string-match-p "OK: no rows" out)))))

(ert-deftest test-elot-gptel-tool-db-individual-types-rejects-empty ()
  (elot-gptel-db-shims-wb-test--with-fresh-db
    (let ((out (elot-gptel-tool-db-individual-types "")))
      (should (string-prefix-p "ERROR:" out)))))

(provide 'elot-gptel-db-shims-wave-b-test)

;;; elot-gptel-db-shims-wave-b-test.el ends here
