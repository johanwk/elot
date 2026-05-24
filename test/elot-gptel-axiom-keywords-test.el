;;; elot-gptel-axiom-keywords-test.el --- Tests for elot_axiom_keywords  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-axiom-keywords-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.a -- tests for the
;; read-only `elot_axiom_keywords' authoring helper.  Pure Elisp;
;; no ROBOT, no DB.  Exercises subject resolution (CURIE +
;; label), per-kind frame-keyword tables, the universal
;; annotation-properties block, the per-kind buffer signature,
;; and the existing-rows render.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-axiom-keywords-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-axiom-keywords-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology covering every kind we need to test.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-axiom-keywords-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Prefixes\n"
   ":PROPERTIES:\n:prefixdefs: yes\n:END:\n"
   "#+name: prefix-table\n"
   "| prefix | uri                                   |\n"
   "|--------+---------------------------------------|\n"
   "| owl:   | http://www.w3.org/2002/07/owl#        |\n"
   "| rdfs:  | http://www.w3.org/2000/01/rdf-schema# |\n"
   "| skos:  | http://www.w3.org/2004/02/skos/core#  |\n"
   "| ex:    | http://example.org/                   |\n"
   "** my-ont ontology (ex:my-ont)\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-ontology-declaration\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   " - rdfs:comment :: A living organism\n"
   " - skos:example :: mouse, elephant\n"
   "**** Dog (ex:dog)\n"
   "**** Cat (ex:cat)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   " - Range :: ex:cat\n"
   " - Characteristics :: Asymmetric\n"
   "** Data properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-data-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** ageInYears (ex:ageInYears)\n"
   "** Annotation properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-annotation-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** rdfs:comment\n"
   "*** skos:example\n"
   "** Datatypes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-datatypes\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** myType (ex:myType)\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"
   " - Types :: ex:dog\n"))

(defun elot-gptel-axiom-keywords-test--write ()
  (let ((path (expand-file-name
               (format "elot-axiom-kw-fixture-%d.org" (random 1000000))
               elot-gptel-axiom-keywords-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-axiom-keywords-test--fixture)))
    path))

(defmacro elot-gptel-axiom-keywords-test--with-fixture (path-var &rest body)
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-axiom-keywords-test--write)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-axiom-keywords-test--rel (path)
  (file-relative-name
   path elot-gptel-axiom-keywords-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Subject resolution
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-keywords-test-resolve-by-curie ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:animal")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "subject ex:animal" out))
      (should (string-match-p "kind Class" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-resolve-by-label ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "chases")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "subject ex:chases" out))
      (should (string-match-p "kind ObjectProperty" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-unknown-subject ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:nope")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "not found" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-empty-subject ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "non-empty" out)))))


;;; ---------------------------------------------------------------------------
;;; Frame-keyword tables, per kind
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-keywords-test-class-keywords ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:dog")))
      (should (string-match-p "== Frame keywords ==" out))
      (should (string-match-p "SubClassOf :: <ClassExpression>" out))
      (should (string-match-p "EquivalentTo :: <ClassExpression>" out))
      (should (string-match-p "DisjointWith :: " out))
      (should (string-match-p "DisjointUnionOf :: " out))
      (should (string-match-p "HasKey :: " out))
      ;; Property-only keywords must NOT appear.
      (should-not (string-match-p "SubPropertyChain" out))
      (should-not (string-match-p "InverseOf" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-objectproperty-keywords ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:chases")))
      (should (string-match-p "Domain :: " out))
      (should (string-match-p "Range :: " out))
      (should (string-match-p "InverseOf :: " out))
      (should (string-match-p "Characteristics :: " out))
      (should (string-match-p "Asymmetric" out))
      (should (string-match-p "SubPropertyChain :: " out))
      ;; Class-only keywords must NOT appear.
      (should-not (string-match-p "SubClassOf" out))
      (should-not (string-match-p "DisjointUnionOf" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-dataproperty-keywords ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path)
                 "ex:ageInYears")))
      (should (string-match-p "kind DataProperty" out))
      (should (string-match-p "Range :: <DataRange>" out))
      ;; DataProperty Characteristics is Functional-only.
      (should (string-match-p "Characteristics :: Functional" out))
      (should-not (string-match-p "Asymmetric" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-individual-keywords ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:scooby")))
      (should (string-match-p "kind Individual" out))
      (should (string-match-p "Types :: <ClassExpression>" out))
      (should (string-match-p "Facts :: " out))
      (should (string-match-p "SameAs :: " out))
      (should (string-match-p "DifferentFrom :: " out))
      (should-not (string-match-p "SubClassOf" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-datatype-keywords ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:myType")))
      (should (string-match-p "kind Datatype" out))
      (should (string-match-p "EquivalentTo :: <DataRange>" out)))))


;;; ---------------------------------------------------------------------------
;;; Universal annotation rows + buffer signature + existing rows
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-keywords-test-universal-aps ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:dog")))
      (should (string-match-p "== Universal annotation rows ==" out))
      (should (string-match-p "rdfs:comment" out))
      (should (string-match-p "skos:example" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-signature ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:dog")))
      (should (string-match-p "== Buffer signature ==" out))
      ;; Per-kind buckets all present with their members.
      (should (string-match-p "Class (" out))
      (should (string-match-p "ex:animal" out))
      (should (string-match-p "ex:dog" out))
      (should (string-match-p "ObjectProperty (" out))
      (should (string-match-p "ex:chases" out))
      (should (string-match-p "DataProperty (" out))
      (should (string-match-p "ex:ageInYears" out))
      (should (string-match-p "Individual (" out))
      (should (string-match-p "ex:scooby" out))
      (should (string-match-p "Datatype (" out))
      (should (string-match-p "ex:myType" out)))))

(ert-deftest elot-gptel-axiom-keywords-test-existing-rows ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path)
                 "ex:animal")))
      (should (string-match-p "== Existing rows on ex:animal ==" out))
      (should (string-match-p "rdfs:comment :: A living organism" out))
      (should (string-match-p "skos:example :: mouse, elephant" out))
      ;; Housekeeping rows must be filtered out.
      (should-not (string-match-p "rdfs:label :: " out))
      (should-not (string-match-p "rdf:type :: " out)))))

(ert-deftest elot-gptel-axiom-keywords-test-existing-rows-empty ()
  (elot-gptel-axiom-keywords-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-keywords-test--repo-root)
           (out (elot-gptel-tool-axiom-keywords
                 (elot-gptel-axiom-keywords-test--rel path) "ex:dog")))
      (should (string-match-p
               "== Existing rows on ex:dog ==" out))
      (should (string-match-p "no description-list rows" out)))))


;;; ---------------------------------------------------------------------------
;;; Tool registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-keywords-test-tool-spec-registered ()
  (let ((spec (assoc "elot_axiom_keywords" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-axiom-keywords))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 2 (length args)))
      (should (equal (plist-get (car args) :name) "file"))
      (should (equal (plist-get (cadr args) :name) "subject")))
    ;; No :confirm -- this is a read-only tool.
    (should (null (plist-get (cdr spec) :confirm)))))

(provide 'elot-gptel-axiom-keywords-test)
;;; elot-gptel-axiom-keywords-test.el ends here
