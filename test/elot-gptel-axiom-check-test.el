;;; elot-gptel-axiom-check-test.el --- Tests for elot_axiom_check  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-axiom-check-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.b -- tests for the
;; read-only `elot_axiom_check' pre-flight validator.  Cases that
;; can be exercised without ROBOT run unconditionally; ROBOT-
;; dependent stages (OMN parse, consistency) are skipped when
;; `elot-robot-available-p' returns nil.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-axiom-check-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-axiom-check-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-robot nil 'noerror)

;;; ---------------------------------------------------------------------------
;;; Fixture (minimal pets-like ontology, ROBOT-free).
;;; ---------------------------------------------------------------------------

(defun elot-gptel-axiom-check-test--fixture ()
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
   "**** Dog (ex:dog)\n"
   "**** Cat (ex:cat)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   "*** isAfraidOf (ex:isAfraidOf)\n"
   "** Data properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-data-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
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
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"))

(defun elot-gptel-axiom-check-test--write ()
  (let ((path (expand-file-name
               (format "elot-axcheck-fixture-%d.org" (random 1000000))
               elot-gptel-axiom-check-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-axiom-check-test--fixture)))
    path))

(defmacro elot-gptel-axiom-check-test--with-fixture (path-var &rest body)
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-axiom-check-test--write)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-axiom-check-test--rel (path)
  (file-relative-name
   path elot-gptel-axiom-check-test--repo-root))

(defun elot-gptel-axiom-check-test--robot-p ()
  (and (fboundp 'elot-robot-available-p)
       (ignore-errors (elot-robot-available-p))))


;;; ---------------------------------------------------------------------------
;;; ERROR cases (input shape; no ROBOT needed)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-check-test-unknown-subject ()
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:nonexistent" "SubClassOf" "ex:animal")))
      (should (string-prefix-p "ERROR:" out))
      (should (string-match-p "not found" out)))))

(ert-deftest elot-gptel-axiom-check-test-empty-fragment ()
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "SubClassOf" "  ")))
      (should (string-prefix-p "ERROR:" out)))))


;;; ---------------------------------------------------------------------------
;;; Keyword-legality FAIL (no ROBOT)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-check-test-keyword-illegal-on-class ()
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "Domain" "ex:animal")))
      (should (string-prefix-p "FAIL:" out))
      (should (string-match-p "not legal" out))
      (should (string-match-p "Class" out))
      (should (string-match-p "SubClassOf" out)))))

(ert-deftest elot-gptel-axiom-check-test-annotation-property-legal-on-class ()
  ;; rdfs:comment is a universal AP -- should not fail at stage 1.
  ;; Stage 4 (ROBOT) may or may not run.  We just assert the call did
  ;; not refuse at the keyword-legality stage.
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "rdfs:comment" "\"A pet.\"@en")))
      (should-not (string-match-p "not legal" out)))))


;;; ---------------------------------------------------------------------------
;;; Static-checker FAIL: undeclared / wrong-kind CURIEs (no ROBOT)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-check-test-undeclared-curie ()
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "SubClassOf" "ex:wolf")))
      (should (string-prefix-p "FAIL:" out))
      (should (string-match-p "static" out))
      (should (string-match-p "ex:wolf" out)))))

(ert-deftest elot-gptel-axiom-check-test-kind-range-mismatch ()
  ;; ex:isAfraidOf is declared but is an ObjectProperty; SubClassOf
  ;; expects a Class -- elot/axiom-keyword-range should flag this.
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "SubClassOf" "ex:isAfraidOf")))
      (should (string-prefix-p "FAIL:" out))
      (should (string-match-p "ex:isAfraidOf" out))
      (should (string-match-p "ObjectProperty" out)))))


;;; ---------------------------------------------------------------------------
;;; OK path (ROBOT-gated): legal keyword + declared leaf
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-check-test-ok-bare-curie ()
  ;; ROBOT no longer involved in 9.2.b's default path; the OK case
  ;; runs unconditionally.
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "SubClassOf" "ex:animal")))
      (should (string-prefix-p "OK:" out))
      (should (string-match-p "ex:dog SubClassOf :: ex:animal" out)))))

(ert-deftest elot-gptel-axiom-check-test-ok-class-expression ()
  ;; Conservative lint tokeniser must not false-positive on
  ;; legitimate restrictions.  ROBOT no longer involved by default.
  (elot-gptel-axiom-check-test--with-fixture path
    (let* ((default-directory elot-gptel-axiom-check-test--repo-root)
           (out (elot-gptel-tool-axiom-check
                 (elot-gptel-axiom-check-test--rel path)
                 "ex:dog" "SubClassOf" "ex:chases some ex:cat")))
      (should (string-prefix-p "OK:" out)))))


;;; ---------------------------------------------------------------------------
;;; Tool registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-axiom-check-test-tool-spec-registered ()
  (let ((spec (assoc "elot_axiom_check" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-axiom-check))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 5 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "subject"))
      (should (equal (plist-get (nth 2 args) :name) "keyword"))
      (should (equal (plist-get (nth 3 args) :name) "fragment"))
      (should (equal (plist-get (nth 4 args) :name) "consistency")))
    ;; Read-only tool -- no :confirm.
    (should (null (plist-get (cdr spec) :confirm)))))

(ert-deftest elot-gptel-axiom-check-test-dispatcher-thunk ()
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-axiom-check)))
    (should (functionp thunk))))

(provide 'elot-gptel-axiom-check-test)
;;; elot-gptel-axiom-check-test.el ends here
