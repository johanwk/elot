;;; elot-gptel-edit-axiom-test.el --- Tests for elot_edit_axiom  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-edit-axiom-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.c -- tests for the
;; mutating `elot_edit_axiom' writer.  Pure-Elisp; the lint /
;; OMN-validate stages are stubbed via `cl-letf' so no ROBOT is
;; required for the happy paths.  Rollback case (#9) supplies a
;; failing lint stub.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-edit-axiom-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-edit-axiom-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology with rows that exercise the matcher.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-edit-axiom-test--fixture ()
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
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "*** Dog (ex:dog)\n"
   " - rdfs:comment :: First comment\n"
   " - rdfs:comment :: Second comment\n"
   "*** Cat (ex:cat)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   "   - rdfs:comment :: Some axiom annotation.\n"
   "   - rdfs:comment :: Another axiom annotation.\n"
   " - Range :: ex:cat\n"
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
   ":END:\n"))

(defun elot-gptel-edit-axiom-test--write ()
  (let ((path (expand-file-name
               (format "elot-edit-axiom-fixture-%d.org" (random 1000000))
               elot-gptel-edit-axiom-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-edit-axiom-test--fixture)))
    path))

(defmacro elot-gptel-edit-axiom-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a fresh fixture file and run BODY with lint stubs."
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-edit-axiom-test--write)))
     (unwind-protect
         (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                    (lambda (&rest _) "OK: no lint issues"))
                   ((symbol-function 'elot-gptel-tool-omn-validate)
                    (lambda (&rest _) "OK: 1 ontology parsed"))
                   ((symbol-function 'elot-robot-available-p)
                    (lambda (&rest _) nil)))
           ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-edit-axiom-test--rel (path)
  (file-relative-name path elot-gptel-edit-axiom-test--repo-root))

(defun elot-gptel-edit-axiom-test--read (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))


;;; ---------------------------------------------------------------------------
;;; ERROR cases (no side effects needed for the gate test, but we
;;; gate explicitly to match real usage)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-side-effects-disabled ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:dog" "rdfs:comment" "\"x\"@en")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))

(ert-deftest elot-gptel-edit-axiom-test-unknown-subject ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:ghost" "rdfs:comment" "\"x\"@en")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "not found" out))))))


;;; ---------------------------------------------------------------------------
;;; OK: add
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-add-ok ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:dog" "rdfs:comment" "\"A pet.\"@en")))
        (should (stringp out))
        (should (string-prefix-p "OK: added 1 row on ex:dog" out)))
      (let ((bytes (elot-gptel-edit-axiom-test--read path)))
        (should (string-match-p
                 " - rdfs:comment :: \"A pet.\"@en"
                 bytes))))))


;;; ---------------------------------------------------------------------------
;;; OK: replace (unique keyword)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-replace-unique ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:chases" "Range" "ex:animal" "replace")))
        (should (stringp out))
        (should (string-prefix-p "OK: replaced 1 row on ex:chases" out)))
      (let ((bytes (elot-gptel-edit-axiom-test--read path)))
        (should (string-match-p " - Range :: ex:animal" bytes))
        (should-not (string-match-p " - Range :: ex:cat" bytes))))))


;;; ---------------------------------------------------------------------------
;;; FAIL (returns ERROR: ambiguous): replace with no fragment matcher,
;;; two rdfs:comment rows
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-replace-ambiguous ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:dog" "rdfs:comment" "\"new\"" "replace")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "ambiguous" out))
        (should (string-match-p "match_fragment" out))))))


;;; ---------------------------------------------------------------------------
;;; OK: replace with match_fragment disambiguating the ambiguity above
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-replace-match-fragment ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:dog" "rdfs:comment"
                  "\"Updated\"@en" "replace" "Second comment")))
        (should (stringp out))
        (should (string-prefix-p "OK: replaced 1 row on ex:dog" out)))
      (let ((bytes (elot-gptel-edit-axiom-test--read path)))
        (should (string-match-p
                 " - rdfs:comment :: \"Updated\"@en"
                 bytes))
        (should (string-match-p
                 " - rdfs:comment :: First comment"
                 bytes))
        (should-not (string-match-p
                     " - rdfs:comment :: Second comment"
                     bytes))))))


;;; ---------------------------------------------------------------------------
;;; OK: delete (unique row)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-delete-unique ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:chases" "Range" nil "delete" "ex:cat")))
        (should (stringp out))
        (should (string-prefix-p
                 "OK: deleted 1 row + 0 nested annotations on ex:chases"
                 out)))
      (let ((bytes (elot-gptel-edit-axiom-test--read path)))
        (should-not (string-match-p " - Range :: ex:cat" bytes))))))


;;; ---------------------------------------------------------------------------
;;; OK: delete with nested annotation children (case #7)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-delete-with-nested ()
  (elot-gptel-edit-axiom-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axiom-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axiom
                  (elot-gptel-edit-axiom-test--rel path)
                  "ex:chases" "Domain" nil "delete" "ex:dog")))
        (should (stringp out))
        (should (string-prefix-p
                 "OK: deleted 1 row + 2 nested annotations on ex:chases"
                 out)))
      (let ((bytes (elot-gptel-edit-axiom-test--read path)))
        (should-not (string-match-p " - Domain :: ex:dog" bytes))
        (should-not (string-match-p "Some axiom annotation" bytes))
        (should-not (string-match-p "Another axiom annotation" bytes))
        ;; Range row untouched.
        (should (string-match-p " - Range :: ex:cat" bytes))))))


;;; ---------------------------------------------------------------------------
;;; FAIL: revalidation failure -> rollback
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-rollback-on-lint-fail ()
  (let ((path (elot-gptel-edit-axiom-test--write)))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                   (lambda (&rest _)
                     "1:1 [elot/synthetic/high] ERROR: synthetic failure\nSummary: 1 errors, 0 warnings"))
                  ((symbol-function 'elot-gptel-tool-omn-validate)
                   (lambda (&rest _) "OK: 1 ontology parsed"))
                  ((symbol-function 'elot-robot-available-p)
                   (lambda (&rest _) nil)))
          (let ((elot-gptel-allow-side-effects t)
                (default-directory elot-gptel-edit-axiom-test--repo-root)
                (before (elot-gptel-edit-axiom-test--read
                         (expand-file-name path))))
            (let ((out (elot-gptel-tool-edit-axiom
                        (elot-gptel-edit-axiom-test--rel path)
                        "ex:dog" "rdfs:comment" "\"x\"@en")))
              (should (string-prefix-p "FAIL:" out))
              (should (string-match-p "rolled back" out)))
            ;; Bytes on disk unchanged.
            (should (equal before
                           (elot-gptel-edit-axiom-test--read
                            (expand-file-name path))))))
      (when (file-exists-p path) (delete-file path))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b)
                   (string= (buffer-file-name b) path))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))))


;;; ---------------------------------------------------------------------------
;;; Tool registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axiom-test-tool-spec-registered ()
  (let ((spec (assoc "elot_edit_axiom" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-edit-axiom))
    (should (plist-get (cdr spec) :confirm))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 6 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "subject"))
      (should (equal (plist-get (nth 2 args) :name) "keyword"))
      (should (equal (plist-get (nth 3 args) :name) "fragment"))
      (should (equal (plist-get (nth 4 args) :name) "op"))
      (should (equal (plist-get (nth 5 args) :name) "match_fragment")))))

(ert-deftest elot-gptel-edit-axiom-test-dispatcher-thunk ()
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-edit-axiom)))
    (should (functionp thunk))))

(provide 'elot-gptel-edit-axiom-test)
;;; elot-gptel-edit-axiom-test.el ends here
