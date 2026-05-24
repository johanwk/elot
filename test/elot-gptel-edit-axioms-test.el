;;; elot-gptel-edit-axioms-test.el --- Tests for elot_edit_axioms (batch)  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-edit-axioms-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:
;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.2.c.1 -- tests for the
;; batch `elot_edit_axioms' writer.  Pure-Elisp; lint /
;; OMN-validate stages are stubbed via `cl-letf' so no ROBOT is
;; required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-edit-axioms-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-edit-axioms-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology with two pre-existing broken OMN rows,
;;; plus an extra Dog/Cat pair for static-shape tests.  The two
;;; broken rows reproduce the pets.org chicken-and-egg case.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-edit-axioms-test--fixture ()
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
   "*** Elephant (ex:elephant)\n"
   "*** Dog (ex:dog)\n"
   " - SubClassOf :: ex:cat some value ex:chases\n"
   "*** Cat (ex:cat)\n"
   "*** Snake (ex:snake)\n"
   " - EquivalentTo :: 1 ex:elephant\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
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

(defun elot-gptel-edit-axioms-test--write ()
  (let ((path (expand-file-name
               (format "elot-edit-axioms-fixture-%d.org" (random 1000000))
               elot-gptel-edit-axioms-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-edit-axioms-test--fixture)))
    path))

(defmacro elot-gptel-edit-axioms-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a fresh fixture file and run BODY with lint stubs.
Lint stub returns clean iff the bytes on disk no longer contain
either of the two broken OMN rows -- so a partial fix still
fails lint, mirroring the pets.org chicken-and-egg case."
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-edit-axioms-test--write)))
     (unwind-protect
         (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                    (lambda (file &optional _sev _cats content)
                      (let ((bytes
                             (or content
                                 (with-temp-buffer
                                   (insert-file-contents
                                    (elot-gptel--resolve-file file))
                                   (buffer-string)))))
                        (cond
                         ((string-match-p
                           "EquivalentTo :: 1 ex:elephant"
                           bytes)
                          "1:1 [elot/omn-syntax/high] ERROR: Invalid EquivalentTo\nSummary: 1 errors, 0 warnings")
                         ((string-match-p
                           "SubClassOf :: ex:cat some value ex:chases"
                           bytes)
                          "1:1 [elot/omn-syntax/high] ERROR: Invalid SubClassOf\nSummary: 1 errors, 0 warnings")
                         (t
                          "OK: no lint issues")))))
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

(defun elot-gptel-edit-axioms-test--rel (path)
  (file-relative-name path elot-gptel-edit-axioms-test--repo-root))

(defun elot-gptel-edit-axioms-test--read (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))


;;; ---------------------------------------------------------------------------
;;; The motivating case: a two-edit batch repairs both errors.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-pairwise-repair ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:dog"
                              :op "replace"
                              :keyword "SubClassOf"
                              :fragment "ex:chases some ex:cat"
                              :match_fragment "ex:cat some value ex:chases")
                        (list :subject "ex:snake"
                              :op "replace"
                              :keyword "EquivalentTo"
                              :fragment "ex:elephant"
                              :match_fragment "1 ex:elephant")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 2 edits" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        (should (string-match-p
                 " - SubClassOf :: ex:chases some ex:cat" bytes))
        (should (string-match-p
                 " - EquivalentTo :: ex:elephant" bytes))
        (should-not (string-match-p
                     "ex:cat some value ex:chases" bytes))
        (should-not (string-match-p
                     "1 ex:elephant" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Atomicity: an edit failure aborts the whole batch -- no save.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-atomic-on-bad-edit ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root)
          (before (elot-gptel-edit-axioms-test--read path)))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:dog"
                              :op "replace"
                              :keyword "SubClassOf"
                              :fragment "ex:chases some ex:cat"
                              :match_fragment "ex:cat some value ex:chases")
                        ;; This one fails (unknown subject) -> abort.
                        (list :subject "ex:ghost"
                              :keyword "rdfs:comment"
                              :fragment "\"nope\"")))))
        (should (string-prefix-p "FAIL at edits[1]:" out))
        (should (string-match-p "ex:ghost" out)))
      ;; Disk bytes unchanged.
      (should (equal before
                     (elot-gptel-edit-axioms-test--read path))))))


;;; ---------------------------------------------------------------------------
;;; Revalidation rollback: edits succeed in memory, then lint
;;; rejects the post-batch state -> rollback.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-rollback-on-lint-fail ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root)
          (before (elot-gptel-edit-axioms-test--read path)))
      ;; Fix only ONE of the two broken rows -- lint stub still
      ;; flags the other -> revalidate fails -> rollback.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:dog"
                              :op "replace"
                              :keyword "SubClassOf"
                              :fragment "ex:chases some ex:cat"
                              :match_fragment "ex:cat some value ex:chases")))))
        (should (string-prefix-p "FAIL:" out))
        (should (string-match-p "rolled back" out)))
      (should (equal before
                     (elot-gptel-edit-axioms-test--read path))))))


;;; ---------------------------------------------------------------------------
;;; Empty edits -> ERROR.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-empty-edits ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  nil)))
        (should (string-prefix-p "FAIL" out))
        (should (string-match-p "non-empty" out)))
      ;; Vector form, empty:
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  [])))
        (should (string-prefix-p "FAIL" out))))))


;;; ---------------------------------------------------------------------------
;;; Side-effects gate.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-side-effects-disabled ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:dog"
                              :keyword "rdfs:comment"
                              :fragment "\"x\"@en")))))
        (should (string-prefix-p "FAIL" out))
        (should (string-match-p "side effects disabled" out))))))


;;; ---------------------------------------------------------------------------
;;; Dry-run: bypasses gate (when allow-side-effects is nil), runs
;;; the pipeline, leaves disk untouched.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-dry-run-bypasses-gate ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-edit-axioms-test--repo-root)
          (before (elot-gptel-edit-axioms-test--read path)))
      ;; Dry-run with the pairwise repair -- should succeed and
      ;; report `dry-run', and the file must be byte-identical.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:dog"
                              :op "replace"
                              :keyword "SubClassOf"
                              :fragment "ex:chases some ex:cat"
                              :match_fragment "ex:cat some value ex:chases")
                        (list :subject "ex:snake"
                              :op "replace"
                              :keyword "EquivalentTo"
                              :fragment "ex:elephant"
                              :match_fragment "1 ex:elephant"))
                  t)))
        (should (stringp out))
        (should (string-prefix-p "OK: dry-run applied 2 edits" out))
        (should (string-match-p "no file written" out)))
      (should (equal before
                     (elot-gptel-edit-axioms-test--read path))))))


;;; ---------------------------------------------------------------------------
;;; Sequencing: later edits see earlier ones (add then replace
;;; against the just-added row).
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-sequencing ()
  (elot-gptel-edit-axioms-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list
                   ;; First fix the two pre-existing broken rows so
                   ;; lint stays clean post-commit.
                   (list :subject "ex:dog"
                         :op "replace"
                         :keyword "SubClassOf"
                         :fragment "ex:chases some ex:cat"
                         :match_fragment "ex:cat some value ex:chases")
                   (list :subject "ex:snake"
                         :op "replace"
                         :keyword "EquivalentTo"
                         :fragment "ex:elephant"
                         :match_fragment "1 ex:elephant")
                   ;; Then add a comment, and replace it in the
                   ;; same batch -- the replace must see the add.
                   (list :subject "ex:cat"
                         :op "add"
                         :keyword "rdfs:comment"
                         :fragment "\"draft\"@en")
                   (list :subject "ex:cat"
                         :op "replace"
                         :keyword "rdfs:comment"
                         :fragment "\"final\"@en"
                         :match_fragment "\"draft\"@en")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 4 edits" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        (should (string-match-p
                 " - rdfs:comment :: \"final\"@en" bytes))
        (should-not (string-match-p
                     " - rdfs:comment :: \"draft\"@en" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Tool registration.
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-tool-spec-registered ()
  (let ((spec (assoc "elot_edit_axioms" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-edit-axioms))
    (should (plist-get (cdr spec) :confirm))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 3 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "edits"))
      (should (equal (plist-get (nth 2 args) :name) "dry_run")))))

(ert-deftest elot-gptel-edit-axioms-test-dispatcher-thunk ()
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-edit-axioms)))
    (should (functionp thunk))))

(provide 'elot-gptel-edit-axioms-test)
;;; elot-gptel-edit-axioms-test.el ends here
