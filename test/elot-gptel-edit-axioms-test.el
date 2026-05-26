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
;;; Empty-row fixtures + tests for the recent work:
;;;   * regex tolerates `- KEY ::' with no trailing value (M9.2.c.1
;;;     follow-up; pets.org "chicken-and-egg" hot-fix);
;;;   * `match_fragment: ""' sentinel targets empty-value rows
;;;     (distinct from "matcher omitted");
;;;   * new op `delete-empty' (singleton + batch) sweeps empty rows;
;;;   * Option A blank-line trim collapses blanks adjacent to a cut.
;;; The fixture below uses a fresh subject `ex:squirrel' carrying four
;;; empty rows plus two populated ones, sitting under a heading with
;;; two pre-existing blank lines below it so the trim test has
;;; something visible to remove.  Lives alongside the original
;;; fixture; the existing tests keep using `--write' / `--fixture'.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-edit-axioms-test--fixture-empties ()
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
   "*** Squirrel (ex:squirrel)\n"
   ;; Two blank lines between the Squirrel heading and its
   ;; first description-list row -- Option A trim should
   ;; eat these once the topmost empty row above them is
   ;; swept.
   "\n"
   "\n"
   " - rdfs:comment ::\n"
   " - skos:example ::   \n"  ; whitespace-only value
   " - skos:example :: keepme\n"
   " - rdfs:seeAlso ::\n"
   "*** Cat (ex:cat)\n"
   " - rdfs:comment :: alpha\n"
   "\n"
   "\n"
   " - rdfs:comment :: beta\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
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
   "*** rdfs:seeAlso\n"
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

(defun elot-gptel-edit-axioms-test--write-empties ()
  (let ((path (expand-file-name
               (format "elot-edit-axioms-empties-%d.org" (random 1000000))
               elot-gptel-edit-axioms-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-edit-axioms-test--fixture-empties)))
    path))

(defmacro elot-gptel-edit-axioms-test--with-empties (path-var &rest body)
  "Like `--with-fixture' but with the empties fixture and a clean
lint stub (no chicken-and-egg broken rows in this file)."
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-edit-axioms-test--write-empties)))
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


;;; --- Empty-row regex tolerance + `match_fragment: ""' sentinel ----

(ert-deftest elot-gptel-edit-axioms-test-empty-row-visible-to-scanner ()
  "Rows ending in `::' with no trailing value must be findable.
Regression: the original `elot-gptel--axiom-toplevel-row-re'
required a space after `::', so empty rows were invisible to
the row scanner and even `replace' with a matching keyword
failed with `no row matches'."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      ;; `rdfs:seeAlso' is unique on ex:squirrel and empty.  No
      ;; match_fragment supplied -- unique keyword must resolve.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "replace"
                              :keyword "rdfs:seeAlso"
                              :fragment "http://example.org/see")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        (should (string-match-p
                 " - rdfs:seeAlso :: http://example.org/see"
                 bytes))))))

(ert-deftest elot-gptel-edit-axioms-test-empty-fragment-targets-empty-row ()
  "`match_fragment: \"\"' must distinguish an empty row from a
populated sibling under the same keyword."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      ;; Two `skos:example' rows on ex:squirrel: one empty (whitespace),
      ;; one carrying "keepme".  Replace the empty one only.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "replace"
                              :keyword "skos:example"
                              :match_fragment ""
                              :fragment "filled")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        (should (string-match-p " - skos:example :: filled" bytes))
        (should (string-match-p " - skos:example :: keepme" bytes))))))

(ert-deftest elot-gptel-edit-axioms-test-empty-fragment-ambiguous-among-empties ()
  "`match_fragment: \"\"' still triggers the ambiguity guard when
multiple empty rows share the keyword.  Documents the limit
the LLM should fall back to `delete-empty' + re-add for."
  (let* ((path (elot-gptel-edit-axioms-test--write-empties))
         ;; Inject a second empty `rdfs:seeAlso' row directly into
         ;; the file -- we cannot do this via `add' (the normaliser
         ;; rejects whitespace-only fragments).
         (_ (with-temp-buffer
              (insert-file-contents path)
              (goto-char (point-min))
              (re-search-forward "^ - rdfs:seeAlso ::$")
              (forward-line 1)
              (insert " - rdfs:seeAlso ::\n")
              (write-region (point-min) (point-max) path nil 'silent))))
    (unwind-protect
        (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                   (lambda (&rest _) "OK: no lint issues"))
                  ((symbol-function 'elot-gptel-tool-omn-validate)
                   (lambda (&rest _) "OK: 1 ontology parsed"))
                  ((symbol-function 'elot-robot-available-p)
                   (lambda (&rest _) nil)))
          (let ((elot-gptel-allow-side-effects t)
                (default-directory elot-gptel-edit-axioms-test--repo-root)
                (before (elot-gptel-edit-axioms-test--read path)))
            (let ((out (elot-gptel-tool-edit-axioms
                        (elot-gptel-edit-axioms-test--rel path)
                        (list (list :subject "ex:squirrel"
                                    :op "replace"
                                    :keyword "rdfs:seeAlso"
                                    :match_fragment ""
                                    :fragment "http://example.org/x")))))
              (should (string-prefix-p "FAIL at edits[0]:" out))
              (should (string-match-p "ambiguous" out)))
            ;; Atomic abort -- file untouched.
            (should (equal before
                           (elot-gptel-edit-axioms-test--read path)))))
      (when (file-exists-p path) (delete-file path))
      (dolist (b (buffer-list))
        (when (and (buffer-file-name b)
                   (string= (buffer-file-name b) path))
          (with-current-buffer b (set-buffer-modified-p nil))
          (kill-buffer b))))))


;;; --- New op: `delete-empty' (batch) -------------------------------

(ert-deftest elot-gptel-edit-axioms-test-delete-empty-keyword-restricted ()
  "`delete-empty' with a keyword filter removes only that
keyword's empty rows; populated rows with the same keyword
survive."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty"
                              :keyword "skos:example")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out))
        (should (string-match-p
                 "swept 1 empty row.* on ex:squirrel: skos:example"
                 out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        ;; Populated row survives.
        (should (string-match-p " - skos:example :: keepme" bytes))
        ;; Empty row is gone; the other empty rows (different
        ;; keywords) are untouched.
        (should (string-match-p " - rdfs:comment ::\n" bytes))
        (should (string-match-p " - rdfs:seeAlso ::\n" bytes))))))

(ert-deftest elot-gptel-edit-axioms-test-delete-empty-sweep-all ()
  "`delete-empty' with no keyword sweeps every empty row on the
subject regardless of keyword.  The populated row survives."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out))
        ;; 3 empties: rdfs:comment, skos:example (whitespace),
        ;; rdfs:seeAlso.
        (should (string-match-p
                 "swept 3 empty rows on ex:squirrel:" out))
        (should (string-match-p "rdfs:comment" out))
        (should (string-match-p "skos:example" out))
        (should (string-match-p "rdfs:seeAlso" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        (should (string-match-p " - skos:example :: keepme" bytes))
        (should-not (string-match-p " - rdfs:comment ::\n" bytes))
        (should-not (string-match-p " - rdfs:seeAlso ::\n" bytes))))))

(ert-deftest elot-gptel-edit-axioms-test-delete-empty-zero-match-success ()
  "Zero matches is the idempotent no-op success case.  The
file must remain byte-identical and the batch envelope OK."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      ;; First sweep -- removes the 3 empties.  Second sweep on
      ;; the same subject should now find nothing.
      (let ((_ (elot-gptel-tool-edit-axioms
                (elot-gptel-edit-axioms-test--rel path)
                (list (list :subject "ex:squirrel"
                            :op "delete-empty"))))
            (before (elot-gptel-edit-axioms-test--read path))
            (out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out))
        (should (string-match-p
                 "no empty rows on ex:squirrel" out))
        (should (equal before
                       (elot-gptel-edit-axioms-test--read path)))))))

(ert-deftest elot-gptel-edit-axioms-test-delete-empty-mixed-with-add ()
  "`delete-empty' composes inside a batch: sweep, then add fresh
populated rows.  Sequencing must run sweep against the running
draft and the subsequent adds must see the post-sweep state."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty")
                        (list :subject "ex:squirrel"
                              :op "add"
                              :keyword "rdfs:comment"
                              :fragment "\"A small rodent.\"@en")
                        (list :subject "ex:squirrel"
                              :op "add"
                              :keyword "skos:example"
                              :fragment "red squirrel")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 3 edits" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        ;; Survivors / additions present:
        (should (string-match-p
                 " - rdfs:comment :: \"A small rodent.\"@en" bytes))
        (should (string-match-p " - skos:example :: keepme" bytes))
        (should (string-match-p
                 " - skos:example :: red squirrel" bytes))
        ;; No empty rows remain on ex:squirrel.
        (should-not (string-match-p " - rdfs:seeAlso ::\n" bytes))))))


;;; --- Option A blank-line trim on delete ---------------------------

(ert-deftest elot-gptel-edit-axioms-test-blank-line-trim-between-rows ()
  "Deleting a row sandwiched between two populated rows that have
blank lines around it collapses the blanks down to zero, so
the surviving rows sit contiguously (Org description lists
break on blank lines)."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      ;; ex:cat has:
      ;;   - rdfs:comment :: alpha
      ;;   <blank>
      ;;   <blank>
      ;;   - rdfs:comment :: beta
      ;; Delete the `beta' one; the two blank lines that surface
      ;; against the cut must collapse.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:cat"
                              :op "delete"
                              :keyword "rdfs:comment"
                              :match_fragment "beta")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        ;; The `alpha' row must be immediately followed by the next
        ;; heading -- no blank lines in between.
        (should (string-match-p
                 " - rdfs:comment :: alpha\n\\*\\* Object properties"
                 bytes))
        (should-not (string-match-p "rdfs:comment :: beta" bytes))))))

(ert-deftest elot-gptel-edit-axioms-test-blank-line-trim-after-sweep ()
  "After a `delete-empty' sweep removes every row directly under
a heading whose body began with blank lines, those pre-existing
blanks collapse too -- the topmost cut in the bottom-up sweep
sits next to them.  Mirrors the pets.org Squirrel session.

This test runs the sweep on ex:squirrel (4 empty rows with
2 blank lines above the heading's first row) and then asserts
the heading is followed directly by its surviving content
(`skos:example :: keepme'), with no blank-line slack between
heading and row."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty")))))
        (should (stringp out))
        (should (string-prefix-p "OK: applied 1 edit\n" out)))
      (let ((bytes (elot-gptel-edit-axioms-test--read path)))
        ;; Heading directly followed by the surviving populated row.
        (should (string-match-p
                 "\\*\\*\\* Squirrel (ex:squirrel)\n - skos:example :: keepme"
                 bytes))))))


;;; --- Normaliser: `delete-empty' is recognised by the batch
;;;     edit-shape validator (vs. unknown-op rejection).
;;; ---------------------------------------------------------------

(ert-deftest elot-gptel-edit-axioms-test-delete-empty-normaliser ()
  "`delete-empty' edits pass the per-edit normaliser without
requiring `keyword' or `fragment'."
  (elot-gptel-edit-axioms-test--with-empties path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-edit-axioms-test--repo-root))
      ;; Edit missing keyword AND fragment -- legal for delete-empty.
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "delete-empty")))))
        (should (stringp out))
        (should (string-prefix-p "OK:" out)))
      ;; And an unknown op still gets rejected (negative control).
      (let ((out (elot-gptel-tool-edit-axioms
                  (elot-gptel-edit-axioms-test--rel path)
                  (list (list :subject "ex:squirrel"
                              :op "vandalise"
                              :keyword "rdfs:comment"
                              :fragment "x")))))
        (should (string-prefix-p "FAIL" out))
        (should (string-match-p "delete-empty" out))))))


;;; --- Tool registration --------------------------------------------

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
