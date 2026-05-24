;;; elot-gptel-rename-merge-test.el --- Tests for op=merge mode  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-rename-merge-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9 Slice A.3 --
;; tests for `op=merge' on `elot-rename-resource' /
;; `elot_rename_resource'.  Pure-Elisp; lint and OMN-validate
;; stages are stubbed so no ROBOT is required.
;;
;; Covers:
;;   - happy path: TARGET already declared; SOURCE references
;;     rewritten; SOURCE heading removed
;;   - children of SOURCE are promoted one outline level
;;   - op=merge refused when TARGET is undeclared
;;   - op=rename (default) still refuses when TARGET declared
;;   - F1 round-trip: elot_replace_with_parent succeeds
;;     end-to-end through the merge-enabled rename

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-rename-merge-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-rename-merge-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-id-rename)

;;; ---------------------------------------------------------------------------
;;; Fixture: ex:cat is declared; ex:pantherinae is declared as a child
;;; of ex:cat and is referenced from ex:mouse's SubClassOf row.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-rename-merge-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: slug\n"
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
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Cat (ex:cat)\n"
   "**** Pantherinae (ex:pantherinae)\n"
   "*** Mouse (ex:mouse)\n"
   " - SubClassOf :: ex:pantherinae\n"))

(defun elot-gptel-rename-merge-test--fixture-with-children ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: slug\n"
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
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Cat (ex:cat)\n"
   "**** Pantherinae (ex:pantherinae)\n"
   "***** Lion (ex:lion)\n"
   "***** Tiger (ex:tiger)\n"))

(defun elot-gptel-rename-merge-test--write (filename content)
  (let ((path (expand-file-name filename
                                elot-gptel-rename-merge-test--repo-root)))
    (with-temp-file path
      (insert content))
    path))

(defmacro elot-gptel-rename-merge-test--with-fixture
    (path-var content-form &rest body)
  (declare (indent 2))
  `(let ((,path-var (elot-gptel-rename-merge-test--write
                     (format "elot-rename-merge-fixture-%d.org"
                             (random 1000000))
                     ,content-form)))
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

(defun elot-gptel-rename-merge-test--rel (path)
  (file-relative-name path elot-gptel-rename-merge-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Happy path: TARGET declared -> op=merge succeeds
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-merge-test-happy-path ()
  "op=merge rewrites references to TARGET and removes SOURCE's heading."
  (elot-gptel-rename-merge-test--with-fixture path
      (elot-gptel-rename-merge-test--fixture)
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-merge-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-merge-test--rel path)
                  "ex:pantherinae" "ex:cat" nil nil nil "merge")))
        (should (stringp out))
        (should (string-prefix-p "OK: merged" out))
        (should (string-match-p "ex:pantherinae" out))
        (should (string-match-p "ex:cat" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        ;; SOURCE heading removed entirely.
        (should-not (string-match-p "(ex:pantherinae)" bytes))
        ;; Mouse's SubClassOf row now points at ex:cat.
        (should (string-match-p
                 "SubClassOf[ \t]*::[ \t]*ex:cat" bytes))
        ;; TARGET's declaration survives.
        (should (string-match-p "\\*\\*\\* Cat (ex:cat)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Children of SOURCE are promoted one outline level on merge
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-merge-test-children-promoted ()
  "SOURCE's heading-nested children land one level shallower
after merge, becoming children of SOURCE's former outline
parent (which IS TARGET in the F1 use case)."
  (elot-gptel-rename-merge-test--with-fixture path
      (elot-gptel-rename-merge-test--fixture-with-children)
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-merge-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-merge-test--rel path)
                  "ex:pantherinae" "ex:cat" nil nil nil "merge")))
        (should (string-prefix-p "OK: merged" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should-not (string-match-p "(ex:pantherinae)" bytes))
        ;; Lion and Tiger were at level 5 (*****); after promoting
        ;; once they sit at level 4 (****), as children of Cat.
        (should (string-match-p "^\\*\\*\\*\\* Lion (ex:lion)" bytes))
        (should (string-match-p "^\\*\\*\\*\\* Tiger (ex:tiger)" bytes))
        (should-not (string-match-p "^\\*\\*\\*\\*\\* Lion" bytes))))))


;;; ---------------------------------------------------------------------------
;;; op=merge refused when TARGET undeclared
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-merge-test-undeclared-target ()
  "op=merge refuses when TARGET is not already declared."
  (elot-gptel-rename-merge-test--with-fixture path
      (elot-gptel-rename-merge-test--fixture)
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-merge-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-merge-test--rel path)
                  "ex:pantherinae" "ex:freshtarget" nil nil nil "merge")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "op=merge requires" out))))))


;;; ---------------------------------------------------------------------------
;;; Default op (rename) still refuses on TARGET collision
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-merge-test-default-op-still-refuses-collision ()
  "Without op=merge, an already-declared TARGET is still refused."
  (elot-gptel-rename-merge-test--with-fixture path
      (elot-gptel-rename-merge-test--fixture)
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-merge-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-merge-test--rel path)
                  "ex:pantherinae" "ex:cat")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "collides\\|collision\\|existing" out))))))


;;; ---------------------------------------------------------------------------
;;; F1 round-trip: elot_replace_with_parent succeeds end-to-end with merge
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-merge-test-replace-with-parent-roundtrip ()
  "F1 (elot_replace_with_parent) succeeds end-to-end on a file
where the parent (TARGET) is already declared -- which is its
defining use case."
  (elot-gptel-rename-merge-test--with-fixture path
      (elot-gptel-rename-merge-test--fixture)
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-merge-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rename-merge-test--rel path)
                  "ex:pantherinae")))
        (should (stringp out))
        (should (string-prefix-p "OK:" out))
        (should (string-match-p "NOTE: ex:pantherinae folded into ex:cat"
                                out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should-not (string-match-p "(ex:pantherinae)" bytes))
        (should (string-match-p
                 "SubClassOf[ \t]*::[ \t]*ex:cat" bytes))))))


(provide 'elot-gptel-rename-merge-test)
;;; elot-gptel-rename-merge-test.el ends here
