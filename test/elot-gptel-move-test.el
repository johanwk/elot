;;; elot-gptel-move-test.el --- Tests for the elot_move_resource gptel tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-move-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.1 -- tests for the
;; LLM-facing `elot_move_resource' tool wrapper.  Pure-Elisp; the
;; tests stub the lint and OMN-validate stages so no ROBOT is
;; required.  Side-effects flag is bound around each test so the
;; user's global setting is not perturbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-move-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-move-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-id-move)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology with two classes, an object property,
;;; a datatype and an individual.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-move-test--fixture ()
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
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "**** Dog (ex:dog)\n"
   "*** Plant (ex:plant)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"))

(defun elot-gptel-move-test--write-fixture (filename)
  "Write the fixture under the repo root with FILENAME; return the absolute path."
  (let ((path (expand-file-name filename
                                elot-gptel-move-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-move-test--fixture)))
    path))

(defmacro elot-gptel-move-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a freshly-written fixture file and run BODY.
The file is unlinked after BODY returns.  Lint and OMN-validate
are stubbed to return clean reports so revalidation never blocks."
  (declare (indent 1))
  `(let* ((,path-var (elot-gptel-move-test--write-fixture
                      (format "elot-move-fixture-%d.org" (random 1000000)))))
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

(defun elot-gptel-move-test--rel (path)
  (file-relative-name path elot-gptel-move-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Side-effect gate
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-side-effects-disabled ()
  "Refuses with a structured ERROR when the side-effects flag is nil."
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:dog" "ex:plant" "sibling")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: move a class as sibling, file rewritten on disk
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-success-sibling ()
  "A sibling move succeeds, writes the file, and emits an OK line."
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:dog" "ex:plant" "sibling")))
        (should (stringp out))
        (should (string-prefix-p "OK: moved ex:dog" out))
        (should (string-match-p "as sibling" out))
        (should (string-match-p "== LINT ==" out)))
      ;; On disk: Dog is now at level 3 directly under Classes.
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        ;; ex:dog should now appear with three stars (was four).
        (should (string-match-p "^\\*\\*\\* Dog (ex:dog)" bytes))
        (should-not (string-match-p "^\\*\\*\\*\\* Dog (ex:dog)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: move to section root via `top'
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-success-top ()
  "TARGET=`top' lifts a class to the section root."
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:dog" "top")))
        (should (stringp out))
        (should (string-prefix-p "OK: moved ex:dog" out))
        (should (string-match-p "as child" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "^\\*\\*\\* Dog (ex:dog)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Kind mismatch refusal
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-kind-mismatch ()
  "Moving a class under an object property is refused with a CURIE-aware ERROR."
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:dog" "ex:chases")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "kind mismatch" out))))))


;;; ---------------------------------------------------------------------------
;;; Bad `as' argument
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-bad-as ()
  "An unrecognised `as' value is refused."
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:dog" "ex:plant" "nephew")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "child\\|sibling" out))))))


;;; ---------------------------------------------------------------------------
;;; Source not declared
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-source-missing ()
  (elot-gptel-move-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-move-test--repo-root))
      (let ((out (elot-gptel-tool-move-resource
                  (elot-gptel-move-test--rel path)
                  "ex:nope" "ex:plant" "sibling")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))))))


;;; ---------------------------------------------------------------------------
;;; Revalidation failure -> rollback
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-revalidation-rolls-back ()
  "A lint error after the move restores the pre-move bytes."
  (elot-gptel-move-test--with-fixture path
    (let* ((elot-gptel-allow-side-effects t)
           (default-directory elot-gptel-move-test--repo-root)
           (before (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
      (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                 (lambda (&rest _)
                   "1:1  [elot/fake/low]  synthetic error\nSummary: 1 error, 0 warnings")))
        (let ((out (elot-gptel-tool-move-resource
                    (elot-gptel-move-test--rel path)
                    "ex:dog" "ex:plant" "sibling")))
          (should (stringp out))
          (should (string-prefix-p
                   "ERROR: revalidation failed -- changes rolled back"
                   out))
          (should (string-match-p "synthetic error" out))))
      (let ((after (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
        (should (string= before after))))))


;;; ---------------------------------------------------------------------------
;;; Tool registration: spec + dispatcher arity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-move-test-spec-registered ()
  "`elot_move_resource' is present in the tool-spec table with the
right argument shape."
  (let ((spec (assoc "elot_move_resource" elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-move-resource))
      (should (= 4 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "source"))
      (should (equal (plist-get (nth 2 args) :name) "target"))
      (should (equal (plist-get (nth 3 args) :name) "as")))))

(ert-deftest elot-gptel-move-test-dispatcher-arity ()
  "The dispatcher accepts the four-argument form (3 required + 1 optional)."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-move-resource)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-move-resource)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 4 args"
                       (funcall thunk "f" "s" "t" "child"))))))


(provide 'elot-gptel-move-test)
;;; elot-gptel-move-test.el ends here
