;;; elot-gptel-rename-test.el --- Tests for the elot_rename_resource gptel tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-rename-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.4 -- tests for the
;; LLM-facing `elot_rename_resource' tool wrapper.  Pure-Elisp; the
;; tests stub the lint and OMN-validate stages so no ROBOT is
;; required.  Side-effects flag is bound around each test so the
;; user's global setting is not perturbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-rename-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-rename-test--repo-root repo-root))

(require 'elot-gptel)
(require 'elot-id-rename)

;;; ---------------------------------------------------------------------------
;;; Fixture: minimal ontology with classes, an OP with axiom annotations,
;;; an individual, and a full-IRI reference.
;;; ---------------------------------------------------------------------------

(defun elot-gptel-rename-test--fixture ()
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
   "*** Animal (ex:animal)\n"
   "*** Dog (ex:dog)\n"
   " - rdfs:seeAlso :: <http://example.org/dog>\n"
   "*** Dogfood (ex:dogfood)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   " - Range :: ex:cat\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"
   " - Types :: ex:dog\n"))

(defun elot-gptel-rename-test--write-fixture (filename)
  "Write the fixture under the repo root with FILENAME; return the absolute path."
  (let ((path (expand-file-name filename
                                elot-gptel-rename-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-rename-test--fixture)))
    path))

(defmacro elot-gptel-rename-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a freshly-written fixture file and run BODY.
The file is unlinked after BODY returns.  Lint and OMN-validate
are stubbed to return clean reports so revalidation never blocks."
  (declare (indent 1))
  `(let* ((,path-var (elot-gptel-rename-test--write-fixture
                      (format "elot-rename-fixture-%d.org" (random 1000000)))))
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
       ;; Kill the visiting buffer so no stale state leaks between tests.
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-rename-test--rel (path)
  "Return PATH relative to the repo root."
  (file-relative-name path elot-gptel-rename-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Side-effect gate
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-side-effects-disabled ()
  "Refuses with a structured ERROR when the side-effects flag is nil."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "ex:canine")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: same-prefix rename, file saved + report formatted
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-success-same-prefix ()
  "A same-prefix rename succeeds, writes back the file, and emits OK with counts."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "ex:canine")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed ex:dog -> ex:canine" out))
        (should (string-match-p "CURIE" out))
        (should (string-match-p "IRI" out))
        ;; The success line includes the lint/omn reports.
        (should (string-match-p "== LINT ==" out)))
      ;; File on disk is rewritten.
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "(ex:canine)" bytes))
        (should-not (string-match-p "(ex:dog)" bytes))
        ;; Full-IRI rewrite preserved the angle-bracket form.
        (should (string-match-p "<http://example.org/canine>" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Target collision
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-target-collides ()
  "Target colliding with an existing declaration is refused."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "ex:dogfood")))
        (should (stringp out))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "collides\\|collision\\|existing" out))))))


;;; ---------------------------------------------------------------------------
;;; Undeclared target prefix WITHOUT target_iri -> structured error
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-undeclared-prefix-no-target-iri ()
  "Refused with a structured candidate line when prefix is undeclared
and target_iri is omitted."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      ;; Stub elot-db candidate lookup to return a deterministic list.
      (cl-letf (((symbol-function 'elot-id-rename--db-candidates)
                 (lambda (_prefix)
                   '("http://candidate.example.org/"
                     "http://other.example.com/"))))
        (let ((out (elot-gptel-tool-rename-resource
                    (elot-gptel-rename-test--rel path)
                    "ex:dog" "newp:canine")))
          (should (stringp out))
          (should (string-prefix-p "ERROR:" out))
          (should (string-match-p "undeclared prefix `newp:'" out))
          (should (string-match-p "<http://candidate.example.org/>" out))
          (should (string-match-p "|" out))
          (should (string-match-p "retry with target_iri" out)))))))

(ert-deftest elot-gptel-rename-test-undeclared-prefix-no-candidates ()
  "Empty candidate list surfaces as `(none)' with the fresh-target hint."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (cl-letf (((symbol-function 'elot-id-rename--db-candidates)
                 (lambda (_prefix) nil)))
        (let ((out (elot-gptel-tool-rename-resource
                    (elot-gptel-rename-test--rel path)
                    "ex:dog" "newp:canine")))
          (should (stringp out))
          (should (string-prefix-p "ERROR:" out))
          (should (string-match-p "candidates: (none)" out))
          (should (string-match-p "supply a fresh target_iri" out)))))))


;;; ---------------------------------------------------------------------------
;;; Undeclared target prefix WITH target_iri -> proceeds + declared trailer
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-undeclared-prefix-with-target-iri ()
  "Supplying target_iri adds the prefix row and reports it in the success line."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "newp:canine" nil
                  "http://new.example.org/")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed" out))
        (should (string-match-p
                 "declared prefix newp: -> <http://new.example.org/>"
                 out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "(newp:canine)" bytes))
        (should (string-match-p "newp:.*http://new.example.org/" bytes))
        (should (string-match-p "<http://new.example.org/canine>" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Revalidation failure -> rollback
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-revalidation-rolls-back ()
  "A lint error after the rewrite restores the pre-rewrite bytes."
  (elot-gptel-rename-test--with-fixture path
    (let* ((elot-gptel-allow-side-effects t)
           (default-directory elot-gptel-rename-test--repo-root)
           (before (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
      ;; Override the lint stub to report an error after the rewrite.
      (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                 (lambda (&rest _)
                   "1:1  [elot/fake/low]  synthetic error\nSummary: 1 error, 0 warnings")))
        (let ((out (elot-gptel-tool-rename-resource
                    (elot-gptel-rename-test--rel path)
                    "ex:dog" "ex:canine")))
          (should (stringp out))
          (should (string-prefix-p
                   "ERROR: revalidation failed -- changes rolled back"
                   out))
          (should (string-match-p "synthetic error" out))))
      ;; File on disk restored to pre-rewrite bytes.
      (let ((after (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
        (should (string= before after))))))


;;; ---------------------------------------------------------------------------
;;; new_label argument -- rewrite heading label alongside the CURIE
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-new-label-rewrites-heading ()
  "Supplying new_label rewrites the resource heading's label inside
the same atomic operation as the CURIE rewrite; bytes on disk
reflect both changes."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "ex:canine" nil nil "Canine")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed ex:dog -> ex:canine" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "\\*\\*\\* Canine (ex:canine)" bytes))
        (should-not (string-match-p "(ex:dog)" bytes))
        (should-not (string-match-p "\\*\\*\\* Dog (ex:canine)" bytes))))))

(ert-deftest elot-gptel-rename-test-new-label-with-target-iri ()
  "new_label and target_iri compose: cross-prefix rename plus label rewrite."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "newp:canine" nil
                  "http://new.example.org/"
                  "Canine")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed" out))
        (should (string-match-p
                 "declared prefix newp: -> <http://new.example.org/>"
                 out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "\\*\\*\\* Canine (newp:canine)" bytes))
        (should (string-match-p "<http://new.example.org/canine>" bytes))))))

(ert-deftest elot-gptel-rename-test-new-label-empty-ignored ()
  "An empty new_label argument is treated the same as nil -- only the
CURIE changes; the original heading label is preserved."
  (elot-gptel-rename-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rename-test--repo-root))
      (let ((out (elot-gptel-tool-rename-resource
                  (elot-gptel-rename-test--rel path)
                  "ex:dog" "ex:canine" nil nil "")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should (string-match-p "\\*\\*\\* Dog (ex:canine)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; Tool registration: spec + dispatcher arity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rename-test-spec-registered ()
  "`elot_rename_resource' is present in the tool-spec table with the
right argument shape."
  (let ((spec (assoc "elot_rename_resource" elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-rename-resource))
      (should (= 7 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "source"))
      (should (equal (plist-get (nth 2 args) :name) "target"))
      (should (equal (plist-get (nth 3 args) :name) "ontology"))
      (should (equal (plist-get (nth 4 args) :name) "target_iri"))
      (should (equal (plist-get (nth 5 args) :name) "new_label"))
      (should (equal (plist-get (nth 6 args) :name) "op")))))

(ert-deftest elot-gptel-rename-test-dispatcher-arity ()
  "The dispatcher accepts the seven-argument form (3 required + 4 optional)."
  (let ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-rename-resource)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-rename-resource)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 7 args"
                       (funcall thunk "f" "s" "t" "o" "i" "l" "op"))))))


(provide 'elot-gptel-rename-test)
;;; elot-gptel-rename-test.el ends here
