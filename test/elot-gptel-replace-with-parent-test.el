;;; elot-gptel-replace-with-parent-test.el --- M9.9.F1 tool tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-replace-with-parent-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9.F1 -- tests for the
;; `elot_replace_with_parent' tool wrapper.  Pure-Elisp; the
;; underlying `elot-gptel-tool-rename-resource' is stubbed so no
;; ROBOT is required and so we can pin the wrapper's parent-guard
;; behaviour without bringing the full rename pipeline along.
;;
;; Covers:
;;   - single-parent happy path -> auto-pick + NOTE appended
;;   - multi-parent refusal -> candidates listed
;;   - non-parent rejection (parent= not an immediate parent)
;;   - 0-parent refusal
;;   - dry_run round-trip: file on disk unchanged
;;   - tool-spec registration
;;   - dispatcher arity
;;   - NOTE line present in OK envelope

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-rwp-tool-test--repo-root nil)
(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this))
  (setq elot-gptel-rwp-tool-test--repo-root root))

(require 'elot-tangle)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-rwp-tool-test--fixture ()
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
   "**** Dog (ex:dog)\n"
   "***** Beagle (ex:beagle)\n"
   "**** Hybrid (ex:hybrid)\n"
   " - SubClassOf :: ex:mammal\n"
   "*** Mammal (ex:mammal)\n"
   "*** Loner (ex:loner)\n"
   "*** Unrelated (ex:unrelated)\n"))

(defun elot-gptel-rwp-tool-test--write (filename)
  (let ((path (expand-file-name filename
                                elot-gptel-rwp-tool-test--repo-root)))
    (with-temp-file path
      (insert (elot-gptel-rwp-tool-test--fixture)))
    path))

(defmacro elot-gptel-rwp-tool-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a freshly-written fixture; stub rename; run BODY.
The underlying `elot-gptel-tool-rename-resource' is stubbed to
return a canned OK envelope without touching anything -- F1's
job is parent enumeration + guard + delegation, not the rename
itself."
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-rwp-tool-test--write
                     "elot-gptel-rwp-tool-test-fixture.org")))
     (unwind-protect
         (cl-letf (((symbol-function 'elot-gptel-tool-rename-resource)
                    (lambda (_file source target &rest _)
                      (format
                       "OK: renamed %s -> %s (1 CURIE, 0 IRI; 0 prose mentions skipped)\n\n== LINT ==\nOK: no lint issues\n"
                       source target))))
           ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

(defun elot-gptel-rwp-tool-test--rel (path)
  (file-relative-name path elot-gptel-rwp-tool-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Happy paths
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rwp-tool-test-defined ()
  (should (fboundp 'elot-gptel-tool-replace-with-parent)))

(ert-deftest elot-gptel-rwp-tool-test-single-parent-autopick ()
  "ex:dog has exactly one immediate parent (ex:animal); the wrapper
auto-picks it, delegates to rename, and appends the NOTE."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rwp-tool-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rwp-tool-test--rel path)
                  "ex:dog")))
        (should (stringp out))
        (should (string-prefix-p "OK: renamed ex:dog -> ex:animal" out))
        (should (string-match-p "NOTE: ex:dog folded into ex:animal"
                                out))))))

(ert-deftest elot-gptel-rwp-tool-test-multi-parent-supplied ()
  "ex:hybrid has two immediate parents (ex:animal heading-nested,
ex:mammal from SubClassOf row); supplying parent=ex:mammal
disambiguates."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rwp-tool-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rwp-tool-test--rel path)
                  "ex:hybrid" "ex:mammal")))
        (should (string-prefix-p "OK: renamed ex:hybrid -> ex:mammal" out))
        (should (string-match-p "NOTE:" out))))))


;;; ---------------------------------------------------------------------------
;;; Guard branches
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rwp-tool-test-multi-parent-refusal ()
  "ex:hybrid has two parents and no parent= supplied -> refuse with
candidate list."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rwp-tool-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rwp-tool-test--rel path)
                  "ex:hybrid")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "2 immediate parents" out))
        (should (string-match-p "ex:animal" out))
        (should (string-match-p "ex:mammal" out))
        (should (string-match-p "supply parent=" out))))))

(ert-deftest elot-gptel-rwp-tool-test-non-parent-rejection ()
  "ex:unrelated is a declared class but not a parent of ex:dog ->
refuse with the actual candidate list."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rwp-tool-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rwp-tool-test--rel path)
                  "ex:dog" "ex:unrelated")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p
                 "ex:unrelated is not an immediate parent of ex:dog" out))
        (should (string-match-p "ex:animal" out))))))

(ert-deftest elot-gptel-rwp-tool-test-zero-parent-refusal ()
  "ex:loner sits directly under the section root and has no
SubClassOf rows -> no parents -> refuse."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-rwp-tool-test--repo-root))
      (let ((out (elot-gptel-tool-replace-with-parent
                  (elot-gptel-rwp-tool-test--rel path)
                  "ex:loner")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "no immediate parents" out))))))


;;; ---------------------------------------------------------------------------
;;; dry_run
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rwp-tool-test-dry-run-round-trip ()
  "dry_run=t snapshots disk bytes before delegating, restores them
afterwards regardless of the rename's verdict, and annotates the
OK line."
  (elot-gptel-rwp-tool-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil) ;; gate stays off; dry_run bypasses
          (default-directory elot-gptel-rwp-tool-test--repo-root)
          (before-bytes (with-temp-buffer
                          (insert-file-contents path)
                          (buffer-string))))
      ;; Make the stubbed rename also overwrite the file, so the
      ;; restore step has something to undo.
      (cl-letf (((symbol-function 'elot-gptel-tool-rename-resource)
                 (lambda (file source target &rest _)
                   (let ((true-file (expand-file-name file)))
                     (with-temp-file true-file
                       (insert "MUTATED\n")))
                   (format "OK: renamed %s -> %s (1 CURIE, 0 IRI; 0 prose mentions skipped)\n"
                           source target))))
        (let ((out (elot-gptel-tool-replace-with-parent
                    (elot-gptel-rwp-tool-test--rel path)
                    "ex:dog" nil t)))
          (should (string-prefix-p "OK:" out))
          (should (string-match-p "dry_run: file unchanged on disk" out))))
      ;; File on disk is byte-identical to baseline.
      (let ((after-bytes (with-temp-buffer
                           (insert-file-contents path)
                           (buffer-string))))
        (should (string= before-bytes after-bytes))))))


;;; ---------------------------------------------------------------------------
;;; Tool registration: spec + dispatcher arity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-rwp-tool-test-spec-registered ()
  (let ((spec (assoc "elot_replace_with_parent"
                     elot-gptel--tool-specs)))
    (should spec)
    (let* ((plist (cdr spec))
           (args (plist-get plist :args)))
      (should (eq (plist-get plist :function)
                  'elot-gptel-tool-replace-with-parent))
      (should (eq (plist-get plist :confirm) t))
      (should (= 4 (length args)))
      (should (equal (plist-get (nth 0 args) :name) "file"))
      (should (equal (plist-get (nth 1 args) :name) "subject"))
      (should (equal (plist-get (nth 2 args) :name) "parent"))
      (should (equal (plist-get (nth 3 args) :name) "dry_run"))
      (should (eq (plist-get (nth 3 args) :type) 'boolean)))))

(ert-deftest elot-gptel-rwp-tool-test-dispatcher-arity ()
  "The dispatcher accepts the four-argument form (2 required + 2 optional)."
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-replace-with-parent)))
    (should (functionp thunk))
    (cl-letf (((symbol-function 'elot-gptel-tool-replace-with-parent)
               (lambda (&rest args) (format "got %d args" (length args)))))
      (should (string= "got 4 args"
                       (funcall thunk "f" "s" "p" t))))))


(provide 'elot-gptel-replace-with-parent-test)
;;; elot-gptel-replace-with-parent-test.el ends here
