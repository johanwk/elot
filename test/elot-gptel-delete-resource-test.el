;;; elot-gptel-delete-resource-test.el --- M9.9 B.2 tool wrapper tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-delete-resource-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9 Slice B.2 -- tests
;; for the `elot_delete_resource' singleton tool wrapper.
;; Pure-Elisp; lint / OMN-validate stages are stubbed via
;; cl-letf so no ROBOT is required.
;;
;; Covers:
;;   - side-effects gate (refuses when flag off, unless dry_run)
;;   - happy path: leaf deletion, file rewritten on disk
;;   - cascade=refuse (default) refuses when SUBJECT has
;;     children; lists the children in the diagnostic
;;   - cascade=reparent: children promoted one outline level
;;   - cascade=delete: whole subtree removed
;;   - dangling axiom-row reference -> refusal (regardless of
;;     cascade) with the offending row enumerated
;;   - dry_run round-trip: file on disk byte-identical after
;;   - unknown subject -> structured ERROR
;;   - tool-spec registration + dispatcher arity

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-dr-test--repo-root nil)
(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this))
  (setq elot-gptel-dr-test--repo-root root))

(require 'elot-tangle)
(require 'elot-id-rename)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-dr-test--fixture ()
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
   " - SubClassOf :: ex:animal\n"
   "**** Puppy (ex:puppy)\n"
   "**** Beagle (ex:beagle)\n"
   "*** Cat (ex:cat)\n"
   "*** Spooky (ex:spooky)\n"
   " - rdfs:comment :: scares ex:dog\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   " - Domain :: ex:dog\n"
   " - Range :: ex:cat\n"))

(defun elot-gptel-dr-test--write (filename)
  (let ((path (expand-file-name filename
                                elot-gptel-dr-test--repo-root)))
    (with-temp-file path (insert (elot-gptel-dr-test--fixture)))
    path))

(defmacro elot-gptel-dr-test--with-fixture (path-var &rest body)
  (declare (indent 1))
  `(let ((,path-var (elot-gptel-dr-test--write
                     (format "elot-delete-fixture-%d.org"
                             (random 1000000)))))
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

(defun elot-gptel-dr-test--rel (path)
  (file-relative-name path elot-gptel-dr-test--repo-root))


;;; ---------------------------------------------------------------------------
;;; Defined / registration
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-defined ()
  (should (fboundp 'elot-gptel-tool-delete-resource)))

(ert-deftest elot-gptel-dr-test-spec-registered ()
  (let ((spec (assoc "elot_delete_resource" elot-gptel--tool-specs)))
    (should spec)
    (should (eq 'elot-gptel-tool-delete-resource
                (plist-get (cdr spec) :function)))
    (should (eq t (plist-get (cdr spec) :confirm)))
    (let ((args (plist-get (cdr spec) :args)))
      (should (= 4 (length args))))))

(ert-deftest elot-gptel-dr-test-dispatcher-arity ()
  (let ((thunk (elot-gptel--tool-thunk
                'elot-gptel-tool-delete-resource)))
    (cl-letf (((symbol-function 'elot-gptel-tool-delete-resource)
               (lambda (&rest args)
                 (format "got %d args" (length args)))))
      (should (string= "got 4 args"
                       (funcall thunk "f" "s" "c" "d"))))))


;;; ---------------------------------------------------------------------------
;;; Side-effect gate
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-side-effects-disabled ()
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects nil)
          (default-directory elot-gptel-dr-test--repo-root))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:cat")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "side effects disabled" out))))))


;;; ---------------------------------------------------------------------------
;;; Happy path: leaf delete
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-leaf-success ()
  ;; ex:spooky has no children and is not referenced by any axiom
  ;; row on another subject (its own rdfs:comment row mentioning
  ;; ex:dog does not count -- the scan flags only incoming refs).
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:spooky")))
        (should (string-prefix-p "OK: deleted ex:spooky" out))
        (should (string-match-p "== LINT ==" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should-not (string-match-p "(ex:spooky)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; cascade=refuse on subject with children
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-refuses-on-children ()
  "Default cascade refuses with the child list when SUBJECT has children.
Note: ex:dog is also referenced by ex:chases (Domain), so this
case actually trips the dangling-reference refusal first.  Use
a different subject -- ex:spooky has no children and no
references, so insert a synthetic parent for the test."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      ;; Strip the Range :: ex:cat row so the cascade=refuse branch
      ;; on children fires before any dangling-reference refusal.
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-min))
        (while (re-search-forward "^ - Range :: ex:cat\n" nil t)
          (replace-match ""))
        (goto-char (point-max))
        (re-search-backward "^\\*\\*\\* Cat (ex:cat)" nil t)
        (forward-line 1)
        (insert "**** Kitten (ex:kitten)\n")
        (save-buffer)
        (when (fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale)))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:cat")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "semantic dependent" out))
        (should (string-match-p "ex:kitten" out))
        (should (string-match-p "cascade=" out))))))


;;; ---------------------------------------------------------------------------
;;; cascade=reparent: children promoted
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-reparent-promotes-children ()
  "cascade=reparent promotes Dog's children when no axiom rows reference Dog.
The fixture's ex:chases Domain row references ex:dog, so to
exercise reparent cleanly we first delete the offending row."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      ;; Strip the Domain/Range rows on ex:chases that reference
      ;; ex:dog / ex:cat to clear the dangling-reference refusal.
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-min))
        (while (re-search-forward
                "^ - Domain :: ex:dog\n\\| - Range :: ex:cat\n"
                nil t)
          (replace-match ""))
        (save-buffer)
        (when (fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale)))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:dog" "reparent")))
        (should (string-prefix-p "OK: deleted ex:dog" out))
        (should (string-match-p "2 child heading.* promoted" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (should-not (string-match-p "^\\*\\*\\* Dog " bytes))
        ;; Puppy and Beagle now sit at the level Dog used to (3 stars).
        (should (string-match-p "^\\*\\*\\* Puppy " bytes))
        (should (string-match-p "^\\*\\*\\* Beagle " bytes))))))


;;; ---------------------------------------------------------------------------
;;; cascade=delete: subtree removed
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-cascade-delete-subtree ()
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      ;; Strip ex:dog/ex:cat references AND the Spooky comment that
      ;; mentions ex:dog (so the post-delete byte check on "ex:dog"
      ;; sees a clean file).
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-min))
        (while (re-search-forward
                "^ - Domain :: ex:dog\n\\| - Range :: ex:cat\n\\| - rdfs:comment :: scares ex:dog\n"
                nil t)
          (replace-match ""))
        (save-buffer)
        (when (fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale)))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:dog" "delete")))
        (should (string-prefix-p "OK: deleted ex:dog" out))
        (should (string-match-p "2 descendant heading" out)))
      (let ((bytes (with-temp-buffer
                     (insert-file-contents path)
                     (buffer-string))))
        (dolist (gone '("ex:dog" "ex:puppy" "ex:beagle"))
          (should-not (string-match-p (regexp-quote gone) bytes)))))))


;;; ---------------------------------------------------------------------------
;;; Dangling axiom-row reference refusal
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-refuses-on-dangling-axiom-row ()
  "Even cascade=delete refuses when SUBJECT is referenced by an
axiom row on a different subject.  The fixture's ex:chases
Domain references ex:dog -- deleting ex:dog must refuse with
the offending row enumerated."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:dog" "delete")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "dangling reference" out))
        (should (string-match-p "ex:chases" out))
        (should (string-match-p "Domain" out))
        (should (string-match-p "elot_edit_axiom" out))))))


;;; ---------------------------------------------------------------------------
;;; dry_run round-trip
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-dry-run-leaves-file-untouched ()
  ;; Use ex:spooky -- a true leaf with no incoming references.
  (elot-gptel-dr-test--with-fixture path
    (let ((default-directory elot-gptel-dr-test--repo-root)
          (elot-gptel-allow-side-effects nil)
          (before (with-temp-buffer
                    (insert-file-contents path) (buffer-string))))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:spooky"
                  nil t)))
        (should (string-prefix-p "OK: deleted ex:spooky" out))
        (should (string-match-p "dry_run" out)))
      (let ((after (with-temp-buffer
                     (insert-file-contents path) (buffer-string))))
        (should (string= before after))))))


;;; ---------------------------------------------------------------------------
;;; Unknown subject
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-unknown-subject ()
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:nope")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "not a declared resource" out))))))


;;; ---------------------------------------------------------------------------
;;; B.3 follow-up: wrapper-level rollback on revalidation failure
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-revalidation-rolls-back ()
  "A lint error after the byte-level delete restores the pre-delete file.
Exercises the wrapper-layer rollback path -- complementary to
the B.1 atomic-rollback test (which signals mid-surgery)."
  (elot-gptel-dr-test--with-fixture path
    (let* ((elot-gptel-allow-side-effects t)
           (default-directory elot-gptel-dr-test--repo-root)
           (before (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
      (cl-letf (((symbol-function 'elot-gptel-tool-lint)
                 (lambda (&rest _)
                   "1:1  [elot/fake/low]  synthetic error\nSummary: 1 error, 0 warnings")))
        (let ((out (elot-gptel-tool-delete-resource
                    (elot-gptel-dr-test--rel path) "ex:spooky")))
          (should (stringp out))
          (should (string-prefix-p
                   "ERROR: revalidation failed -- changes rolled back"
                   out))
          (should (string-match-p "synthetic error" out))))
      ;; File on disk restored to pre-delete bytes.
      (let ((after (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
        (should (string= before after))))))


;;; ---------------------------------------------------------------------------
;;; B.3 follow-up: F8 hierarchy freshness across mutators
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-hierarchy-fresh-after-delete ()
  "A delete followed by a rename on a sibling succeeds; the rename
sees the post-delete hierarchy state.  Exercises the F8 contract
that `elot-headline-hierarchy-mark-stale' fires post-delete (the
mirror image of the analogous post-rename / post-insert test)."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      ;; Delete ex:spooky (clean leaf), then rename ex:cat -> ex:feline
      ;; in the same buffer.  The rename must succeed without
      ;; spuriously seeing the pre-delete hierarchy state.
      (let ((del (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:spooky")))
        (should (string-prefix-p "OK: deleted ex:spooky" del)))
      (let ((ren (elot-gptel-tool-rename-resource
                  (elot-gptel-dr-test--rel path)
                  "ex:cat" "ex:feline")))
        (should (string-prefix-p "OK:" ren)))
      (let ((bytes (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
        (should-not (string-match-p "(ex:spooky)" bytes))
        (should-not (string-match-p "(ex:cat)" bytes))
        (should (string-match-p "(ex:feline)" bytes))))))


;;; ---------------------------------------------------------------------------
;;; B.3 follow-up: dangling-reference refusal precedence under cascade=reparent
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-dangling-refuses-under-reparent ()
  "Axiom-row references refuse regardless of cascade.  The
existing `refuses-on-dangling-axiom-row' test pins cascade=delete;
this test pins the same precedence under cascade=reparent."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:dog" "reparent")))
        (should (string-prefix-p "ERROR:" out))
        (should (string-match-p "dangling reference" out))
        (should (string-match-p "ex:chases" out))
        (should (string-match-p "Domain" out))
        ;; Refusal precedence: dangling refs win over the
        ;; children-branch -- we must not see the cascade hint.
        (should-not (string-match-p "semantic dependent" out))))))


;;; ---------------------------------------------------------------------------
;;; B.2 follow-up: annotation-row advisory NOTE
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-dr-test-annotation-row-note ()
  "An annotation-property row on another subject that mentions
SUBJECT must NOT refuse the delete (OWL tolerates undeclared
IRIs in annotation values), but must surface in the OK envelope
as a `NOTE:' block enumerating the row(s) for audit."
  (elot-gptel-dr-test--with-fixture path
    (let ((elot-gptel-allow-side-effects t)
          (default-directory elot-gptel-dr-test--repo-root))
      ;; Add `rdfs:seeAlso :: ex:spooky' under ex:cat -- an
      ;; annotation-property row on another subject pointing at
      ;; the soon-to-be-deleted ex:spooky.
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-min))
        (re-search-forward "^\\*\\*\\* Cat (ex:cat)\n")
        (insert " - rdfs:seeAlso :: ex:spooky\n")
        (save-buffer)
        (when (fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale)))
      (let ((out (elot-gptel-tool-delete-resource
                  (elot-gptel-dr-test--rel path) "ex:spooky")))
        (should (string-prefix-p "OK: deleted ex:spooky" out))
        (should (string-match-p "audit recommended" out))
        (should (string-match-p
                 "NOTE: 1 annotation row.* reference ex:spooky" out))
        (should (string-match-p "ex:cat" out))
        (should (string-match-p "rdfs:seeAlso" out)))
      ;; File still on disk minus ex:spooky's heading, but the
      ;; rdfs:seeAlso row on ex:cat is untouched.
      (let ((bytes (with-temp-buffer (insert-file-contents path)
                                     (buffer-string))))
        (should-not (string-match-p "(ex:spooky)" bytes))
        (should (string-match-p "rdfs:seeAlso :: ex:spooky" bytes))))))


;;; elot-gptel-delete-resource-test.el ends here
