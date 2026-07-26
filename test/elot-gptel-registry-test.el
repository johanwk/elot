;;; elot-gptel-registry-test.el --- Tool-registry meta-test  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-registry-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-V1-PLAN.org Phase 1 Step 1.2 (Dispatcher / spec
;; consistency).  A single meta-test suite that walks
;; `elot-gptel--tool-specs' and asserts, for every registered tool:
;;
;;   - `:function' is `fboundp';
;;   - a `elot-gptel--tool-thunk' dispatcher arm exists (no
;;     "no dispatcher" error);
;;   - dispatcher arity agrees with the spec's `:args' (required /
;;     optional counts match `func-arity');
;;   - `:args' names are unique and non-empty, and every optional
;;     arg follows the required ones (positional &optional shape);
;;   - tool names are unique and no two specs share a `:function';
;;   - every known mutating tool carries `:confirm t' (this is the
;;     check that would have caught the Step 0.1 mismatch, where
;;     `elot_rename_resource' / `elot_move_resource' shipped without
;;     `:confirm');
;;   - mutating (`:confirm t') specs dispatch to functions that
;;     honour the side-effects gate: called with
;;     `elot-gptel-allow-side-effects' nil they return a refusal
;;     string rather than mutating.
;;
;; Pure-Elisp; gptel is not required (the spec table and dispatcher
;; are plain data / a `pcase').  The gate smoke-test writes a small
;; fixture under the repo root so the file-reading mutators
;; (`elot_replace_with_parent' resolves the file before delegating)
;; have something to open; lint / OMN-validate are never reached
;; because the gate fires first.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-gptel-registry-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-registry-test--repo-root repo-root))

(require 'elot-gptel)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun elot-gptel-registry-test--specs ()
  "Return `elot-gptel--tool-specs' (list of (NAME . PLIST))."
  elot-gptel--tool-specs)

(defun elot-gptel-registry-test--arg-required-p (arg)
  "Non-nil when ARG (an `:args' plist) is a required argument."
  (not (plist-get arg :optional)))

(defun elot-gptel-registry-test--dispatcher-arity (fn)
  "Return (MIN . MAX) `func-arity' of the dispatcher thunk for FN.
Signals an error via `elot-gptel--tool-thunk' when FN has no
dispatcher arm."
  (func-arity (elot-gptel--tool-thunk fn)))

;; Canonical roster of mutating tools -- every tool that writes to
;; disk / the DB and therefore honours the side-effects gate.  Kept
;; as an explicit list (rather than derived from `:confirm') so the
;; test fails loudly if a mutator ever loses its `:confirm t' flag
;; (the exact Step 0.1 regression: rename/move once lacked it).
(defconst elot-gptel-registry-test--mutating-tools
  '("elot_rename_resource"
    "elot_move_resource"
    "elot_delete_resource"
    "elot_replace_with_parent"
    "elot_insert_sibling_resource"
    "elot_insert_child_resource"
    "elot_insert_resource_tree"
    "elot_edit_axiom"
    "elot_edit_axioms"
    "elot_db_remove_source")
  "Tool names expected to be mutating (side-effect-gated, `:confirm t').")

;; Minimal, gate-off invocation arguments per mutating tool, in the
;; positional order the dispatcher thunk expects.  Chosen so the
;; side-effects gate is the first thing that fires (valid-enough
;; args to clear any pre-gate shape validation).  `<FILE>' is
;; substituted with the fixture path at call time.
(defconst elot-gptel-registry-test--mutating-args
  '(("elot_rename_resource"          "<FILE>" "ex:dog" "ex:hound")
    ("elot_move_resource"            "<FILE>" "ex:dog" "ex:animal")
    ("elot_delete_resource"          "<FILE>" "ex:dog")
    ("elot_replace_with_parent"      "<FILE>" "ex:dog")
    ("elot_insert_sibling_resource"  "<FILE>" "ex:dog" ["Hound"])
    ("elot_insert_child_resource"    "<FILE>" "ex:dog" ["Hound"])
    ("elot_insert_resource_tree"     "<FILE>" "ex:dog" ["Hound"])
    ("elot_edit_axiom"               "<FILE>" "ex:dog" "rdfs:comment" "\"x\"@en")
    ("elot_edit_axioms"              "<FILE>"
     ((:subject "ex:dog" :keyword "rdfs:comment" :fragment "\"x\"@en")))
    ("elot_db_remove_source"         "elot-registry-test-no-such-source"))
  "Per-tool positional args for the gate-off refusal smoke-test.")

(defun elot-gptel-registry-test--fixture ()
  "Return a minimal but valid ELOT ontology source string.
`ex:dog' is heading-nested under `ex:animal' so
`elot_replace_with_parent' can enumerate a parent before it
delegates to the (gate-refusing) rename."
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
   "**** Dog (ex:dog)\n"))

(defmacro elot-gptel-registry-test--with-fixture (path-var &rest body)
  "Bind PATH-VAR to a freshly-written fixture; run BODY; clean up."
  (declare (indent 1))
  `(let ((,path-var (expand-file-name
                     (format "elot-registry-fixture-%d.org" (random 1000000))
                     elot-gptel-registry-test--repo-root)))
     (with-temp-file ,path-var
       (insert (elot-gptel-registry-test--fixture)))
     (unwind-protect
         (progn ,@body)
       (when (file-exists-p ,path-var)
         (delete-file ,path-var))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string= (buffer-file-name b) ,path-var))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b))))))

;;; ---------------------------------------------------------------------------
;;; Sanity: the table is non-empty
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-specs-non-empty ()
  "The tool-spec table exists and holds a non-trivial roster."
  (let ((specs (elot-gptel-registry-test--specs)))
    (should (consp specs))
    (should (> (length specs) 10))))

;;; ---------------------------------------------------------------------------
;;; Every spec's :function is fboundp
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-function-fboundp ()
  "Each spec names an existing, callable `:function'."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let* ((name (car spec))
           (fn (plist-get (cdr spec) :function)))
      (should (symbolp fn))
      (unless (fboundp fn)
        (ert-fail (format "%s: :function %S is not fboundp" name fn))))))

;;; ---------------------------------------------------------------------------
;;; Every spec has a dispatcher arm
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-dispatcher-arm-exists ()
  "Each spec's `:function' resolves to a dispatcher thunk.
`elot-gptel--tool-thunk' signals for an unknown function (the
`_' pcase arm), so a clean `functionp' return proves the arm
exists."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let* ((name (car spec))
           (fn (plist-get (cdr spec) :function))
           (thunk (condition-case err
                      (elot-gptel--tool-thunk fn)
                    (error
                     (ert-fail
                      (format "%s: no dispatcher arm for %S (%s)"
                              name fn (error-message-string err)))))))
      (should (functionp thunk)))))

;;; ---------------------------------------------------------------------------
;;; Dispatcher arity agrees with the spec :args
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-arity-agrees ()
  "The dispatcher thunk arity matches each spec's `:args'.
MIN equals the count of required args; MAX equals the total arg
count (all optionals are positional-tail `&optional')."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let* ((name (car spec))
           (fn (plist-get (cdr spec) :function))
           (args (plist-get (cdr spec) :args))
           (required (cl-count-if
                      #'elot-gptel-registry-test--arg-required-p args))
           (total (length args))
           (arity (elot-gptel-registry-test--dispatcher-arity fn))
           (amin (car arity))
           (amax (cdr arity)))
      (unless (equal amin required)
        (ert-fail
         (format "%s: dispatcher min-arity %S != %d required args"
                 name amin required)))
      (unless (or (eq amax 'many) (equal amax total))
        (ert-fail
         (format "%s: dispatcher max-arity %S != %d total args"
                 name amax total))))))

;;; ---------------------------------------------------------------------------
;;; :args names are unique, non-empty, and optionals follow requireds
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-arg-names-clean ()
  "Every `:args' entry has a non-empty, unique `:name'."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let* ((name (car spec))
           (args (plist-get (cdr spec) :args))
           (names (mapcar (lambda (a) (plist-get a :name)) args)))
      (dolist (n names)
        (unless (and (stringp n) (not (string-empty-p n)))
          (ert-fail (format "%s: arg name %S is empty / non-string" name n))))
      (unless (equal (length names) (length (delete-dups (copy-sequence names))))
        (ert-fail (format "%s: duplicate arg names in %S" name names))))))

(ert-deftest elot-gptel-registry-test-optionals-follow-requireds ()
  "No required arg follows an optional arg (positional &optional shape)."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let* ((name (car spec))
           (args (plist-get (cdr spec) :args))
           (seen-optional nil))
      (dolist (a args)
        (if (elot-gptel-registry-test--arg-required-p a)
            (when seen-optional
              (ert-fail
               (format "%s: required arg %S follows an optional arg"
                       name (plist-get a :name))))
          (setq seen-optional t))))))

;;; ---------------------------------------------------------------------------
;;; Tool names unique; no two specs share a :function
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-names-unique ()
  "Tool names are unique across the spec table."
  (let ((names (mapcar #'car (elot-gptel-registry-test--specs))))
    (should (equal (length names)
                   (length (delete-dups (copy-sequence names)))))))

(ert-deftest elot-gptel-registry-test-functions-unique ()
  "No two specs dispatch to the same `:function'."
  (let ((fns (mapcar (lambda (s) (plist-get (cdr s) :function))
                     (elot-gptel-registry-test--specs))))
    (should (equal (length fns)
                   (length (delete-dups (copy-sequence fns)))))))

;;; ---------------------------------------------------------------------------
;;; Every known mutating tool carries :confirm t
;;; (the check that would have caught the Step 0.1 rename/move mismatch)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-mutators-have-confirm ()
  "Each known mutating tool is registered with `:confirm t'."
  (dolist (tool elot-gptel-registry-test--mutating-tools)
    (let ((spec (assoc tool (elot-gptel-registry-test--specs))))
      (unless spec
        (ert-fail (format "mutating tool %s is not registered" tool)))
      (unless (eq t (plist-get (cdr spec) :confirm))
        (ert-fail (format "mutating tool %s lacks :confirm t" tool))))))

(ert-deftest elot-gptel-registry-test-confirm-only-on-mutators ()
  "Only the known mutating tools carry `:confirm t'.
Guards against a read-only tool acquiring a spurious `:confirm'."
  (dolist (spec (elot-gptel-registry-test--specs))
    (let ((name (car spec)))
      (when (eq t (plist-get (cdr spec) :confirm))
        (unless (member name elot-gptel-registry-test--mutating-tools)
          (ert-fail
           (format "%s carries :confirm t but is not a known mutator"
                   name)))))))

;;; ---------------------------------------------------------------------------
;;; Mutating tools honour the side-effects gate (gate off -> refusal)
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-registry-test-mutators-honour-gate ()
  "With the side-effects gate off, each mutating tool refuses.
Calls the tool through its dispatcher thunk with minimal args
and asserts a refusal string (never a raw signal, never an OK)."
  (elot-gptel-registry-test--with-fixture path
    (let ((default-directory elot-gptel-registry-test--repo-root)
          (elot-gptel-allow-side-effects nil)
          (rel (file-relative-name path elot-gptel-registry-test--repo-root)))
      (dolist (tool elot-gptel-registry-test--mutating-tools)
        (let* ((entry (assoc tool elot-gptel-registry-test--mutating-args))
               (raw-args (cdr entry))
               (args (mapcar (lambda (a) (if (equal a "<FILE>") rel a))
                             raw-args))
               (spec (assoc tool (elot-gptel-registry-test--specs)))
               (fn (plist-get (cdr spec) :function))
               (thunk (elot-gptel--tool-thunk fn))
               (out (apply thunk args)))
          (unless (stringp out)
            (ert-fail (format "%s: gate-off call returned non-string %S"
                              tool out)))
          (when (string-prefix-p "OK:" out)
            (ert-fail (format "%s: gate-off call succeeded: %s" tool out)))
          (unless (string-match-p "refused\\|side effects disabled" out)
            (ert-fail (format "%s: gate-off refusal not recognised: %s"
                              tool out))))))))

(ert-deftest elot-gptel-registry-test-mutating-args-cover-roster ()
  "The gate smoke-test arg table covers every known mutating tool."
  (dolist (tool elot-gptel-registry-test--mutating-tools)
    (should (assoc tool elot-gptel-registry-test--mutating-args))))

;;; ---------------------------------------------------------------------------
;;; Shared arg fragments are `eq'-shared, not copied (Step 1.5.2)
;;; ---------------------------------------------------------------------------

;; Each canonical arg fragment (defined once as
;; `elot-gptel--arg-FOO') is spliced into every spec that uses it.
;; Splicing the *same* object guarantees byte-identical
;; descriptions/enums across tools; this test asserts the sharing is
;; real (`eq'), so a future inline copy that drifts out of sync fails
;; loudly here rather than silently diverging.

(defconst elot-gptel-registry-test--shared-fragments
  '((elot-gptel--arg-file
     "elot_check" "elot_lint" "elot_omn_validate" "elot_omn_report"
     "elot_unsatisfiable" "elot_consistency" "elot_explain"
     "elot_rename_resource" "elot_move_resource" "elot_delete_resource"
     "elot_replace_with_parent" "elot_insert_sibling_resource"
     "elot_insert_child_resource" "elot_insert_resource_tree"
     "elot_axiom_keywords" "elot_axiom_check"
     "elot_resources" "elot_read_resource"
     "elot_edit_axiom" "elot_edit_axioms")
    (elot-gptel--arg-file-rdf
     "elot_sparql" "elot_sparql_select")
    (elot-gptel--arg-reasoner
     "elot_unsatisfiable" "elot_consistency")
    (elot-gptel--arg-sparql-format
     "elot_sparql" "elot_sparql_select")
    (elot-gptel--arg-sparql-limit
     "elot_sparql" "elot_sparql_select")
    (elot-gptel--arg-limit
     "elot_resources" "elot_read_resource"))
  "Map of canonical arg-fragment symbol -> tools expected to share it (`eq').")

(ert-deftest elot-gptel-registry-test-shared-args-are-eq ()
  "Specs sharing a canonical arg reference the *same* fragment (`eq')."
  (dolist (entry elot-gptel-registry-test--shared-fragments)
    (let* ((sym (car entry))
           (tools (cdr entry))
           (fragment (symbol-value sym)))
      (dolist (tool tools)
        (let* ((spec (assoc tool (elot-gptel-registry-test--specs)))
               (args (plist-get (cdr spec) :args)))
          (unless spec
            (ert-fail (format "%s: tool not registered" tool)))
          (unless (memq fragment args)
            (ert-fail
             (format "%s: :args does not `eq'-share fragment %s"
                     tool sym))))))))

(ert-deftest elot-gptel-registry-test-truthy-coerces-json-false ()
  "`elot-gptel--truthy' maps the JSON-false sentinel to nil.
Regression guard for the bug where an explicit JSON `false'
(marshalled as `:json-false', which is non-nil in Elisp) made
every boolean tool argument -- notably `dry_run' -- read as true,
so mutators silently ran in dry-run mode and never edited the
file."
  (should (null (elot-gptel--truthy :json-false)))
  (should (null (elot-gptel--truthy "false")))
  (should (null (elot-gptel--truthy nil)))
  ;; genuinely-true values pass through untouched
  (should (eq t (elot-gptel--truthy t)))
  (should (equal "true" (elot-gptel--truthy "true"))))

(ert-deftest elot-gptel-registry-test-as-limit-coerces ()
  "`elot-gptel--as-limit' coerces float / numeric-string limits to int.
Regression guard for the bug where a caller-supplied `limit: 10'
was marshalled as a float (10.0) or a string (\"10\"), failed the
downstream `integerp' guard, and silently fell back to the tool
default (200)."
  ;; genuine integers pass through
  (should (= 10 (elot-gptel--as-limit 10)))
  ;; floats are truncated to int
  (should (= 10 (elot-gptel--as-limit 10.0)))
  (should (= 10 (elot-gptel--as-limit 10.7)))
  ;; numeric strings (with surrounding whitespace) are parsed
  (should (= 10 (elot-gptel--as-limit "10")))
  (should (= 10 (elot-gptel--as-limit " 10 ")))
  ;; omitted / non-numeric -> nil (tool applies its own default)
  (should (null (elot-gptel--as-limit nil)))
  (should (null (elot-gptel--as-limit "all")))
  (should (null (elot-gptel--as-limit :json-false))))

(provide 'elot-gptel-registry-test)
;;; elot-gptel-registry-test.el ends here
