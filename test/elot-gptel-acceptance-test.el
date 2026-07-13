;;; elot-gptel-acceptance-test.el --- Live pets.org acceptance tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l ./test-helper.el -l elot-gptel-acceptance-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-V1-PLAN.org Step 2.2 follow-up (b) -- live end-to-end
;; acceptance tests for the resource-mutation family, driven against
;; a real, ROBOT-consistent ELOT ontology (`test/fixtures/pets.org').
;;
;; Unlike the per-tool unit suites -- which build synthetic fixtures
;; and STUB the lint / OMN-validate / rename stages -- these tests run
;; the *real* pipeline (actual ROBOT lint, OMN parse, reasoner) over a
;; throwaway COPY of the fixture, so nothing on disk that is checked in
;; ever changes.  The copy-to-temp plumbing lives in
;; `elot-test-with-fixture-copy' (test-helper.el); the canonical,
;; test-owned ontology snapshot is `test/fixtures/pets.org' (decoupled
;; from the user-facing `examples/pets.org').
;;
;; ROBOT policy (per project decision): these live tests assume ROBOT
;; is on PATH / configured.  When it is not, they SKIP cleanly rather
;; than fail, mirroring the other live suites.
;;
;; First users -- the two `elot_replace_with_parent' cases that
;; exercise the tool you flagged as most innovative / most fragile:
;;
;;   1. COVARIANT (safe) fold: `ex:asianElephant' -> `ex:elephant'.
;;      A pure leaf with a single heading-nesting parent and no other
;;      occurrences; folding it is a genuine semantic weakening and the
;;      ontology stays consistent -- `elot_check' remains green.
;;
;;   1b. COVARIANT (safe) fold of a NON-LEAF: `ex:elephant' -> `ex:animal'.
;;      An internal node WITH a subclass (`ex:asianElephant') and its own
;;      parent axiom; folding a subtree is still a safe weakening and the
;;      former subclass must survive with the ontology still consistent.
;;
;;   2. CONTRAVARIANT (unsafe) fold: `ex:dog' -> `ex:animal'.
;;      `ex:dog' occurs in `DisjointClasses :: ex:dog, ex:cat' (a
;;      contravariant / negative context).  Folding dog into its parent
;;      rewrites that to `DisjointClasses :: ex:animal, ex:cat'; since
;;      `ex:cat' is itself a subclass of `ex:animal', `ex:cat' becomes
;;      unsatisfiable.  The tool's own lint+OMN revalidation does NOT
;;      see this, but the fold now runs `elot_consistency' by default
;;      and folds a WARNING into the OK envelope -- so the regression is
;;      surfaced automatically rather than only by a manual follow-up.
;;
;;   3. CONTRAVARIANT fold -> UNSATISFIABLE class: fold `ex:watercraft'
;;      into `ex:vehicle' (disjoint sibling `ex:landVehicle' becomes
;;      unsatisfiable, but no individual is in it, so the ontology stays
;;      consistent).  Exercises the coda's `unsatisfiable class(es)'
;;      WARNING branch.
;;
;;   4. CONTRAVARIANT fold -> INCONSISTENT ontology: fold
;;      `ex:landVehicle' into `ex:vehicle' while `ex:titanic' is a member
;;      of the now-unsatisfiable `ex:watercraft', making the whole
;;      ontology inconsistent.  Exercises the coda's INCONSISTENT
;;      WARNING branch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'test-helper)

(defvar elot-gptel-acceptance-test--repo-root nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-acceptance-test--repo-root repo-root))

(require 'elot-tangle)
(require 'elot-gptel)
(require 'elot-robot)


;;; ---------------------------------------------------------------------------
;;; Live-or-skip guard
;;; ---------------------------------------------------------------------------

(defun elot-gptel-acceptance-test--live-or-skip ()
  "Skip the current test unless ROBOT is available."
  (when (fboundp 'elot-robot-reset-cache)
    (ignore-errors (elot-robot-reset-cache)))
  (unless (and (fboundp 'elot-robot-available-p)
               (ignore-errors (elot-robot-available-p)))
    (ert-skip
     "ROBOT not available; set `elot-robot-jar-path' or install `robot'")))


;;; ---------------------------------------------------------------------------
;;; Invocation helper
;;; ---------------------------------------------------------------------------
;;
;; The mutation tools resolve FILE against the project root and refuse
;; paths outside it.  A temp copy under `temporary-file-directory' is
;; outside the ELOT repo, so we bind `default-directory' to the copy's
;; own directory: with no enclosing project there, `project-current'
;; returns nil and `elot-gptel--project-root' falls back to
;; `default-directory', making the copy's directory the effective root.
;; Tools are then called with the bare file name.

(defmacro elot-gptel-acceptance-test--in-copy (path-var name-var &rest body)
  "Copy pets.org to a temp file (PATH-VAR), bind NAME-VAR to its base
name, `default-directory' to its directory, and run BODY with the
side-effects gate armed."
  (declare (indent 2))
  `(elot-test-with-fixture-copy ,path-var "fixtures/pets.org"
     (let* ((,name-var (file-name-nondirectory ,path-var))
            (default-directory (file-name-directory ,path-var))
            (elot-gptel-allow-side-effects t))
       ,@body)))

(defun elot-gptel-acceptance-test--slurp (path)
  "Return the current on-disk contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))


;;; ---------------------------------------------------------------------------
;;; 1. Covariant (safe) fold -- ex:asianElephant -> ex:elephant
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-acceptance-test-replace-covariant-stays-consistent ()
  "Folding a leaf with only covariant occurrences is a true semantic
weakening: the tool returns OK and a full `elot_check' on the
mutated file remains green."
  (elot-gptel-acceptance-test--live-or-skip)
  (elot-gptel-acceptance-test--in-copy path name
    ;; Baseline: the pristine copy must itself pass `elot_check',
    ;; otherwise the post-fold assertion proves nothing.
    (let ((baseline (elot-gptel-tool-check name nil nil "hermit")))
      (should (string-match-p "all checks pass" baseline)))
    (let ((out (elot-gptel-tool-replace-with-parent
                name "ex:asianElephant")))
      (should (stringp out))
      (should (string-prefix-p "OK: merged ex:asianElephant into ex:elephant"
                               out))
      (should (string-match-p "NOTE: ex:asianElephant folded into ex:elephant"
                              out))
      ;; The subject heading is gone; the parent survives.
      (let ((after (elot-gptel-acceptance-test--slurp path)))
        (should-not (string-match-p "ex:asianElephant" after))
        (should (string-match-p "ex:elephant" after)))
      ;; The real payoff: the mutated ontology is still fully clean.
      (let ((check (elot-gptel-tool-check name nil nil "hermit")))
        (should (string-match-p "all checks pass" check))))))


;;; ---------------------------------------------------------------------------
;;; 1b. Covariant (safe) fold of a NON-LEAF -- ex:elephant -> ex:animal
;;; ---------------------------------------------------------------------------
;;
;; Unlike `ex:asianElephant' (a pure leaf), `ex:elephant' is an
;; *internal* node: it has a heading-nested subclass (`ex:asianElephant')
;; and a `SubClassOf :: ex:isAfraidOf some ex:mouse' axiom.  Folding it
;; into its parent `ex:animal' must (i) preserve the surviving subclass,
;; whose own parent axiom weakens from `... elephant' to `... animal',
;; and (ii) leave the ontology consistent.  This exercises the tool on a
;; subtree, not just a leaf.

(ert-deftest elot-gptel-acceptance-test-replace-nonleaf-covariant-stays-consistent ()
  "Folding an internal node (a class WITH subclasses) into its parent
is still a safe covariant weakening: the tool returns OK, the former
subclass survives (now rooted at the parent), and a full `elot_check'
on the mutated file remains green."
  (elot-gptel-acceptance-test--live-or-skip)
  (elot-gptel-acceptance-test--in-copy path name
    (let ((baseline (elot-gptel-tool-check name nil nil "hermit")))
      (should (string-match-p "all checks pass" baseline)))
    (let ((out (elot-gptel-tool-replace-with-parent name "ex:elephant")))
      (should (stringp out))
      (should (string-prefix-p "OK: merged ex:elephant into ex:animal" out))
      (should (string-match-p "NOTE: ex:elephant folded into ex:animal" out))
      (let ((after (elot-gptel-acceptance-test--slurp path)))
        ;; The subject is gone ...
        (should-not (string-match-p "ex:elephant" after))
        ;; ... but its former subclass survives.
        (should (string-match-p "ex:asianElephant" after)))
      ;; The whole subtree still reasons cleanly.
      (let ((check (elot-gptel-tool-check name nil nil "hermit")))
        (should (string-match-p "all checks pass" check))))))


;;; ---------------------------------------------------------------------------
;;; 2. Contravariant (unsafe) fold -- ex:dog -> ex:animal
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-acceptance-test-replace-contravariant-commits-unsafely ()
  "Regression lock for the documented covariance weakness (follow-up
(a)), now with the default post-fold consistency check (follow-up
(c)).  `ex:dog' appears in `DisjointClasses :: ex:dog, ex:cat', a
contravariant context.  Folding it into `ex:animal' makes `ex:cat'
unsatisfiable.  The tool's lint+OMN-only revalidation does NOT catch
it, so the fold COMMITS -- but the default `elot_consistency' coda
now surfaces the regression as a WARNING in the OK envelope.  An
independent `elot_unsatisfiable' run confirms the same."
  (elot-gptel-acceptance-test--live-or-skip)
  (elot-gptel-acceptance-test--in-copy path name
    ;; Baseline copy is consistent (no unsatisfiable classes).
    (let ((base-unsat (elot-gptel-tool-unsatisfiable name "hermit")))
      (should (string-match-p "no unsatisfiable classes" base-unsat)))
    (let ((out (elot-gptel-tool-replace-with-parent name "ex:dog")))
      (should (stringp out))
      ;; (i) The fold committed -- lint+OMN revalidation let it through.
      (should (string-prefix-p "OK: merged ex:dog into ex:animal" out))
      ;; (ii) The default post-fold consistency check now CATCHES the
      ;;      regression and folds a WARNING into the OK envelope,
      ;;      shrinking the "promise gap" from a manual advisory to an
      ;;      automatic reasoner verdict.
      (should (string-match-p "NOTE: ex:dog folded into ex:animal" out))
      (should (string-match-p "WARNING: fold left" out))
      ;; (iii) The DisjointClasses row was rewritten onto ex:animal.
      ;;       (We assert the row specifically rather than global
      ;;       absence of ex:dog: stale ttl `#+RESULTS' blocks are not
      ;;       part of the live axiom surface and may retain the old
      ;;       token.)
      (let ((after (elot-gptel-acceptance-test--slurp path)))
        (should (string-match-p "DisjointClasses[^\n]*ex:animal" after))
        (should-not (string-match-p "DisjointClasses[^\n]*ex:dog" after)))
      ;; (iv) The regression the tool DID NOT catch: an independent
      ;;      reasoner run now reports an unsatisfiable class (or an
      ;;      outright inconsistency).
      (let ((unsat (elot-gptel-tool-unsatisfiable name "hermit")))
        (should (string-match-p "UNSATISFIABLE\\|INCONSISTENT" unsat))))))


;;; ---------------------------------------------------------------------------
;;; 3. Contravariant fold -> unsatisfiable class (no individual involved)
;;; ---------------------------------------------------------------------------
;;
;; `ex:vehicle' has two disjoint subclasses (`DisjointClasses ::
;; ex:landVehicle, ex:watercraft').  Folding `ex:watercraft' into its
;; parent `ex:vehicle' rewrites that row to `DisjointClasses ::
;; ex:landVehicle, ex:vehicle'; since `ex:landVehicle' is itself a
;; subclass of `ex:vehicle', `ex:landVehicle' becomes UNSATISFIABLE.
;; No individual is asserted into it, so the ontology stays consistent
;; overall -- the reasoner reports unsatisfiable classes but not an
;; outright inconsistency, exercising the coda's `unsatisfiable
;; class(es)' WARNING branch.

(ert-deftest elot-gptel-acceptance-test-replace-induces-unsatisfiable ()
  "Folding a disjoint sibling into its parent makes the other sibling
unsatisfiable.  The default post-fold consistency coda surfaces this
as a `WARNING: fold left unsatisfiable class(es)' line in the OK
envelope, and an independent `elot_unsatisfiable' run confirms it."
  (elot-gptel-acceptance-test--live-or-skip)
  (elot-gptel-acceptance-test--in-copy path name
    (let ((base-unsat (elot-gptel-tool-unsatisfiable name "hermit")))
      (should (string-match-p "no unsatisfiable classes" base-unsat)))
    (let ((out (elot-gptel-tool-replace-with-parent name "ex:watercraft")))
      (should (stringp out))
      (should (string-prefix-p "OK: merged ex:watercraft into ex:vehicle" out))
      (should (string-match-p "WARNING: fold left unsatisfiable class(es)" out))
      (let ((unsat (elot-gptel-tool-unsatisfiable name "hermit")))
        (should (string-match-p "UNSATISFIABLE\\|ex:landVehicle" unsat))))))


;;; ---------------------------------------------------------------------------
;;; 4. Contravariant fold -> outright INCONSISTENT ontology
;;; ---------------------------------------------------------------------------
;;
;; `ex:titanic' is asserted `Types :: ex:watercraft'.  Folding
;; `ex:landVehicle' into `ex:vehicle' rewrites the disjointness row to
;; `DisjointClasses :: ex:vehicle, ex:watercraft', making
;; `ex:watercraft' unsatisfiable.  Because `ex:titanic' is a member of
;; the now-unsatisfiable `ex:watercraft', the whole ontology becomes
;; INCONSISTENT -- exercising the coda's INCONSISTENT WARNING branch.

(ert-deftest elot-gptel-acceptance-test-replace-induces-inconsistency ()
  "Folding a disjoint sibling into its parent makes the other sibling
unsatisfiable, and because an individual is asserted into it the
ontology becomes outright INCONSISTENT.  The default post-fold
consistency coda surfaces this as a `WARNING: fold left the ontology
INCONSISTENT' line in the OK envelope."
  (elot-gptel-acceptance-test--live-or-skip)
  (elot-gptel-acceptance-test--in-copy path name
    (let ((base (elot-gptel-tool-consistency name)))
      (should (string-prefix-p "OK:" base)))
    (let ((out (elot-gptel-tool-replace-with-parent name "ex:landVehicle")))
      (should (stringp out))
      (should (string-prefix-p "OK: merged ex:landVehicle into ex:vehicle" out))
      (should (string-match-p "WARNING: fold left the ontology INCONSISTENT" out))
      (let ((verdict (elot-gptel-tool-consistency name)))
        (should (string-prefix-p "INCONSISTENT" verdict))))))


(provide 'elot-gptel-acceptance-test)
;;; elot-gptel-acceptance-test.el ends here
