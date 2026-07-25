;;; test-helper.el --- shared setup for ELOT ERT tests  -*- lexical-binding: t; -*-

;; Loaded via `-l ./test-helper.el' (see test/Makefile) before any
;; test file, so its settings apply to every batch run.
;;
;; Responsibilities:
;;
;;   1. Disable the optional DB sync in `elot-slurp-to-vars'.  The
;;      sync writes the parsed slurp into the user's global ELOT
;;      label DB (`elot-db-file'), which is fine for interactive use
;;      but pollutes the user's cache with rows from short-lived
;;      fixture files when running the test suite (every fixture
;;      heading ends up in the DB indexed by a path that no longer
;;      exists once the test deletes its temp file).
;;
;;   2. Redirect `elot-db-file' to a per-process throwaway path, as a
;;      belt-and-braces guard against any other writer (direct calls
;;      to `elot-db-update-source', `elot-source--do-register', ...).
;;      Tests that need their own DB still rebind `elot-db-file' via
;;      `let' or `cl-letf'; this only sets a safer default.
;;
;; Both settings are made via `setq-default' before elot-db.el is
;; loaded.  `defcustom' respects an existing top-level value (it does
;; not overwrite when the variable is already bound), so the settings
;; stick once the package itself eventually `require's elot-db.
;; `setq-default' (rather than plain `setq') makes sure that per-test
;; `let' bindings around these dynamic variables shadow cleanly and
;; unwind to the helper's value, not to a vanished local.
;;
;; Recommended opt-out pattern for tests that genuinely exercise the
;; slurp -> DB write path:
;;
;;   (ert-deftest my-db-roundtrip-test ()
;;     (let ((elot-db-sync-on-slurp t)
;;           (elot-db-file (make-temp-file "my-test-db-" nil ".sqlite")))
;;       (unwind-protect
;;           (progn
;;             ;; ... exercises slurp -> DB ...
;;             )
;;         (when (file-exists-p elot-db-file)
;;           (delete-file elot-db-file)))))

;; (1) Suppress slurp-driven DB writes.  See `elot-db-sync-on-slurp'
;; in elot-db.el for the user-facing documentation.
(setq-default elot-db-sync-on-slurp nil)

;; (2) Redirect the global DB path to a temp file.  Generated lazily
;; with a `delete-on-exit'-style hook so a clean test run leaves no
;; .sqlite files behind in `temporary-file-directory'.
(let ((tmp (make-temp-file "elot-test-db-" nil ".sqlite")))
  (setq-default elot-db-file tmp)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (and (stringp tmp) (file-exists-p tmp))
                (ignore-errors (delete-file tmp))))))

;; (3) General-purpose fixture-copy helper.  Live acceptance tests
;; want to exercise the *real* pipeline (ROBOT lint / OMN-validate /
;; reasoner) against a genuine ELOT ontology, but must never mutate
;; the checked-in fixture.  This macro copies a fixture file into a
;; throwaway temp file, binds PATH-VAR to that copy, runs BODY, and
;; deletes the copy (and kills any buffer visiting it) on the way
;; out -- regardless of how BODY exits.
;;
;; Nothing here is gptel-specific; any test type (lint, reasoning,
;; sparql, ...) that needs a mutable, real on-disk ontology can use
;; it.  The canonical, test-owned pets ontology lives at
;; `test/fixtures/pets.org' (a snapshot decoupled from the
;; user-facing `examples/pets.org', which may change for
;; pedagogical reasons).
;;
;; FIXTURE is resolved relative to the test/ directory (the
;; directory that holds this helper).  Example:
;;
;;   (elot-test-with-fixture-copy path "fixtures/pets.org"
;;     (let ((elot-gptel-allow-side-effects t))
;;       ... run a mutating tool against PATH, assert on disk ...))

(defvar elot-test--helper-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Absolute path of the test/ directory (where test-helper.el lives).")

(defun elot-test-fixture-path (relative)
  "Return the absolute path of RELATIVE under the test/ directory."
  (expand-file-name relative elot-test--helper-dir))

(defmacro elot-test-with-fixture-copy (path-var fixture &rest body)
  "Copy FIXTURE to a temp file, bind PATH-VAR to it, run BODY.

FIXTURE is a path relative to the test/ directory (e.g.
\"fixtures/pets.org\").  The copy is created in
`temporary-file-directory' with the same extension as FIXTURE,
so mode detection (org-mode etc.) still works.  On exit the copy
is deleted and any buffer visiting it is killed (modified flag
cleared first, so no interactive save prompt under --batch),
whether BODY returns normally or non-locally."
  (declare (indent 2))
  (let ((src (make-symbol "src"))
        (ext (make-symbol "ext")))
    `(let* ((,src (elot-test-fixture-path ,fixture))
            (,ext (file-name-extension ,src t))
            (,path-var (make-temp-file "elot-fixture-" nil ,ext)))
       (copy-file ,src ,path-var 'ok-if-already-exists)
       (unwind-protect
           (progn ,@body)
         (dolist (b (buffer-list))
           (when (and (buffer-file-name b)
                      (string= (file-truename (buffer-file-name b))
                               (file-truename ,path-var)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (when (file-exists-p ,path-var)
           (ignore-errors (delete-file ,path-var)))))))

;; (4) Shared ROBOT skip/fail gate.
;;
;; Many test files exercise the live ROBOT pipeline (lint /
;; OMN-validate / reasoning / sparql / diff).  When ROBOT is absent
;; these tests `skip-unless' cleanly so the default `make test' stays
;; green on machines (and CI runners) without Java/ROBOT installed.
;;
;; The ROBOT CI job (Step 4.3) runs the *same* suite with
;; ELOT_REQUIRE_ROBOT=1 set in the environment.  In that mode a
;; missing ROBOT is a hard failure instead of a skip, so a broken or
;; mis-provisioned ROBOT install fails loudly rather than silently
;; skipping the bucket-3 tests.  There is no separate `test-robot'
;; target list: the gate is purely this env flag, so no test names are
;; duplicated.
;;
;; ROBOT-gated test files should call
;; `elot-test-robot-skip-unless-available' at the top of each live
;; test (or from their local `--live-or-skip' helper) instead of
;; calling `ert-skip' directly.

(defun elot-test-require-robot-p ()
  "Non-nil when ELOT_REQUIRE_ROBOT=1 (ROBOT-gated tests must not skip)."
  (equal (getenv "ELOT_REQUIRE_ROBOT") "1"))

(defun elot-test-robot-skip-unless-available ()
  "Skip the current test unless ROBOT is available.

When ELOT_REQUIRE_ROBOT=1 a missing ROBOT is a hard failure
instead of a skip, so CI's ROBOT job fails loudly."
  (when (fboundp 'elot-robot-reset-cache)
    (ignore-errors (elot-robot-reset-cache)))
  (unless (and (fboundp 'elot-robot-available-p)
               (ignore-errors (elot-robot-available-p)))
    (if (elot-test-require-robot-p)
        (error "ELOT_REQUIRE_ROBOT=1 but ROBOT is not available: \
set `elot-robot-jar-path' or install `robot'")
      (ert-skip
       "ROBOT not available; set `elot-robot-jar-path' or install `robot'"))))

(provide 'test-helper)
;;; test-helper.el ends here
