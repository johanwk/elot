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

(provide 'test-helper)
;;; test-helper.el ends here
