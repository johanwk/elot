;;; elot-test-helper-guard-test.el --- regression guard for test-helper.el  -*- lexical-binding: t; -*-

;; Verifies that `test-helper.el' is in effect for the current batch
;; run.  If a future refactor stops loading the helper (e.g. by
;; rewriting the Makefile recipe), this test fails loudly instead of
;; quietly letting fixture rows leak into the user's real
;; `elot-db-file'.

(require 'ert)

(ert-deftest test-helper-disables-slurp-db-sync ()
  "The helper must set `elot-db-sync-on-slurp' to nil by default."
  (should (boundp 'elot-db-sync-on-slurp))
  (should (null (default-value 'elot-db-sync-on-slurp))))

(ert-deftest test-helper-redirects-elot-db-file ()
  "The helper must redirect `elot-db-file' under `temporary-file-directory'."
  (should (boundp 'elot-db-file))
  (let ((path (default-value 'elot-db-file))
        (tmp (file-name-as-directory
              (expand-file-name temporary-file-directory))))
    (should (stringp path))
    (should (string-prefix-p tmp (expand-file-name path)))))

(provide 'elot-test-helper-guard-test)
;;; elot-test-helper-guard-test.el ends here
