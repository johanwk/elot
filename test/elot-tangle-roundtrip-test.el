;;; elot-tangle-roundtrip-test.el --- Tangle round-trip regression  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 1, Step 1.7 Verification:
;;
;; Guard against drift between the literate sources (`elot-defs.org',
;; `elot-mode.org') and the tangled `.el' files checked in under
;; `elot-package/'.  For each literate source we tangle it into a
;; fresh temporary directory and compare the produced `.el' file
;; byte-for-byte against the checked-in copy.
;;
;; If this test fails after a hand-edit to one of the `.el' files,
;; mirror the change in the corresponding `.org' source block and
;; re-run.  If it fails after editing the `.org' source, re-tangle
;; and commit the regenerated `.el'.

;;; Code:

(require 'ert)
(require 'ob-tangle)
(require 'org)

(defconst elot-tangle-roundtrip-test--repo-root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, derived from this test file's location.")

(defun elot-tangle-roundtrip-test--tangle-and-compare (org-rel el-rel)
  "Tangle ORG-REL into a temp dir and compare against EL-REL.
Both paths are relative to the repository root.  Returns nil on
match; signals an `ert-test-failed' (via `should') with a unified
diff in the message on mismatch."
  (let* ((root      elot-tangle-roundtrip-test--repo-root)
         (org-src   (expand-file-name org-rel root))
         (el-ref    (expand-file-name el-rel root))
         (tmpdir    (make-temp-file "elot-tangle-rt-" t))
         (org-copy  (expand-file-name (file-name-nondirectory org-src) tmpdir))
         (el-name   (file-name-nondirectory el-ref))
         (el-out    nil))
    (unwind-protect
        (progn
          (should (file-readable-p org-src))
          (should (file-readable-p el-ref))
          ;; Copy the .org source into the temp dir so that any
          ;; relative `:tangle ./elot-package/foo.el' header
          ;; resolves under the temp dir rather than overwriting
          ;; the checked-in file.  Mirror the directory layout
          ;; (elot-package/) used by the headers.
          (make-directory (expand-file-name "elot-package" tmpdir) t)
          (copy-file org-src org-copy t)
          (let ((default-directory (file-name-as-directory tmpdir))
                ;; Don't prompt, don't load anything from the user's
                ;; init, and keep tangling deterministic.
                (org-confirm-babel-evaluate nil)
                ;; The checked-in `.el' files contain noweb-reference
                ;; comments such as
                ;;   ;; [[file:../elot-defs.org::src-require][src-require]]
                ;; i.e. paths relative to the tangled output.  Setting
                ;; this to nil would force absolute paths, which on a
                ;; copy under a temp dir do not match the checked-in
                ;; file even when the source content is otherwise
                ;; identical.  Keep it t to preserve the relative form.
                (org-babel-tangle-use-relative-file-links t)
                (vc-handled-backends nil)
                ;; Force Unix (LF) line endings and UTF-8 on the
                ;; tangled output regardless of platform.  Without
                ;; this, on Windows (msys2 / native w32) tangle
                ;; writes CRLF while the checked-in `.el' files are
                ;; stored with LF, so a literal byte-for-byte compare
                ;; reports every line as different even though
                ;; `git status' shows no change (Git normalises on
                ;; commit/checkout).
                (coding-system-for-write 'utf-8-unix)
                (coding-system-for-read  'utf-8-unix))
            (with-current-buffer (find-file-noselect org-copy)
              (unwind-protect
                  (let ((inhibit-message t)
                        (message-log-max nil))
                    (org-babel-tangle))
                (kill-buffer (current-buffer)))))
          (setq el-out (expand-file-name (concat "elot-package/" el-name)
                                         tmpdir))
          (should (file-readable-p el-out))
          (let ((ref      (with-temp-buffer
                            (insert-file-contents-literally el-ref)
                            (buffer-string)))
                (produced (with-temp-buffer
                            (insert-file-contents-literally el-out)
                            (buffer-string))))
            (unless (string-equal ref produced)
              (let* ((diff-out (expand-file-name "tangle.diff" tmpdir))
                     (rc (call-process "diff" nil
                                       (list :file diff-out) nil
                                       "-u" el-ref el-out))
                     (diff (when (file-readable-p diff-out)
                             (with-temp-buffer
                               (insert-file-contents-literally diff-out)
                               (buffer-substring-no-properties
                                (point-min)
                                (min (point-max) (+ (point-min) 8000)))))))
                (ert-fail
                 (list (format "Tangled %s differs from checked-in %s (diff exit %s)"
                               org-rel el-rel rc)
                       :diff (or diff "<diff unavailable>")))))))
      (when (file-directory-p tmpdir)
        (delete-directory tmpdir t)))))

(ert-deftest elot-defs-tangle-roundtrip ()
  "Re-tangling `elot-defs.org' reproduces `elot-package/elot.el'."
  (elot-tangle-roundtrip-test--tangle-and-compare
   "elot-defs.org" "elot-package/elot.el"))

(ert-deftest elot-mode-tangle-roundtrip ()
  "Re-tangling `elot-mode.org' reproduces `elot-package/elot-mode.el'."
  (elot-tangle-roundtrip-test--tangle-and-compare
   "elot-mode.org" "elot-package/elot-mode.el"))

(provide 'elot-tangle-roundtrip-test)
;;; elot-tangle-roundtrip-test.el ends here
