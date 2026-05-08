;;; elot-load-without-sparql-test.el --- Load ELOT without optional SPARQL deps  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 4, Sub-step 4.2.B.
;;
;; Asserts the contract that ELOT loads and `elot-mode' is
;; available even when none of the optional external packages is
;; present.  The packages shadowed unavailable in the child Emacs
;; are: `htmlize', `omn-mode', `sparql-mode', `ob-sparql',
;; `ob-plantuml', `ht'.
;;
;; Approach: spawn a child `emacs --batch -Q' with our package
;; directory on `load-path', advise `require' to refuse the listed
;; packages (returning nil with NOERROR; signalling `file-missing'
;; otherwise), then `(require 'elot)' and print a status line.
;;
;; Asserts:
;;   1. The child exits with status 0.
;;   2. The output contains `elot-mode=t' (i.e. the symbol is
;;      bound after `(require 'elot)').
;;   3. `elot-missing-dependencies' records every blocked package.
;;
;; No ROBOT, no network.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst elot-lws-test--blocked
  '(htmlize omn-mode sparql-mode ob-sparql ob-plantuml ht)
  "Packages to shadow unavailable in the child Emacs.")

(defconst elot-lws-test--package-dir
  (expand-file-name "../elot-package"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun elot-lws-test--child-form ()
  "Elisp the child Emacs evaluates after -L PACKAGE_DIR."
  (let ((blocked elot-lws-test--blocked))
    `(progn
       (defvar elot-lws--blocked ',blocked)
       (advice-add
        'require :around
        (lambda (orig feature &optional filename noerror)
          (if (memq feature elot-lws--blocked)
              (if noerror
                  nil
                (signal 'file-missing
                        (list "Cannot open load file"
                              (symbol-name feature))))
            (funcall orig feature filename noerror))))
       (condition-case err
           (require 'elot)
         (error
          (princ (format "REQUIRE-FAILED: %S\n" err))
          (kill-emacs 2)))
       ;; Until Milestone 4 Step 4.6 makes `elot.el' require its
       ;; sibling modules, `elot-mode' is not pulled in by
       ;; `(require 'elot)'.  Load it explicitly so the contract
       ;; under test - "ELOT loads cleanly without the optional
       ;; SPARQL/PlantUML/htmlize/omn-mode deps" - is exercised
       ;; against the load path users actually take today.  Once
       ;; Step 4.6 lands, this extra `require' becomes redundant
       ;; and can be removed.
       (condition-case err
           (require 'elot-mode)
         (error
          (princ (format "REQUIRE-MODE-FAILED: %S\n" err))
          (kill-emacs 3)))
       (princ (format "elot-mode=%s\n" (fboundp 'elot-mode)))
       (princ (format "missing-deps=%S\n"
                      (and (boundp 'elot-missing-dependencies)
                           elot-missing-dependencies)))
       (kill-emacs 0))))

(ert-deftest elot-load-without-sparql/elot-loads-and-mode-bound ()
  "ELOT loads in a child Emacs with the optional deps shadowed away."
  (let* ((emacs (or (getenv "EMACS") "emacs"))
         (form  (elot-lws-test--child-form))
         (form-string (let ((print-length nil)
                            (print-level nil))
                        (prin1-to-string form)))
         (output-buf (generate-new-buffer " *elot-lws-out*"))
         exit-code)
    (unwind-protect
        (progn
          (setq exit-code
                (call-process emacs nil output-buf nil
                              "--batch" "-Q"
                              "-L" elot-lws-test--package-dir
                              "--eval" form-string))
          (let ((output (with-current-buffer output-buf
                          (buffer-string))))
            (should (equal exit-code 0))
            (should (string-match-p "elot-mode=t" output))
            ;; Each blocked pkg should appear as a whole word in
            ;; the missing-deps line.  Use a word-boundary regexp
            ;; so e.g. `ht' does not spuriously match `htmlize'.
            (dolist (pkg elot-lws-test--blocked)
              (let ((name (regexp-quote (symbol-name pkg))))
                (unless (eq pkg 'ht)
                  ;; `ht' is no longer in the optional-deps dolist
                  ;; (it was unused; cf. Step 4.4), so it will not
                  ;; appear in `elot-missing-dependencies' even
                  ;; when shadowed unavailable.  Skip the per-pkg
                  ;; assertion for it; the require-shadowing path
                  ;; is still exercised.
                  (should (string-match-p
                           (concat "\\(^\\|[^[:alnum:]-]\\)"
                                   name
                                   "\\($\\|[^[:alnum:]-]\\)")
                           output)))))))
      (kill-buffer output-buf))))

(provide 'elot-load-without-sparql-test)
;;; elot-load-without-sparql-test.el ends here
