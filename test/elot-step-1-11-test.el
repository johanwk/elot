;;; elot-step-1-11-test.el --- Tests for Step 1.11  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.11 polish for `elot-global-label-display-mode':
;;   1. one-shot "no active sources" hint when the mode is enabled
;;      in a buffer with `elot-active-label-sources' = nil;
;;   2. debounced: toggling off/on in the same buffer does not spam;
;;   3. silent when sources are present;
;;   4. collision resolution is highest-priority-wins (re-covers
;;      Step 1.2 through `elot-db-get-label-any');
;;   5. user-facing commands carry autoload cookies -- in-tree this
;;      is verified by asserting the cookie magic comment appears
;;      immediately above the defun in the tangled .el file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

(defconst elot-s111-test--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun elot-s111-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-s111-test--dir))

(defvar elot-s111-test--tmp-db nil)
(defvar elot-s111-test--tmp-files nil)

(defun elot-s111-test--setup ()
  (setq elot-s111-test--tmp-db (make-temp-file "elot-s111-" nil ".sqlite"))
  (setq elot-s111-test--tmp-files nil)
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-s111-test--tmp-db))

(defun elot-s111-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-s111-test--tmp-db (file-exists-p elot-s111-test--tmp-db))
    (ignore-errors (delete-file elot-s111-test--tmp-db)))
  (dolist (f elot-s111-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f))))
  (setq-default elot-active-label-sources nil))

(defun elot-s111-test--copy-fixture (name)
  (let* ((src (elot-s111-test--fixture name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-s111-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-s111-test--tmp-files)
    tmp))

(defmacro elot-s111-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-s111-test--setup) ,@body)
     (elot-s111-test--teardown)))

(defmacro elot-s111-test--capture-messages (&rest body)
  "Run BODY and return messages emitted via `message' as a string.
The implementation temporarily rebinds `message' (via `cl-letf')
to a collecting shim, so it works under `emacs --batch' without
relying on `*Messages*' semantics."
  (declare (indent 0))
  `(let ((captured nil))
     (cl-letf (((symbol-function 'message)
                (lambda (&rest args)
                  (let ((s (apply #'format-message args)))
                    (push s captured)
                    s))))
       ,@body)
     (mapconcat #'identity (nreverse captured) "\n")))

;;; ---------------------------------------------------------------------------
;;; 1/2/3: no-sources messaging
;;; ---------------------------------------------------------------------------

(ert-deftest test-global-mode-messages-when-no-active-sources ()
  "Enabling the mode without active sources produces the hint message."
  (elot-s111-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources nil)
      (let ((output (elot-s111-test--capture-messages
                      (elot-global-label-display-mode 1))))
        (should (string-match-p
                 "no active label sources in this buffer"
                 output)))
      (elot-global-label-display-mode -1))))

(ert-deftest test-global-mode-idempotent-message ()
  "Toggling off/on without sources does not re-emit the hint."
  (elot-s111-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources nil)
      ;; First activation emits the hint.
      (elot-global-label-display-mode 1)
      (elot-global-label-display-mode -1)
      ;; Second activation in the same buffer: no new hint.
      (let ((output (elot-s111-test--capture-messages
                      (elot-global-label-display-mode 1))))
        (should-not (string-match-p
                     "no active label sources in this buffer"
                     output)))
      (elot-global-label-display-mode -1))))

(ert-deftest test-global-mode-silent-when-sources-present ()
  "With active sources present, no no-sources hint appears."
  (elot-s111-test--with-fresh
    (let ((csv (elot-s111-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (setq-local elot-active-label-sources `((,csv)))
        (let ((output (elot-s111-test--capture-messages
                        (elot-global-label-display-mode 1))))
          (should-not (string-match-p
                       "no active label sources in this buffer"
                       output)))
        (elot-global-label-display-mode -1)))))

;;; ---------------------------------------------------------------------------
;;; 4: collision resolution
;;; ---------------------------------------------------------------------------

(ert-deftest test-collision-highest-priority-wins ()
  "Two active sources with the same id: the first in the list wins."
  (elot-s111-test--with-fresh
    (elot-db-update-source "src-high" nil "csv"
                           '(("ex:foo" "HighLabel" nil)))
    (elot-db-update-source "src-low"  nil "csv"
                           '(("ex:foo" "LowLabel" nil)))
    ;; high listed first -- highest priority.
    (should (equal "HighLabel"
                   (elot-db-get-label-any
                    "ex:foo"
                    '(("src-high" nil) ("src-low" nil)))))
    ;; Reverse order -- the other label wins.
    (should (equal "LowLabel"
                   (elot-db-get-label-any
                    "ex:foo"
                    '(("src-low" nil) ("src-high" nil)))))))

;;; ---------------------------------------------------------------------------
;;; 5: autoload cookies appear above the documented commands
;;; ---------------------------------------------------------------------------

(defconst elot-s111-test--expected-autoloads
  '(("elot-label-display.el" . (elot-global-label-display-mode
                                 elot-global-label-display-setup))
    ("elot-sources.el"       . (elot-label-register-source
                                 elot-label-register-current-buffer
                                 elot-label-activate-source
                                 elot-label-deactivate-source
                                 elot-label-list-sources
                                 elot-label-list-active-sources)))
  "Files and commands that must carry ;;;###autoload cookies per Step 1.11.")

(defun elot-s111-test--file-path (basename)
  (let* ((here elot-s111-test--dir)
         (pkg  (expand-file-name "../elot-package/" here)))
    (expand-file-name basename pkg)))

(defun elot-s111-test--has-autoload-cookie-p (file fn-symbol)
  "Return non-nil if FILE has a ;;;###autoload cookie above FN-SYMBOL's defun."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((pattern
           (format "^;;;###autoload\n\\(?:(declare[^\n]*\n\\)?(\\(?:\\(?:cl-\\)?defun\\|define-minor-mode\\|defmacro\\)[ \t\n]+%s\\_>"
                   (regexp-quote (symbol-name fn-symbol)))))
      (re-search-forward pattern nil t))))

(ert-deftest test-autoload-cookies-present ()
  "Every command listed in `elot-s111-test--expected-autoloads' has a cookie."
  (dolist (entry elot-s111-test--expected-autoloads)
    (let* ((file (elot-s111-test--file-path (car entry))))
      (should (file-readable-p file))
      (dolist (sym (cdr entry))
        (should
         (or (elot-s111-test--has-autoload-cookie-p file sym)
             (error "Missing ;;;###autoload cookie for %s in %s"
                    sym (car entry))))))))

(ert-deftest test-autoload-commands-are-fboundp ()
  "After loading, every documented command is actually defined."
  (dolist (entry elot-s111-test--expected-autoloads)
    (dolist (sym (cdr entry))
      (should (fboundp sym)))))

(provide 'elot-step-1-11-test)
;;; elot-step-1-11-test.el ends here
