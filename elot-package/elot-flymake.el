;;; elot-flymake.el --- Flymake backend for ELOT lint checks  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides in-buffer squiggly-line diagnostics for ELOT ontology
;; buffers by bridging the existing `org-lint' checkers (registered in
;; `elot-lint.el') to Emacs's built-in Flymake framework.
;;
;; When `elot-mode' is active, Flymake runs the ELOT lint checkers in
;; the background and underlines problems directly in the buffer —
;; errors in red, warnings in yellow — just like the "squiggly lines"
;; in VS Code.
;;
;; The `org-lint' list view (M-x elot-org-lint) continues to work as
;; before; this module adds the *complementary* in-place display.
;;
;; Usage:
;;   ;; Automatically enabled by elot-mode.  To use standalone:
;;   (add-hook 'elot-mode-hook #'elot-flymake-setup)

;;; Code:

(require 'flymake)
(require 'org-lint)

;; Require elot-lint to ensure all ELOT org-lint checkers are registered
;; before the Flymake backend tries to call them.
(require 'elot-lint)

(defcustom elot-flymake-checkers
  '(elot/nodeclare-id-prefix-label
    elot/ontology-header
    elot/prefix-table
    elot/ontology-presence
    elot/required-sections
    elot/description-list-curies
    elot/axiom-value-curies
    elot/omn-syntax)
  "List of `org-lint' checker names to run via Flymake.
These must match the first argument to `org-lint-add-checker' in
`elot-lint.el'."
  :type '(repeat symbol)
  :group 'elot)

(defun elot-flymake--checker-by-name (name)
  "Return the `org-lint--checker' struct whose name is NAME, or nil."
  (seq-find (lambda (c)
              (eq (org-lint-checker-name c) name))
            org-lint--checkers))

(defun elot-flymake--classify-message (text)
  "Return a Flymake diagnostic type for message TEXT.
Looks for \"ERROR:\" or \"WARNING:\" prefixes (possibly hidden
inside text properties)."
  (let ((plain (substring-no-properties text)))
    (cond
     ((string-match-p "\\`ERROR:" plain)   :error)
     ((string-match-p "\\`WARNING:" plain) :warning)
     ((string-match-p "ERROR"  plain)      :error)
     ((string-match-p "WARNING" plain)     :warning)
     (t                                     :note))))

(defun elot-flymake--run-checkers (report-fn &rest _args)
  "Flymake backend function for ELOT lint checks.
Runs every checker listed in `elot-flymake-checkers' on the
current buffer's parsed Org tree, converts the resulting
\(POSITION MESSAGE) pairs into Flymake diagnostics, and passes
them to REPORT-FN."
  (condition-case err
      (let* ((tree (org-element-parse-buffer))
             (diags '()))
        (dolist (checker-name elot-flymake-checkers)
          (when-let* ((checker (elot-flymake--checker-by-name checker-name))
                      (fn      (org-lint-checker-function checker)))
            (condition-case checker-err
                (let ((hits (save-excursion (funcall fn tree))))
                  (dolist (hit hits)
                    (let* ((pos  (nth 0 hit))
                           (msg  (nth 1 hit))
                           (type (elot-flymake--classify-message msg))
                           ;; Region: from pos to end of that line
                           (beg  (save-excursion
                                   (goto-char (min pos (point-max)))
                                   (line-beginning-position)))
                           (end  (save-excursion
                                   (goto-char (min pos (point-max)))
                                   (line-end-position))))
                      (push (flymake-make-diagnostic
                             (current-buffer) beg end type
                             (substring-no-properties msg))
                            diags))))
              (error
               (message "elot-flymake: checker %s signalled: %S"
                        checker-name checker-err)))))
        (funcall report-fn (nreverse diags)))
    (error
     (message "elot-flymake: backend error: %S" err)
     (funcall report-fn nil))))

;;;###autoload
(defun elot-flymake-setup ()
  "Enable the ELOT Flymake backend in the current buffer.
Adds `elot-flymake--run-checkers' to `flymake-diagnostic-functions'
and enables `flymake-mode' if it is not already active."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'elot-flymake--run-checkers nil t)
  (unless flymake-mode
    (flymake-mode 1)))

(defun elot-flymake-teardown ()
  "Disable the ELOT Flymake backend in the current buffer."
  (remove-hook 'flymake-diagnostic-functions #'elot-flymake--run-checkers t)
  (when flymake-mode
    (flymake-mode -1)))

(provide 'elot-flymake)
;;; elot-flymake.el ends here
