;;; gen-manual-reference.el --- Auto-generate manual reference chapters  -*- lexical-binding: t; -*-

;; Run via:
;;   emacs --batch -L elot-package -l documentation/manual/gen-manual-reference.el
;;
;; Or, from the top-level Makefile:
;;   make manual-reference
;;
;; Produces, under documentation/manual/reference/:
;;   commands.org        - every interactive elot-* command + key + docstring
;;   customization.org   - every defcustom in group `elot' (and sub-groups)
;;   lob-helpers.org     - every named source block in elot-lob.org
;;
;; The "TODO --- run make manual-reference to populate" stubs in each
;; file are replaced between AUTO-GENERATED-BEGIN/END markers.

(require 'cl-lib)
(require 'subr-x)

(defun elot-manual--first-doc-line (sym)
  "Return the first line of SYM's docstring, or empty string."
  (let ((doc (or (and (fboundp sym) (ignore-errors (documentation sym)))
                 (documentation-property sym 'variable-documentation)
                 "")))
    (car (split-string (or doc "") "\n"))))

(defun elot-manual--key-for (cmd)
  "Return where CMD is bound globally or in `elot-mode-map', as a string."
  (let* ((maps (delq nil (list (and (boundp 'elot-mode-map) elot-mode-map)
                               (current-global-map))))
         (keys (cl-loop for m in maps
                        for k = (where-is-internal cmd (list m) t)
                        when k return k)))
    (if keys (key-description keys) "")))

(defun elot-manual--collect-elot-symbols (pred)
  (let (out)
    (mapatoms (lambda (s)
                (when (and (string-prefix-p "elot-" (symbol-name s))
                           (funcall pred s))
                  (push s out))))
    (sort out (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defun elot-manual--replace-region (file body)
  "Replace the AUTO-GENERATED-BEGIN/END region in FILE with BODY."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (if (re-search-forward "^# AUTO-GENERATED-BEGIN$" nil t)
        (let ((begin (1+ (line-end-position))))
          (re-search-forward "^# AUTO-GENERATED-END$")
          (delete-region begin (line-beginning-position))
          (goto-char begin)
          (insert body "\n"))
      (error "Markers not found in %s" file))
    (write-region (point-min) (point-max) file)))

;;; --- Commands ---------------------------------------------------------

(defun elot-manual-gen-commands ()
  (let* ((cmds (elot-manual--collect-elot-symbols
                (lambda (s) (and (fboundp s) (commandp s)))))
         (lines
          (cons
           "| Command | Key | Description |\n|---------+-----+-------------|"
           (mapcar
            (lambda (c)
              (format "| =%s= | =%s= | %s |"
                      (symbol-name c)
                      (elot-manual--key-for c)
                      (elot-manual--first-doc-line c)))
            cmds))))
    (string-join lines "\n")))

;;; --- Customization ----------------------------------------------------

(defun elot-manual-gen-customization ()
  (let* ((opts (elot-manual--collect-elot-symbols
                (lambda (s) (custom-variable-p s))))
         (lines
          (cons
           "| Variable | Default | Group | Description |\n|----------+---------+-------+-------------|"
           (mapcar
            (lambda (v)
              (format "| =%s= | =%S= | =%s= | %s |"
                      (symbol-name v)
                      (ignore-errors (default-toplevel-value v))
                      (or (get v 'custom-group) "elot")
                      (elot-manual--first-doc-line v)))
            opts))))
    (string-join lines "\n")))

;;; --- LOB helpers ------------------------------------------------------

(defun elot-manual-gen-lob (lob-file)
  (with-temp-buffer
    (insert-file-contents lob-file)
    (let (entries)
      (goto-char (point-min))
      (while (re-search-forward "^#\\+name:[ \t]+\\([^\n]+\\)$" nil t)
        (let ((name (match-string 1))
              (doc ""))
          (save-excursion
            (forward-line -1)
            (when (looking-at "^#\\+\\(?:caption\\|description\\):[ \t]+\\(.*\\)$")
              (setq doc (match-string 1))))
          (push (cons name doc) entries)))
      (string-join
       (cons "| Block | Description |\n|-------+-------------|"
             (mapcar (lambda (e)
                       (format "| =%s= | %s |" (car e) (cdr e)))
                     (nreverse entries)))
       "\n"))))

;;; --- Driver -----------------------------------------------------------

(defun elot-manual-generate ()
  (require 'elot)
  (let* ((root (file-name-directory (or load-file-name buffer-file-name default-directory)))
         (ref  (expand-file-name "reference/" root))
         (lob  (expand-file-name "../../elot-package/elot-lob.org" root)))
    (elot-manual--replace-region
     (expand-file-name "commands.org" ref)
     (elot-manual-gen-commands))
    (elot-manual--replace-region
     (expand-file-name "customization.org" ref)
     (elot-manual-gen-customization))
    (when (file-exists-p lob)
      (elot-manual--replace-region
       (expand-file-name "lob-helpers.org" ref)
       (elot-manual-gen-lob lob)))
    (message "Manual reference regenerated.")))

(elot-manual-generate)

;;; gen-manual-reference.el ends here
