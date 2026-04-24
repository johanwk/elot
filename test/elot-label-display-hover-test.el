;;; elot-label-display-hover-test.el --- Tests for Step 1.14 Tier 1  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.14 Tier 1: rich hover / eldoc summary for
;; `elot-global-label-display-mode'.
;;
;; Symptom addressed: in a non-ELOT buffer (e.g. `.ttl'), the
;; minibuffer showed only `id  label' on idle-hover because
;; `elot-global--build-keywords' installed a precomputed string as
;; the `help-echo' text property, shadowing the richer eldoc
;; backend.  Fix: install `help-echo' as a *function*
;; (`elot-global--help-echo') that calls `elot--summary-from-db'
;; lazily.  The matched id is stashed in the `elot-global--id'
;; text property so the lazy callback can recover it without
;; re-tokenising.
;;
;; Covers:
;;   1. After font-lock, the `help-echo' text property on a
;;      matched id is a function (specifically
;;      `elot-global--help-echo'), not a string.
;;   2. Calling that function with `(WINDOW OBJECT POS)' returns
;;      a string that includes the rdf:type and the `[src: ...]'
;;      provenance marker drawn from `:source-origin'.
;;   3. The `elot-global--id' text property carries the id, so
;;      the callback works even when `bounds-of-thing-at-point'
;;      would misfire (defensive regression check).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

(defvar elot-hover-test--tmp-db nil)

(defun elot-hover-test--setup ()
  (setq elot-hover-test--tmp-db
        (make-temp-file "elot-hover-" nil ".sqlite"))
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-hover-test--tmp-db))

(defun elot-hover-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-hover-test--tmp-db
             (file-exists-p elot-hover-test--tmp-db))
    (ignore-errors (delete-file elot-hover-test--tmp-db)))
  (setq elot-hover-test--tmp-db nil)
  (setq-default elot-active-label-sources nil))

(defmacro elot-hover-test--with-fresh (&rest body)
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn (elot-hover-test--setup) ,@body)
     (elot-hover-test--teardown)))

(ert-deftest test-help-echo-is-function-and-returns-rich-summary ()
  "`help-echo' on a fontified token is `elot-global--help-echo' (a function),
and invoking it yields a rich one-line summary containing the
rdf:type and the `[src: ...]' provenance marker."
  (elot-hover-test--with-fresh
    (elot-db-update-source
     "src-hover" nil "csv"
     '(("ex:SHI_10082"
        "has subcontracted welder"
        ("rdf:type" "owl:ObjectProperty"
         "skos:definition" "Indicates that a weld was made by a subcontractor."))))
    (with-temp-buffer
      (fundamental-mode)
      (insert "        ex:SHI_10082  ex:Os_HGAFNEH5;\n")
      (setq-local elot-active-label-sources '(("src-hover" nil)))
      (let ((elot-global-label-display-show-source t))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "ex:SHI_10082")
                      (match-beginning 0)))
               (he  (get-text-property pos 'help-echo))
               (id  (get-text-property pos 'elot-global--id)))
          ;; 1. help-echo is a function, not a string.
          (should (functionp he))
          (should (eq he #'elot-global--help-echo))
          ;; 3. the id is stashed for the lazy callback.
          (should (equal id "ex:SHI_10082"))
          ;; 2. invoking the callback yields the rich summary.
          (let ((summary (funcall he nil (current-buffer) pos)))
            (should (stringp summary))
            (should (string-match-p "ex:SHI_10082"       summary))
            (should (string-match-p "has subcontracted welder" summary))
            (should (string-match-p "owl:ObjectProperty" summary))
            (should (string-match-p "\\[src: src-hover\\]" summary))))
        (elot-global-label-display-mode -1)))))

(provide 'elot-label-display-hover-test)
;;; elot-label-display-hover-test.el ends here
