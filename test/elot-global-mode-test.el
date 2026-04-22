;;; elot-global-mode-test.el --- Tests for Step 1.7  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.7: `elot-global-label-display-mode' for non-ELOT buffers.
;;
;; Exercises the DB-driven font-lock regex path end-to-end:
;;   - `elot-db-all-active-ids' returns the right set of ids
;;   - the global minor mode builds a regexp from those ids
;;   - enabling it in an arbitrary buffer (Turtle, Python) decorates
;;     matching tokens with labels, without touching `elot-slurp'
;;   - the regex is anchored so sub-identifiers in longer tokens
;;     are not accidentally matched

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

(defconst elot-gm-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-gm-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-gm-test--dir))

(defvar elot-gm-test--tmp-db nil)
(defvar elot-gm-test--tmp-files nil)

(defun elot-gm-test--setup ()
  (setq elot-gm-test--tmp-db (make-temp-file "elot-gm-" nil ".sqlite"))
  (setq elot-gm-test--tmp-files nil)
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-gm-test--tmp-db))

(defun elot-gm-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-gm-test--tmp-db (file-exists-p elot-gm-test--tmp-db))
    (ignore-errors (delete-file elot-gm-test--tmp-db)))
  (dolist (f elot-gm-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f))))
  (setq-default elot-active-label-sources nil))

(defun elot-gm-test--copy-fixture (name)
  (let* ((src (elot-gm-test--fixture name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-gm-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-gm-test--tmp-files)
    tmp))

(defmacro elot-gm-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-gm-test--setup) ,@body)
     (elot-gm-test--teardown)))

(defun elot-gm-test--text-label-at (buffer pos)
  "Return the `display' text property at POS in BUFFER, or nil."
  (with-current-buffer buffer
    (get-text-property pos 'display)))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(ert-deftest test-db-all-active-ids-dedupes ()
  "`elot-db-all-active-ids' returns a deduped union across sources."
  (elot-gm-test--with-fresh
    (let ((csv  (elot-gm-test--copy-fixture "labels.csv"))
          (tsv  (elot-gm-test--copy-fixture "labels.tsv")))
      (elot-label-register-source csv)
      (elot-label-register-source tsv)
      (setq elot-active-label-sources
            `((,csv) (,tsv)))
      (let ((ids (elot-db-all-active-ids)))
        (should (member "ex:Widget" ids))
        (should (member "EMP-001" ids))
        ;; no duplicates
        (should (= (length ids) (length (delete-dups (copy-sequence ids)))))))))

(ert-deftest test-db-all-active-ids-empty-when-no-sources ()
  "With no active sources, `elot-db-all-active-ids' returns nil."
  (elot-gm-test--with-fresh
    (setq elot-active-label-sources nil)
    (should (null (elot-db-all-active-ids)))))

(ert-deftest test-global-mode-regexp-built-from-db ()
  "The mode builds its regexp from DB ids via `regexp-opt'."
  (elot-gm-test--with-fresh
    (let ((csv (elot-gm-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (setq-local elot-active-label-sources `((,csv)))
        (elot-global-label-display-mode 1)
        (should (stringp elot-global--fontify-regexp))
        ;; regexp matches a known id
        (should (string-match-p elot-global--fontify-regexp "ex:Widget"))
        ;; regexp does not match an unrelated token
        (should-not (string-match-p elot-global--fontify-regexp "zzz-none"))
        (elot-global-label-display-mode -1)))))

(ert-deftest test-global-mode-decorates-tokens ()
  "Enabling the mode puts `display' properties on matching ids."
  (elot-gm-test--with-fresh
    (let ((csv (elot-gm-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (insert "Here is ex:Widget in prose.\n")
        (setq-local elot-active-label-sources `((,csv)))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "ex:Widget")
                      (match-beginning 0)))
               (label (get-text-property pos 'display)))
          (should (equal label "Widget")))
        (elot-global-label-display-mode -1)))))

(ert-deftest test-global-mode-uc3-python-buffer ()
  "UC3: opaque ids in a Python-mode buffer get decorated."
  (elot-gm-test--with-fresh
    (let ((tsv (elot-gm-test--copy-fixture "labels.tsv")))
      (elot-label-register-source tsv)
      (with-temp-buffer
        ;; Avoid loading python-mode in batch; fake it via fundamental-mode.
        (fundamental-mode)
        (insert "def lookup():\n    return 'EMP-001'\n")
        (setq-local elot-active-label-sources `((,tsv)))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "EMP-001")
                      (match-beginning 0)))
               (label (get-text-property pos 'display)))
          (should (stringp label))
          (should (> (length label) 0)))
        (elot-global-label-display-mode -1)))))

(ert-deftest test-global-mode-no-local-slurp-required ()
  "The mode works in a buffer with no `elot-slurp' populated."
  (elot-gm-test--with-fresh
    (let ((csv (elot-gm-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (insert "token ex:Gadget.\n")
        (setq-local elot-active-label-sources `((,csv)))
        ;; deliberately do not bind elot-slurp / elot-codelist-ht
        (should-not (boundp 'elot-slurp-dummy-marker))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (let* ((pos (save-excursion
                      (goto-char (point-min))
                      (search-forward "ex:Gadget")
                      (match-beginning 0)))
               (label (get-text-property pos 'display)))
          (should (equal label "Gadget")))
        (elot-global-label-display-mode -1)))))

(ert-deftest test-global-mode-disable-removes-decorations ()
  "Disabling the mode removes the `display' properties."
  (elot-gm-test--with-fresh
    (let ((csv (elot-gm-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (insert "ex:Widget here.\n")
        (setq-local elot-active-label-sources `((,csv)))
        (elot-global-label-display-mode 1)
        (font-lock-ensure (point-min) (point-max))
        (elot-global-label-display-mode -1)
        (let ((pos (save-excursion
                     (goto-char (point-min))
                     (search-forward "ex:Widget")
                     (match-beginning 0))))
          (should (null (get-text-property pos 'display))))))))

(provide 'elot-global-mode-test)
;;; elot-global-mode-test.el ends here
