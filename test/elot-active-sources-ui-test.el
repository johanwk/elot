;;; elot-active-sources-ui-test.el --- Tests for Step 1.7.2  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.7.2: priority UI for `elot-active-label-sources'.
;; Covers move-up/move-down, tabulated-list entry building, the
;; change-hook contract, global-mode auto-refresh, and auto-repersist
;; semantics (only when .dir-locals.el already has the entry).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)

(defconst elot-asui-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-asui-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-asui-test--dir))

(defvar elot-asui-test--tmp-db nil)
(defvar elot-asui-test--tmp-files nil)

(defun elot-asui-test--setup ()
  (setq elot-asui-test--tmp-db  (make-temp-file "elot-asui-" nil ".sqlite"))
  (setq elot-asui-test--tmp-files nil)
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-asui-test--tmp-db))

(defun elot-asui-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-asui-test--tmp-db (file-exists-p elot-asui-test--tmp-db))
    (ignore-errors (delete-file elot-asui-test--tmp-db)))
  (dolist (f elot-asui-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f)))))

(defun elot-asui-test--copy-fixture (name)
  (let* ((src (elot-asui-test--fixture name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-asui-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-asui-test--tmp-files)
    tmp))

(defmacro elot-asui-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-asui-test--setup) ,@body)
     (elot-asui-test--teardown)))

;;;; Move commands

(ert-deftest test-move-source-up-reorders ()
  "Moving entry #2 up yields [B A C]."
  (elot-asui-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources
                  '(("A" nil) ("B" nil) ("C" nil)))
      (elot-label-move-source-up "B" nil)
      (should (equal elot-active-label-sources
                     '(("B" nil) ("A" nil) ("C" nil)))))))

(ert-deftest test-move-source-down-reorders ()
  "Moving entry #1 down yields [B A C]."
  (elot-asui-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources
                  '(("A" nil) ("B" nil) ("C" nil)))
      (elot-label-move-source-down "A" nil)
      (should (equal elot-active-label-sources
                     '(("B" nil) ("A" nil) ("C" nil)))))))

(ert-deftest test-move-source-up-at-top-noop ()
  "Moving the first entry up is a no-op."
  (elot-asui-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources
                  '(("A" nil) ("B" nil)))
      (elot-label-move-source-up "A" nil)
      (should (equal elot-active-label-sources
                     '(("A" nil) ("B" nil)))))))

(ert-deftest test-move-source-down-at-bottom-noop ()
  "Moving the last entry down is a no-op."
  (elot-asui-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources
                  '(("A" nil) ("B" nil)))
      (elot-label-move-source-down "B" nil)
      (should (equal elot-active-label-sources
                     '(("A" nil) ("B" nil)))))))

;;;; Tabulated entries

(ert-deftest test-tabulated-list-reflects-order ()
  "`elot-label--active-build-entries' rows reflect variable order and position."
  (elot-asui-test--with-fresh
    (let* ((entries '(("one" nil) ("two" "ds") ("three" nil)))
           (rows (elot-label--active-build-entries entries)))
      (should (equal (length rows) 3))
      (should (equal (elt (cadr (nth 0 rows)) 0) "0"))
      (should (equal (elt (cadr (nth 0 rows)) 1) "one"))
      (should (equal (elt (cadr (nth 1 rows)) 0) "1"))
      (should (equal (elt (cadr (nth 1 rows)) 2) "ds"))
      (should (equal (elt (cadr (nth 2 rows)) 0) "2"))
      (should (equal (elt (cadr (nth 2 rows)) 1) "three")))))

;;;; Change hook

(ert-deftest test-change-hook-fires-on-mutations ()
  "The change hook fires on activate, deactivate, and move."
  (elot-asui-test--with-fresh
    (let ((csv (elot-asui-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (let ((count 0))
          (add-hook 'elot-active-label-sources-change-hook
                    (lambda () (setq count (1+ count))) nil t)
          (elot-label-activate-source csv nil)
          (should (= count 1))
          (setq-local elot-active-label-sources
                      (append elot-active-label-sources
                              '(("X" nil) ("Y" nil))))
          (elot-label-move-source-up "X" nil)
          (should (= count 2))
          (elot-label-deactivate-source "Y" nil)
          (should (= count 3)))))))

;;;; Global-mode auto-refresh

(ert-deftest test-global-mode-refreshes-on-source-change ()
  "Activating a new source while global mode is on rebuilds the regex."
  (elot-asui-test--with-fresh
    (let ((csv (elot-asui-test--copy-fixture "labels.csv"))
          (tsv (elot-asui-test--copy-fixture "labels.tsv")))
      (elot-label-register-source csv)
      (elot-label-register-source tsv)
      (with-temp-buffer
        (setq-local elot-active-label-sources (list (list csv nil)))
        (elot-global-label-display-mode 1)
        ;; Initial regex matches ex:Widget (from CSV) but not EMP-001.
        ;; Match semantically -- regexp-opt factors common prefixes, so
        ;; the regex source won't contain "ex:Widget" as a substring.
        (should (string-match-p elot-global--fontify-regexp "see ex:Widget here"))
        (should-not (string-match-p elot-global--fontify-regexp "see EMP-001 here"))
        ;; Activate the TSV source -- hook should rebuild the regex.
        (elot-label-activate-source tsv nil)
        (should (string-match-p elot-global--fontify-regexp "see EMP-001 here"))
        (elot-global-label-display-mode -1)))))

;;;; Auto-repersist semantics

(ert-deftest test-no-auto-persist-when-dir-locals-absent ()
  "Without a dir-locals entry, reorder does not create .dir-locals.el."
  (elot-asui-test--with-fresh
    (let* ((tmpdir (make-temp-file "elot-asui-proj-" t))
           (file (expand-file-name "dummy.txt" tmpdir)))
      (push tmpdir elot-asui-test--tmp-files)
      (with-temp-file file (insert "x"))
      (push file elot-asui-test--tmp-files)
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (setq-local elot-active-label-sources
                        '(("A" nil) ("B" nil)))
            (elot-label-move-source-down "A" nil)
            (should-not (file-exists-p
                         (expand-file-name ".dir-locals.el" tmpdir)))
            (kill-buffer (current-buffer)))
        (when (file-directory-p tmpdir)
          (dolist (f (directory-files tmpdir t "^[^.]"))
            (ignore-errors (delete-file f)))
          (ignore-errors (delete-directory tmpdir)))))))

(ert-deftest test-auto-repersist-when-dir-locals-exists ()
  "With a pre-existing dir-locals entry, reorder re-writes the file."
  (elot-asui-test--with-fresh
    (let* ((tmpdir  (make-temp-file "elot-asui-proj-" t))
           (dl      (expand-file-name ".dir-locals.el" tmpdir))
           (file    (expand-file-name "dummy.txt" tmpdir)))
      (push tmpdir elot-asui-test--tmp-files)
      (with-temp-file file (insert "x"))
      (push file elot-asui-test--tmp-files)
      ;; Seed .dir-locals.el with an initial ordering.
      (with-temp-file dl
        (insert ";;; Directory Local Variables -*- no-byte-compile: t -*-\n"
                "((nil . ((elot-active-label-sources . ((\"A\" nil) (\"B\" nil))))))\n"))
      (unwind-protect
          (with-current-buffer (find-file-noselect file)
            (setq-local elot-active-label-sources
                        '(("A" nil) ("B" nil)))
            (elot-label-move-source-down "A" nil)
            (should (file-exists-p dl))
            (let ((content (with-temp-buffer
                             (insert-file-contents dl)
                             (buffer-string))))
              ;; New order: B before A
              (should (string-match-p "\"B\"" content))
              (should (string-match-p "elot-active-label-sources" content))
              (let* ((b-pos (string-match "\"B\"" content))
                     (a-pos (string-match "\"A\"" content)))
                (should (and b-pos a-pos (< b-pos a-pos)))))
            (kill-buffer (current-buffer)))
        (when (file-directory-p tmpdir)
          (dolist (f (directory-files tmpdir t "^[^.]"))
            (ignore-errors (delete-file f)))
          (ignore-errors (delete-directory tmpdir)))))))

(provide 'elot-active-sources-ui-test)
;;; elot-active-sources-ui-test.el ends here
