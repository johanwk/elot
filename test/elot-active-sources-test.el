;;; elot-active-sources-test.el --- Tests for Step 1.5  -*- lexical-binding: t; -*-

;;; Commentary:
;; Step 1.5: buffer-local `elot-active-label-sources', activate/deactivate
;; commands, and priority-respected lookup integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)

(defconst elot-as-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-as-test--fixture (name)
  (expand-file-name (concat "fixtures/" name) elot-as-test--dir))

(defvar elot-as-test--tmp-db nil)
(defvar elot-as-test--tmp-files nil)

(defun elot-as-test--setup ()
  (setq elot-as-test--tmp-db  (make-temp-file "elot-as-" nil ".sqlite"))
  (setq elot-as-test--tmp-files nil)
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-as-test--tmp-db))

(defun elot-as-test--teardown ()
  (when elot-db (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-as-test--tmp-db (file-exists-p elot-as-test--tmp-db))
    (ignore-errors (delete-file elot-as-test--tmp-db)))
  (dolist (f elot-as-test--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f)))))

(defun elot-as-test--copy-fixture (name)
  (let* ((src (elot-as-test--fixture name))
         (ext (file-name-extension name))
         (tmp (make-temp-file "elot-as-" nil (concat "." ext))))
    (copy-file src tmp t)
    (push tmp elot-as-test--tmp-files)
    tmp))

(defmacro elot-as-test--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-as-test--setup) ,@body)
     (elot-as-test--teardown)))

;;;; Tests

(ert-deftest test-active-sources-buffer-local ()
  "`elot-active-label-sources' is buffer-local; changes in one buffer
do not leak to another."
  (elot-as-test--with-fresh
    (with-temp-buffer
      (setq-local elot-active-label-sources '(("a.csv" nil)))
      (with-temp-buffer
        (should (null elot-active-label-sources)))
      (should (equal elot-active-label-sources '(("a.csv" nil)))))))

(ert-deftest test-set-active-sources-normalizes-entries ()
  "`elot-label-set-active-sources' normalises empty data-sources to nil
and preserves order."
  (elot-as-test--with-fresh
    (with-temp-buffer
      (let ((result (elot-label-set-active-sources
                     '(("a.csv" "")
                       ("b.rq"  "data.ttl")
                       ("c.org" nil)))))
        (should (equal result
                       '(("a.csv" nil)
                         ("b.rq"  "data.ttl")
                         ("c.org" nil))))
        (should (equal elot-active-label-sources result))))))

(ert-deftest test-activate-source-appends-and-moves ()
  "Activating appends at end by default; re-activating with POSITION
moves the entry rather than duplicating it."
  (elot-as-test--with-fresh
    (with-temp-buffer
      (elot-label-activate-source "a.csv" nil)
      (elot-label-activate-source "b.csv" nil)
      (elot-label-activate-source "c.csv" nil)
      (should (equal elot-active-label-sources
                     '(("a.csv" nil) ("b.csv" nil) ("c.csv" nil))))
      ;; Move c.csv to the front.
      (elot-label-activate-source "c.csv" nil 0)
      (should (equal elot-active-label-sources
                     '(("c.csv" nil) ("a.csv" nil) ("b.csv" nil))))
      ;; Activating an existing entry at the end.
      (elot-label-activate-source "c.csv" nil)
      (should (equal elot-active-label-sources
                     '(("a.csv" nil) ("b.csv" nil) ("c.csv" nil)))))))

(ert-deftest test-deactivate-source-removes ()
  "`elot-label-deactivate-source' removes the matching entry and is a
no-op for absent entries."
  (elot-as-test--with-fresh
    (with-temp-buffer
      (elot-label-set-active-sources
       '(("a.csv" nil) ("b.rq" "data.ttl") ("c.csv" nil)))
      (elot-label-deactivate-source "b.rq" "data.ttl")
      (should (equal elot-active-label-sources
                     '(("a.csv" nil) ("c.csv" nil))))
      ;; No-op on missing.
      (elot-label-deactivate-source "nope.csv" nil)
      (should (equal elot-active-label-sources
                     '(("a.csv" nil) ("c.csv" nil)))))))

(ert-deftest test-safe-local-variable-predicate ()
  "The safe-local-variable predicate accepts well-formed lists and
rejects malformed ones."
  (let ((pred (get 'elot-active-label-sources 'safe-local-variable)))
    (should (functionp pred))
    (should (funcall pred '()))
    (should (funcall pred '(("a.csv" nil))))
    (should (funcall pred '(("a.csv" nil) ("b.rq" "x.ttl"))))
    (should-not (funcall pred '("a.csv")))          ; not a list of lists
    (should-not (funcall pred '((:sym nil))))       ; non-string source
    (should-not (funcall pred '(("a.csv" 42))))))   ; non-string data-source

(ert-deftest test-priority-respected-in-lookup ()
  "Reordering `elot-active-label-sources' changes which source wins."
  (elot-as-test--with-fresh
    (let* ((csv  (elot-as-test--copy-fixture "labels.csv"))
           (json (elot-as-test--copy-fixture "labels-nested.json")))
      (elot-label-register-source csv)
      (elot-label-register-source json)
      ;; Both fixtures define ex:Widget; labels are identical so we
      ;; instead overwrite one side to make the priority test sharp.
      (sqlite-execute
       elot-db
       "UPDATE entities SET label = 'Widget (from CSV)' \
        WHERE id = 'ex:Widget' AND source = ?" (list csv))
      (sqlite-execute
       elot-db
       "UPDATE entities SET label = 'Widget (from JSON)' \
        WHERE id = 'ex:Widget' AND source = ?" (list json))
      (with-temp-buffer
        ;; CSV first.
        (elot-label-set-active-sources `((,csv nil) (,json nil)))
        (should (equal "Widget (from CSV)"
                       (elot-db-get-label "ex:Widget")))
        ;; JSON first.
        (elot-label-set-active-sources `((,json nil) (,csv nil)))
        (should (equal "Widget (from JSON)"
                       (elot-db-get-label "ex:Widget")))))))

(ert-deftest test-activate-then-lookup-end-to-end ()
  "Activating a registered source enables lookup in the same buffer."
  (elot-as-test--with-fresh
    (let ((csv (elot-as-test--copy-fixture "labels.csv")))
      (elot-label-register-source csv)
      (with-temp-buffer
        (should (null (elot-db-get-label "ex:Widget")))
        (elot-label-activate-source csv nil)
        (should (equal "Widget" (elot-db-get-label "ex:Widget")))))))

(provide 'elot-active-sources-test)
;;; elot-active-sources-test.el ends here
