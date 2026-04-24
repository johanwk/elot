;;; elot-sources-ttl-lang-test.el --- Tests for Step 1.16.7  -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:
;; Step 1.16.7: default TTL label-query projects LANG(?label),
;; CSV `lang' column recognition (from 1.16.6) carries the tag
;; into attributes.lang.  ROBOT-gated: skips cleanly if ROBOT
;; is not available.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-db)
(require 'elot-sources)

(defconst elot-s16-7--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defvar elot-s16-7--tmp-db nil)
(defvar elot-s16-7--tmp-files nil)

(defun elot-s16-7--setup ()
  (setq elot-s16-7--tmp-db (make-temp-file "elot-s16-7-" nil ".sqlite")
        elot-s16-7--tmp-files nil)
  (when (and (boundp 'elot-db) elot-db)
    (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (elot-db-init elot-s16-7--tmp-db))

(defun elot-s16-7--teardown ()
  (when (and (boundp 'elot-db) elot-db)
    (ignore-errors (sqlite-close elot-db)) (setq elot-db nil))
  (when (and elot-s16-7--tmp-db (file-exists-p elot-s16-7--tmp-db))
    (ignore-errors (delete-file elot-s16-7--tmp-db)))
  (dolist (f elot-s16-7--tmp-files)
    (when (and f (file-exists-p f)) (ignore-errors (delete-file f)))))

(defun elot-s16-7--copy-fixture (name)
  (let* ((src (expand-file-name (concat "fixtures/" name) elot-s16-7--dir))
         (tmp (make-temp-file "elot-s16-7-" nil
                              (concat "." (file-name-extension name)))))
    (copy-file src tmp t)
    (push tmp elot-s16-7--tmp-files)
    tmp))

(defmacro elot-s16-7--with-fresh (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (progn (elot-s16-7--setup) ,@body)
     (elot-s16-7--teardown)))

(defun elot-s16-7--robot-or-skip ()
  (unless (elot-robot-available-p)
    (ert-skip "ROBOT not available")))

(ert-deftest test-default-ttl-query-projects-lang ()
  "The default `elot-source-ttl-label-query' projects ?lang."
  (should (string-match-p "LANG(\\?label)" elot-source-ttl-label-query))
  (should (string-match-p "\\?lang" elot-source-ttl-label-query)))

(ert-deftest test-ttl-ingest-preserves-language-tags ()
  "Registering a multi-language TTL carries tags into attributes.lang."
  (elot-s16-7--robot-or-skip)
  (elot-s16-7--with-fresh
    (let ((ttl (elot-s16-7--copy-fixture "labels-multilang.ttl")))
      (elot-label-register-source ttl)
      (let* ((rows (sqlite-select
                    elot-db
                    "SELECT value, lang FROM attributes
                       WHERE source = ? AND prop = 'rdfs:label'
                         AND id = 'http://example.org/ex/Widget'
                       ORDER BY value"
                    (list ttl)))
             (tags (mapcar #'cadr rows)))
        (should (= 3 (length rows)))
        ;; All three tag variants present (en, ko, untagged "").
        (should (member "en" tags))
        (should (member "ko" tags))
        (should (member ""   tags))))))

(ert-deftest test-ttl-ingest-respects-preferred-language ()
  "End-to-end: `elot-db-get-label' picks the preferred-language label."
  (elot-s16-7--robot-or-skip)
  (elot-s16-7--with-fresh
    (let ((ttl (elot-s16-7--copy-fixture "labels-multilang.ttl")))
      (elot-label-register-source ttl)
      (let ((actives `((,ttl))))
        ;; Default prefs: untagged first, then @en.
        (let ((elot-preferred-languages nil))
          (should (equal "Widget-untagged"
                         (elot-db-get-label
                          "http://example.org/ex/Widget" actives))))
        ;; Korean first.
        (let ((elot-preferred-languages '("ko")))
          (should (equal "위젯"
                         (elot-db-get-label
                          "http://example.org/ex/Widget" actives))))
        ;; English first (explicitly skipping untagged).
        (let ((elot-preferred-languages '("en" "ko")))
          (should (equal "Widget"
                         (elot-db-get-label
                          "http://example.org/ex/Widget" actives))))))))

(provide 'elot-sources-ttl-lang-test)

;;; elot-sources-ttl-lang-test.el ends here
