;;; elot-db-lang-write-test.el --- Step 1.16.3 tests  -*- lexical-binding: t; -*-

;; Tests for writer-side language-tag plumbing.
;;
;; Step 1.16.3 widens `elot-db-update-source' to accept PLIST values
;; of the form (VALUE LANG) in addition to bare strings.  LANG is
;; written to the new `attributes.lang' column (Step 1.16.2); bare
;; strings continue to write `lang = '''.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

(defvar elot-db-lang-write--tmpfile nil)

(defun elot-db-lang-write--fresh ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-write--tmpfile
             (file-exists-p elot-db-lang-write--tmpfile))
    (ignore-errors (delete-file elot-db-lang-write--tmpfile)))
  (setq elot-db-lang-write--tmpfile
        (make-temp-file "elot-db-lang-write-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-lang-write--tmpfile))
  (elot-db-init elot-db-lang-write--tmpfile))

(defun elot-db-lang-write--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-write--tmpfile
             (file-exists-p elot-db-lang-write--tmpfile))
    (ignore-errors (delete-file elot-db-lang-write--tmpfile)))
  (setq elot-db-lang-write--tmpfile nil))

(ert-deftest test-update-source-writes-lang-column ()
  "Cons-form `(VALUE LANG)' entries write LANG to attributes.lang."
  (unwind-protect
      (progn
        (elot-db-lang-write--fresh)
        (elot-db-update-source
         "S" nil "csv"
         '(("e1" "label-en"
            ("rdfs:label"      ("label-ko" "ko")
             "skos:definition" ("def-en"   "en")
             "rdf:type"        "owl:Class"))))
        (let ((rows (sqlite-select
                     elot-db
                     "SELECT prop, value, lang FROM attributes
                       WHERE id = ? AND source = ? AND data_source = ?
                       ORDER BY prop, lang"
                     (list "e1" "S" ""))))
          (should (member '("rdf:type" "owl:Class" "") rows))
          (should (member '("rdfs:label" "label-ko" "ko") rows))
          (should (member '("skos:definition" "def-en" "en") rows))))
    (elot-db-lang-write--teardown)))

(ert-deftest test-update-source-bare-string-unchanged ()
  "Bare-string PLIST values still write lang = ''."
  (unwind-protect
      (progn
        (elot-db-lang-write--fresh)
        (elot-db-update-source
         "S" nil "csv"
         '(("e1" "label"
            ("rdfs:comment" "a comment"))))
        (let ((rows (sqlite-select
                     elot-db
                     "SELECT value, lang FROM attributes
                       WHERE id = ? AND prop = ?"
                     (list "e1" "rdfs:comment"))))
          (should (equal rows '(("a comment" ""))))))
    (elot-db-lang-write--teardown)))

(ert-deftest test-update-source-empty-lang-normalised ()
  "Cons form with nil or empty LANG writes ''."
  (unwind-protect
      (progn
        (elot-db-lang-write--fresh)
        (elot-db-update-source
         "S" nil "csv"
         '(("e1" "label"
            ("p1" ("v1" nil)
             "p2" ("v2" "")))))
        (let ((rows (sqlite-select
                     elot-db
                     "SELECT prop, lang FROM attributes
                       WHERE id = ? ORDER BY prop"
                     (list "e1"))))
          (should (equal rows '(("p1" "") ("p2" ""))))))
    (elot-db-lang-write--teardown)))

(provide 'elot-db-lang-write-test)
;;; elot-db-lang-write-test.el ends here
