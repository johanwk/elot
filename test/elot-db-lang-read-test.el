;;; elot-db-lang-read-test.el --- Step 1.16.4 tests  -*- lexical-binding: t; -*-

;; Tests for the widened read path: `elot-db-get-attr' and
;; `elot-db-get-all-attrs' now SELECT the `lang' column and feed
;; rows through `elot-db--pick-value-by-lang'.  In Step 1.16.4 the
;; picker collapses to `car' (first row wins), so observable
;; behaviour is identical to the pre-widening path.  Step 1.16.5
;; will swap the picker body for `elot-db--select-by-language'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

(defvar elot-db-lang-read--tmpfile nil)

(defun elot-db-lang-read--fresh ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-read--tmpfile
             (file-exists-p elot-db-lang-read--tmpfile))
    (ignore-errors (delete-file elot-db-lang-read--tmpfile)))
  (setq elot-db-lang-read--tmpfile
        (make-temp-file "elot-db-lang-read-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-lang-read--tmpfile))
  (elot-db-init elot-db-lang-read--tmpfile))

(defun elot-db-lang-read--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-read--tmpfile
             (file-exists-p elot-db-lang-read--tmpfile))
    (ignore-errors (delete-file elot-db-lang-read--tmpfile)))
  (setq elot-db-lang-read--tmpfile nil))

(ert-deftest test-get-all-attrs-invariant-for-untagged ()
  "With all rows carrying lang='', get-all-attrs output is unchanged."
  (unwind-protect
      (progn
        (elot-db-lang-read--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "E one" ("rdf:type" "owl:Class"
                          "skos:definition" "the first"))))
        (let ((pl (elot-db-get-all-attrs "e1" '(("A" nil)))))
          (should (equal "owl:Class" (plist-get pl "rdf:type" #'equal)))
          (should (equal "the first" (plist-get pl "skos:definition" #'equal)))
          (should (equal (cons "A" "")
                         (plist-get pl :source-origin)))))
    (elot-db-lang-read--teardown)))

(ert-deftest test-get-all-attrs-first-row-wins-for-multi-lang ()
  "Multi-lang rows for a single prop collapse to the first (Step 1.16.4)."
  (unwind-protect
      (progn
        (elot-db-lang-read--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "E one"
            ("skos:definition" ("English def" "en")
             "skos:definition" ("Korean def" "ko")
             "rdf:type" "owl:Class"))))
        (let ((pl (elot-db-get-all-attrs "e1" '(("A" nil)))))
          ;; First row (English) wins under the car-picker.
          (should (equal "English def" (plist-get pl "skos:definition" #'equal)))
          (should (equal "owl:Class" (plist-get pl "rdf:type" #'equal)))
          ;; Each prop key appears once in the flat plist.
          (should (= 1 (cl-count "skos:definition" pl :test #'equal)))))
    (elot-db-lang-read--teardown)))

(ert-deftest test-get-attr-first-row-wins-for-multi-lang ()
  "elot-db-get-attr also runs through the picker; first row wins."
  (unwind-protect
      (progn
        (elot-db-lang-read--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "E one"
            ("rdfs:label" ("E one" "en")
             "rdfs:label" ("E uno" "es")))))
        (should (equal "E one"
                       (elot-db-get-attr "e1" "rdfs:label"
                                         '(("A" nil))))))
    (elot-db-lang-read--teardown)))

(ert-deftest test-pick-value-by-lang-is-car ()
  "Step 1.16.4 picker was pure car; Step 1.16.5 activates the language
picker.  For a single untagged row and for an empty list, the output
remains unchanged (car-equivalent); multi-row cases are covered by
`elot-db-lang-picker-test.el'."
  (should (equal "v1" (elot-db--pick-value-by-lang
                       '(("v1" . "")))))
  (should (null (elot-db--pick-value-by-lang nil))))

(provide 'elot-db-lang-read-test)
;;; elot-db-lang-read-test.el ends here
