;;; elot-label-lookup-lang-suffix-test.el --- Step 1.16.8 tests  -*- lexical-binding: t; coding: utf-8; -*-

;; Step 1.16.8: the stage-1 DB annotator surfaces `@LANG' when a
;; singleton label's id has multiple `rdfs:label' language variants
;; in the winning source.  Purely cosmetic; behaviour of the chosen
;; label is unchanged.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)
(require 'elot-label-display)

(defvar elot-ls--tmpfile nil)

(defun elot-ls--fresh ()
  (ignore-errors (elot-db-close))
  (when (and elot-ls--tmpfile (file-exists-p elot-ls--tmpfile))
    (ignore-errors (delete-file elot-ls--tmpfile)))
  (setq elot-ls--tmpfile
        (make-temp-file "elot-ls-" nil ".sqlite"))
  (ignore-errors (delete-file elot-ls--tmpfile))
  (elot-db-init elot-ls--tmpfile))

(defun elot-ls--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-ls--tmpfile (file-exists-p elot-ls--tmpfile))
    (ignore-errors (delete-file elot-ls--tmpfile)))
  (setq elot-ls--tmpfile nil))

(ert-deftest test-lang-suffix-for-id-multiple-variants ()
  "Singleton id with English+Korean `rdfs:label' yields @LANG suffix
  for the winning variant, and nothing for the non-winning label."
  (unwind-protect
      (progn
        (elot-ls--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("ex:Widget" "Widget"
            ("rdfs:label" ("Widget" "en")
             "rdfs:label" ("\uc704\uc82f" "ko")))))
        (let ((elot-active-label-sources '(("A" nil)))
              (elot-preferred-languages nil))   ; default: @en wins
          ;; Winner "Widget" @en  -> suffix "@en"
          (should (equal "@en"
                         (elot-label-lookup--lang-suffix-for-id
                          "Widget" "ex:Widget")))
          ;; Non-winning label "\uc704\uc82f": returns "" (defensive).
          (should (equal ""
                         (elot-label-lookup--lang-suffix-for-id
                          "\uc704\uc82f" "ex:Widget")))))
    (elot-ls--teardown)))

(ert-deftest test-lang-suffix-for-id-respects-prefs ()
  "Changing `elot-preferred-languages' changes the winning variant
  and therefore the suffix the annotator surfaces."
  (unwind-protect
      (progn
        (elot-ls--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("ex:Widget" "Widget"
            ("rdfs:label" ("Widget" "en")
             "rdfs:label" ("\u7a7a\u683c" "ko")))))
        (let ((elot-active-label-sources '(("A" nil)))
              (elot-preferred-languages '("ko")))
          (should (equal "@ko"
                         (elot-label-lookup--lang-suffix-for-id
                          "\u7a7a\u683c" "ex:Widget")))))
    (elot-ls--teardown)))

(ert-deftest test-lang-suffix-for-id-single-variant-empty ()
  "A single `rdfs:label' variant yields the empty string suffix;
  likewise for an id with no `rdfs:label' attribute rows."
  (unwind-protect
      (progn
        (elot-ls--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("ex:Solo" "Solo" ("rdfs:label" ("Solo" "en")))
           ("ex:Bare" "Bare" ())))
        (let ((elot-active-label-sources '(("A" nil))))
          (should (equal "" (elot-label-lookup--lang-suffix-for-id
                             "Solo" "ex:Solo")))
          (should (equal "" (elot-label-lookup--lang-suffix-for-id
                             "Bare" "ex:Bare")))))
    (elot-ls--teardown)))

(ert-deftest test-db-annotations-includes-lang-suffix ()
  "`elot-label-lookup--db-annotations' for a singleton label whose
  id has multiple language variants includes the `@LANG' marker."
  (unwind-protect
      (progn
        (elot-ls--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("ex:Widget" "Widget"
            ("rdf:type"   "owl:Class"
             "rdfs:label" ("Widget" "en")
             "rdfs:label" ("\u7a7a\u683c" "ko")))))
        (let* ((elot-active-label-sources '(("A" nil)))
               (elot-preferred-languages nil)
               (elot-label-lookup-tmp-attriblist-ht
                (make-hash-table :test 'equal)))
          (puthash "Widget"
                   (list :ids '("ex:Widget") :count 1)
                   elot-label-lookup-tmp-attriblist-ht)
          (let ((annot (elot-label-lookup--db-annotations "Widget")))
            (should (stringp annot))
            (should (string-match-p "@en" annot)))))
    (elot-ls--teardown)))

(ert-deftest test-db-annotations-no-suffix-when-single ()
  "Singleton label with exactly one `rdfs:label' variant has no
  `@LANG' marker in the annotation string."
  (unwind-protect
      (progn
        (elot-ls--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("ex:Solo" "Solo"
            ("rdf:type"   "owl:Class"
             "rdfs:label" ("Solo" "en")))))
        (let* ((elot-active-label-sources '(("A" nil)))
               (elot-label-lookup-tmp-attriblist-ht
                (make-hash-table :test 'equal)))
          (puthash "Solo"
                   (list :ids '("ex:Solo") :count 1)
                   elot-label-lookup-tmp-attriblist-ht)
          (let ((annot (elot-label-lookup--db-annotations "Solo")))
            (should (stringp annot))
            (should-not (string-match-p "@en" annot)))))
    (elot-ls--teardown)))

(provide 'elot-label-lookup-lang-suffix-test)
;;; elot-label-lookup-lang-suffix-test.el ends here
