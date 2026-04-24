;;; elot-db-lang-picker-test.el --- Step 1.16.5 tests  -*- lexical-binding: t; -*-

;; Tests for activating the language picker on the read path:
;; `elot-db--pick-value-by-lang' now delegates to
;; `elot-db--select-by-language', consulting `elot-preferred-languages'.
;; Affects `elot-db-get-attr', `elot-db-get-all-attrs',
;; `elot-db-get-label', and `elot-db-all-active-labels'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-db)

(defvar elot-db-lang-picker--tmpfile nil)

(defun elot-db-lang-picker--fresh ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-picker--tmpfile
             (file-exists-p elot-db-lang-picker--tmpfile))
    (ignore-errors (delete-file elot-db-lang-picker--tmpfile)))
  (setq elot-db-lang-picker--tmpfile
        (make-temp-file "elot-db-lang-picker-" nil ".sqlite"))
  (ignore-errors (delete-file elot-db-lang-picker--tmpfile))
  (elot-db-init elot-db-lang-picker--tmpfile))

(defun elot-db-lang-picker--teardown ()
  (ignore-errors (elot-db-close))
  (when (and elot-db-lang-picker--tmpfile
             (file-exists-p elot-db-lang-picker--tmpfile))
    (ignore-errors (delete-file elot-db-lang-picker--tmpfile)))
  (setq elot-db-lang-picker--tmpfile nil))

(ert-deftest test-pick-value-by-lang-delegates-to-select ()
  "Picker consults `elot-preferred-languages' via `--select-by-language'."
  ;; Default prefs (nil -> untagged, en): untagged wins over en/ko.
  (should (equal "untagged"
                 (elot-db--pick-value-by-lang
                  '(("en-val" . "en") ("untagged" . "") ("ko-val" . "ko")))))
  ;; Default prefs, no untagged: en wins over ko.
  (should (equal "en-val"
                 (elot-db--pick-value-by-lang
                  '(("ko-val" . "ko") ("en-val" . "en")))))
  ;; Explicit PREFS arg pins Korean.
  (should (equal "ko-val"
                 (elot-db--pick-value-by-lang
                  '(("en-val" . "en") ("ko-val" . "ko"))
                  '("ko"))))
  ;; User-level override via defcustom.
  (let ((elot-preferred-languages '("ko")))
    (should (equal "ko-val"
                   (elot-db--pick-value-by-lang
                    '(("en-val" . "en") ("ko-val" . "ko")))))))

(ert-deftest test-db-get-attr-respects-prefs ()
  "elot-db-get-attr picks the lang-preferred row."
  (unwind-protect
      (progn
        (elot-db-lang-picker--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "E one"
            ("skos:definition" ("English" "en")
             "skos:definition" ("Korean"  "ko")))))
        ;; Default prefs -> English.
        (should (equal "English"
                       (elot-db-get-attr "e1" "skos:definition" '(("A" nil)))))
        ;; Korean-first prefs -> Korean.
        (let ((elot-preferred-languages '("ko")))
          (should (equal "Korean"
                         (elot-db-get-attr "e1" "skos:definition" '(("A" nil)))))))
    (elot-db-lang-picker--teardown)))

(ert-deftest test-db-get-all-attrs-respects-prefs ()
  "elot-db-get-all-attrs collapses lang variants via prefs."
  (unwind-protect
      (progn
        (elot-db-lang-picker--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "E one"
            ("rdfs:label"     ("E one"   "en")
             "rdfs:label"     ("E uno"   "es")
             "skos:definition" ("English" "en")
             "skos:definition" ("Korean"  "ko")))))
        ;; Default (no untagged, en preferred).
        (let ((pl (elot-db-get-all-attrs "e1" '(("A" nil)))))
          (should (equal "E one"   (plist-get pl "rdfs:label" #'equal)))
          (should (equal "English" (plist-get pl "skos:definition" #'equal))))
        ;; Korean preferred.
        (let* ((elot-preferred-languages '("ko" "en"))
               (pl (elot-db-get-all-attrs "e1" '(("A" nil)))))
          (should (equal "Korean" (plist-get pl "skos:definition" #'equal)))
          ;; No Korean rdfs:label -> alphabetical fallback (en < es) -> en.
          (should (equal "E one" (plist-get pl "rdfs:label" #'equal)))))
    (elot-db-lang-picker--teardown)))

(ert-deftest test-db-get-label-respects-prefs ()
  "elot-db-get-label consults rdfs:label rows when present."
  (unwind-protect
      (progn
        (elot-db-lang-picker--fresh)
        ;; Seed both the denormalised entity label AND multi-lang
        ;; rdfs:label rows.  The attribute rows should win once present.
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "default-label"
            ("rdfs:label" ("English label" "en")
             "rdfs:label" ("Korean label"  "ko")))))
        ;; Default -> en.
        (should (equal "English label"
                       (elot-db-get-label "e1" '(("A" nil)))))
        ;; Korean-first.
        (let ((elot-preferred-languages '("ko")))
          (should (equal "Korean label"
                         (elot-db-get-label "e1" '(("A" nil))))))
        ;; Fallback to denormalised entities.label when no rdfs:label rows.
        (elot-db-update-source
         "B" nil "csv"
         '(("e2" "plain-label" ("rdf:type" "owl:Class"))))
        (should (equal "plain-label"
                       (elot-db-get-label "e2" '(("B" nil))))))
    (elot-db-lang-picker--teardown)))

(ert-deftest test-db-all-active-labels-respects-prefs ()
  "elot-db-all-active-labels uses the preferred rdfs:label per id."
  (unwind-protect
      (progn
        (elot-db-lang-picker--fresh)
        (elot-db-update-source
         "A" nil "csv"
         '(("e1" "default-one"
            ("rdfs:label" ("English one" "en")
             "rdfs:label" ("Korean one"  "ko")))
           ("e2" "default-two"
            ("rdfs:label" ("English two" "en")
             "rdfs:label" ("Korean two"  "ko")))
           ;; No rdfs:label rows -> falls back to entities.label.
           ("e3" "entity-three" ())))
        ;; Default prefs: English wins where attribute rows exist.
        (let ((ht (elot-db-all-active-labels '(("A" nil)))))
          (should (equal '("e1") (gethash "English one" ht)))
          (should (equal '("e2") (gethash "English two" ht)))
          (should (equal '("e3") (gethash "entity-three" ht)))
          (should (null (gethash "Korean one" ht))))
        ;; Korean prefs: Korean wins; fallback unchanged.
        (let* ((elot-preferred-languages '("ko"))
               (ht (elot-db-all-active-labels '(("A" nil)))))
          (should (equal '("e1") (gethash "Korean one" ht)))
          (should (equal '("e2") (gethash "Korean two" ht)))
          (should (equal '("e3") (gethash "entity-three" ht)))
          (should (null (gethash "English one" ht)))))
    (elot-db-lang-picker--teardown)))

(provide 'elot-db-lang-picker-test)
;;; elot-db-lang-picker-test.el ends here
