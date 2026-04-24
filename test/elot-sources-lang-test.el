;;; elot-sources-lang-test.el --- ERT tests for Step 1.16.6  -*- lexical-binding: t; coding: utf-8 -*-

;; Step 1.16.6 tests: CSV/TSV/JSON language-tag conventions.
;;
;; Recognises:
;;   - a `lang' header column (row-level tag), attached to the
;;     primary rdfs:label as a cons entry.
;;   - `label@TAG' header suffixes producing additional rdfs:label
;;     attribute rows with LANG=TAG.
;;
;; Behaviour is backward-compatible: sources without these columns
;; ingest exactly as before.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'elot-sources)

;; Ensure UTF-8 decoding throughout, so no interactive coding-system
;; prompt appears in batch when the parser reads tmpfiles with
;; non-ASCII content.
(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

(defun elot-s16-6--tmpfile (suffix content)
  (let ((f (make-temp-file "elot-s16-6-" nil suffix))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file f
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert content))
    f))

(defun elot-s16-6--rdfs-labels (plist)
  "Return the list of rdfs:label entries in PLIST (as written)."
  (let (result)
    (cl-loop for (k v) on plist by #'cddr
             when (equal k "rdfs:label")
             do (push v result))
    (nreverse result)))

;;;; -------------------------------------------------------------------
;;;; CSV: `lang' header column
;;;; -------------------------------------------------------------------

(ert-deftest test-csv-lang-column-recognised ()
  "`lang' header attaches BCP-47 tag to the row's primary label."
  (let* ((csv (elot-s16-6--tmpfile
               ".csv"
               "id,label,lang,definition\n\
ex:A,English one,en,\"The English one.\"\n\
ex:B,\uD55C\uAD6D\uC5B4,ko,\n\
ex:C,untagged thing,,\"no tag.\"\n"))
         (entries (elot-source-parse-csv csv)))
    (unwind-protect
        (progn
          (should (= 3 (length entries)))
          ;; Row A: rdfs:label carries the (label lang) cons.
          (let* ((a (cl-find "ex:A" entries :key #'car :test #'equal))
                 (labels (elot-s16-6--rdfs-labels (nth 2 a))))
            (should (equal "English one" (nth 1 a)))
            (should (equal '(("English one" "en")) labels))
            ;; `lang' column was consumed, not emitted as attribute.
            (should-not (plist-get (nth 2 a) "lang" #'equal))
            (should (equal "The English one."
                           (plist-get (nth 2 a) "definition" #'equal))))
          ;; Row C: empty lang -> rdfs:label with empty tag (Step 1.16.7:
          ;; when a `lang' column is present, every populated label is
          ;; emitted as an rdfs:label row so the picker can resolve
          ;; `:untagged').
          (let* ((c (cl-find "ex:C" entries :key #'car :test #'equal))
                 (labels (elot-s16-6--rdfs-labels (nth 2 c))))
            (should (equal '(("untagged thing" "")) labels))
            (should (equal "untagged thing" (nth 1 c)))))
      (delete-file csv))))

;;;; -------------------------------------------------------------------
;;;; CSV: `label@TAG' suffix columns
;;;; -------------------------------------------------------------------

(ert-deftest test-csv-label-at-suffix-recognised ()
  "`label@TAG' columns become additional rdfs:label rows tagged TAG."
  (let* ((csv (elot-s16-6--tmpfile
               ".csv"
               "id,label,label@ko,label@en-GB\n\
ex:X,Widget,\uC704\uC824,Widget\n\
ex:Y,Only English,,\n"))
         (entries (elot-source-parse-csv csv)))
    (unwind-protect
        (progn
          (should (= 2 (length entries)))
          (let* ((x (cl-find "ex:X" entries :key #'car :test #'equal))
                 (labels (elot-s16-6--rdfs-labels (nth 2 x))))
            (should (equal "Widget" (nth 1 x)))
            ;; Two rdfs:label rows, one per populated suffix column.
            (should (= 2 (length labels)))
            (should (member '("\uC704\uC824" "ko") labels))
            (should (member '("Widget" "en-GB") labels))
            ;; label@TAG columns were consumed.
            (should-not (plist-get (nth 2 x) "label@ko" #'equal))
            (should-not (plist-get (nth 2 x) "label@en-GB" #'equal)))
          ;; Row Y: empty suffix cells produce no extra rows.
          (let* ((y (cl-find "ex:Y" entries :key #'car :test #'equal))
                 (labels (elot-s16-6--rdfs-labels (nth 2 y))))
            (should (null labels))
            (should (equal "Only English" (nth 1 y)))))
      (delete-file csv))))

;;;; -------------------------------------------------------------------
;;;; Backward-compat: unadorned CSV still ingests identically.
;;;; -------------------------------------------------------------------

(ert-deftest test-csv-without-lang-conventions-unchanged ()
  "CSV with no `lang' / `label@...' columns ingests exactly as before."
  (let* ((csv (elot-s16-6--tmpfile
               ".csv"
               "id,label,definition\n\
ex:A,Widget,\"A generic widget.\"\n"))
         (entries (elot-source-parse-csv csv)))
    (unwind-protect
        (progn
          (should (= 1 (length entries)))
          (let ((a (car entries)))
            (should (equal "ex:A" (nth 0 a)))
            (should (equal "Widget" (nth 1 a)))
            (should (null (elot-s16-6--rdfs-labels (nth 2 a))))
            (should (equal "A generic widget."
                           (plist-get (nth 2 a) "definition" #'equal)))))
      (delete-file csv))))

;;;; -------------------------------------------------------------------
;;;; TSV: same conventions flow through the shared parser.
;;;; -------------------------------------------------------------------

(ert-deftest test-tsv-lang-column-recognised ()
  "TSV honours the `lang' column identically to CSV."
  (let* ((tsv (elot-s16-6--tmpfile
               ".tsv"
               "id\tlabel\tlang\nex:A\tEnglish one\ten\n"))
         (entries (elot-source-parse-tsv tsv)))
    (unwind-protect
        (let* ((a (car entries))
               (labels (elot-s16-6--rdfs-labels (nth 2 a))))
          (should (equal '(("English one" "en")) labels)))
      (delete-file tsv))))

;;;; -------------------------------------------------------------------
;;;; JSON: nested-shape `lang' key and `label@TAG' keys.
;;;; -------------------------------------------------------------------

(ert-deftest test-json-lang-key-recognised ()
  "JSON nested shape: `lang' key tags the primary label."
  (let* ((json (elot-s16-6--tmpfile
                ".json"
                "{\"ex:A\": {\"label\": \"English one\", \"lang\": \"en\"}}"))
         (entries (elot-source-parse-json json)))
    (unwind-protect
        (let* ((a (car entries))
               (labels (elot-s16-6--rdfs-labels (nth 2 a))))
          (should (equal "English one" (nth 1 a)))
          (should (equal '(("English one" "en")) labels)))
      (delete-file json))))

(ert-deftest test-json-label-at-suffix-recognised ()
  "JSON nested shape: `label@TAG' keys become extra rdfs:label rows."
  (let* ((json (elot-s16-6--tmpfile
                ".json"
                "{\"ex:X\": {\"label\": \"Widget\", \"label@ko\": \"\uC704\uC824\"}}"))
         (entries (elot-source-parse-json json)))
    (unwind-protect
        (let* ((x (car entries))
               (labels (elot-s16-6--rdfs-labels (nth 2 x))))
          (should (equal "Widget" (nth 1 x)))
          (should (member '("\uC704\uC824" "ko") labels)))
      (delete-file json))))

(provide 'elot-sources-lang-test)
;;; elot-sources-lang-test.el ends here
