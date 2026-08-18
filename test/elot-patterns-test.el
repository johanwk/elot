;;; elot-patterns-test.el --- Integrity tests for patterns/  -*- lexical-binding: t; -*-

;;; Commentary:

;; Keeps the pattern library in the normal ERT/CI suite.  The live ontology
;; test is the ERT equivalent of `make -C patterns elot-check': every ELOT
;; ontology in patterns/ is passed through the composite `elot_check'
;; pipeline.  The remaining tests protect the library index and its two
;; framework files from drifting.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-table)
(require 'elot)
(require 'elot-gptel)
(require 'elot-robot nil t)

(defconst elot-patterns-test--dir
  (expand-file-name
   "../patterns"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path of the repository's patterns directory.")

(defun elot-patterns-test--file (name)
  "Return the absolute path of NAME in the pattern library."
  (expand-file-name name elot-patterns-test--dir))

(defun elot-patterns-test--ontology-p (file)
  "Return non-nil when FILE declares an ELOT ontology outline."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward
     "^[ \t]*:ELOT-context-type:[ \t]+ontology[ \t]*$" nil t)))

(defun elot-patterns-test--ontology-files ()
  "Return ELOT ontology Org files whose names contain `-pattern-'."
  (cl-remove-if-not
   #'elot-patterns-test--ontology-p
   (sort (directory-files elot-patterns-test--dir t
                          "\\`.+-pattern-.+\\.org\\'")
         #'string<)))

(defun elot-patterns-test--pattern-files ()
  "Return the pattern source files, excluding framework support files."
  (sort (directory-files elot-patterns-test--dir nil
                         "\\`.+-pattern-.+\\.org\\'")
        #'string<))

(defun elot-patterns-test--index-entries ()
  "Return file names named by the cross-pattern index in README.org."
  (with-temp-buffer
    (insert-file-contents (elot-patterns-test--file "README.org"))
    (org-mode)
    (goto-char (point-min))
    (unless (re-search-forward
             "^[ \t]*#\\+caption:[ \t]*Patterns in this library[ \t]*$"
             nil t)
      (error "patterns/README.org has no 'Patterns in this library' table"))
    (unless (re-search-forward "^[ \t]*|" nil t)
      (error "pattern index caption is not followed by a table"))
    (goto-char (line-beginning-position))
    (let (entries)
      (dolist (row (org-table-to-lisp))
        (unless (eq row 'hline)
          (let ((cell (string-trim (car row))))
            (unless (or (string-empty-p cell) (string= cell "pattern"))
              ;; Permit either today's bare stem or an explicit Org file link.
              (when (string-match
                     "\\`\\[\\[file:\\([^]]+\\)\\]\\(?:\\[[^]]*\\]\\)?\\]\\'"
                     cell)
                (setq cell (match-string 1 cell)))
              (unless (string-suffix-p ".org" cell)
                (setq cell (concat cell ".org")))
              (push cell entries)))))
      (nreverse entries))))

(ert-deftest elot-patterns-test-framework-files-present ()
  "The framework specification and normative vocabulary remain in place."
  (dolist (name '("pattern-framework.org" "pattern-vocabulary.org"))
    (let ((file (elot-patterns-test--file name)))
      (should (file-regular-p file))
      (should (file-readable-p file))
      (should (> (file-attribute-size (file-attributes file)) 0)))))

(ert-deftest elot-patterns-test-offline-import-catalog-present ()
  "The catalog used by pattern elot_check runs remains available."
  (let ((catalog (elot-patterns-test--file "catalog-v001.xml")))
    (should (file-regular-p catalog))
    (should (file-readable-p catalog))
    (should (> (file-attribute-size (file-attributes catalog)) 0))))

(ert-deftest elot-patterns-test-index-points-to-existing-files ()
  "Every cross-pattern index row names a real, unique pattern source."
  (let ((entries (elot-patterns-test--index-entries)))
    (should entries)
    (should (= (length entries)
               (length (delete-dups (copy-sequence entries)))))
    (dolist (name entries)
      (let ((file (elot-patterns-test--file name)))
        (should (file-regular-p file))
        ;; An indexed source must also enter the elot_check file set; this
        ;; prevents a malformed/missing ontology drawer from silently
        ;; downgrading an indexed pattern to an unchecked prose file.
        (should (elot-patterns-test--ontology-p file))))))

(ert-deftest elot-patterns-test-index-covers-pattern-files ()
  "Every pattern source is indexed, and every index row is a pattern source."
  (should (equal (sort (elot-patterns-test--index-entries) #'string<)
                 (elot-patterns-test--pattern-files))))

(ert-deftest elot-patterns-test-actions-explicitly-identify-inputs ()
  "Pattern inputs have one action; fixed inputs use CONSTANT explicitly."
  (let ((supported '("CONSTANT" "BORROW" "BIND" "MINT"
                     "BORROW_OR_MINT" "BIND_OR_MINT" "MINT_OR_BIND")))
    (dolist (name (elot-patterns-test--pattern-files))
      (with-temp-buffer
        (insert-file-contents (elot-patterns-test--file name))
        (org-mode)
        (let ((constant-count 0))
          ;; Every explicit action agrees with the resource namespace.
          (goto-char (point-min))
          (while (re-search-forward
                  "^[ ]- pattern:action ::[ \t]+\\([^ \t\n]+\\)[ \t]*$"
                  nil t)
            (let ((action (match-string-no-properties 1)))
              (should (member action supported))
              (save-excursion
                (org-back-to-heading t)
                (let ((variable-p
                       (string-match-p
                        "\\(?:[(]\\)?var:[[:alnum:]_-]+"
                        (org-get-heading t t t t))))
                  (if variable-p
                      (should-not (string= action "CONSTANT"))
                    (should (string= action "CONSTANT"))
                    (cl-incf constant-count))))))
          (should (> constant-count 0))
          ;; Every var: resource heading is an input and has exactly one action.
          (org-map-entries
           (lambda ()
             (when (string-match-p
                    "\\(?:[(]\\)?var:[[:alnum:]_-]+"
                    (org-get-heading t t t t))
               (let ((end (save-excursion
                            (outline-next-heading)
                            (point)))
                     (count 0))
                 (save-excursion
                   (forward-line 1)
                   (while (re-search-forward
                           "^[ ]- pattern:action ::[ \t]+[^ \t\n]+[ \t]*$"
                           end t)
                     (cl-incf count)))
                 (should (= count 1)))))))))))

(ert-deftest elot-patterns-test-all-ontologies-pass-elot-check ()
  "Run the composite elot_check pipeline over every pattern ontology."
  ;; In the ROBOT CI job, do not permit an accidentally missing ROBOT to turn
  ;; this into lint-only coverage.  Ordinary local runs retain elot_check's
  ;; documented graceful fallback when ROBOT is unavailable.
  (when (and (fboundp 'elot-test-require-robot-p)
             (elot-test-require-robot-p))
    (elot-test-robot-skip-unless-available))
  (message "WARNING: only pattern files with -pattern- in the filename will be considered")
  (let ((files (elot-patterns-test--ontology-files))
        (elot-gptel-robot-catalog
         (elot-patterns-test--file "catalog-v001.xml")))
    (should files)
    (dolist (file files)
      (let ((report (elot-gptel-tool-check file)))
        (should
         (or (string-match-p "== SUMMARY ==[\n\r]+OK:" report)
             (ert-fail
              (format "elot_check failed for %s:\n%s"
                      (file-relative-name file elot-patterns-test--dir)
                      report))))))))

(provide 'elot-patterns-test)
;;; elot-patterns-test.el ends here
