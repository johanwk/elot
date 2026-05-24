;;; elot-gptel-conventions-test.el --- Tests for elot_conventions tool  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-conventions-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.1 -- tests for the
;; zero-arg `elot_conventions' tool.  The prose lives in the sibling
;; file `elot-conventions.md'; the Elisp side is a path resolver +
;; thin tool wrapper.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root)))

(require 'elot-gptel)

(ert-deftest elot-gptel-conventions-test-path-resolves ()
  "The on-disk `elot-conventions.md' is found alongside the library."
  (let ((path (elot-gptel--conventions-path)))
    (should (stringp path))
    (should (file-readable-p path))
    (should (string-match-p "elot-conventions\\.md\\'" path))))

(ert-deftest elot-gptel-conventions-test-content-shape ()
  "The conventions document mentions every cardinal idiom."
  (let ((text (elot-gptel-tool-conventions)))
    (should (stringp text))
    (should-not (string-prefix-p "ERROR:" text))
    ;; Core idioms the LLM must internalise.
    (should (string-match-p "Heading nesting" text))
    (should (string-match-p "SubClassOf" text))
    (should (string-match-p "Label (curie)" text))
    (should (string-match-p "rdfs:isDefinedBy" text))
    (should (string-match-p ":nodeclare:" text))
    (should (string-match-p "resourcedefs" text))
    (should (string-match-p "prefixdefs" text))))

(ert-deftest elot-gptel-conventions-test-matches-file ()
  "Tool output matches the on-disk Markdown file byte-for-byte."
  (let* ((path (elot-gptel--conventions-path))
         (on-disk (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))
         (via-tool (elot-gptel-tool-conventions)))
    (should (string= on-disk via-tool))))

(ert-deftest elot-gptel-conventions-test-fallback-non-empty ()
  "The embedded fallback string is non-empty and mentions the key tag."
  (should (stringp elot-gptel--conventions-fallback))
  (should (> (length elot-gptel--conventions-fallback) 100))
  (should (string-match-p ":nodeclare:" elot-gptel--conventions-fallback)))

(ert-deftest elot-gptel-conventions-test-tool-spec-registered ()
  "The `elot_conventions' tool appears in `elot-gptel--tool-specs'."
  (let ((spec (assoc "elot_conventions" elot-gptel--tool-specs)))
    (should spec)
    (should (eq (plist-get (cdr spec) :function)
                'elot-gptel-tool-conventions))
    ;; Zero-arg.
    (should (null (plist-get (cdr spec) :args)))))

(provide 'elot-gptel-conventions-test)
;;; elot-gptel-conventions-test.el ends here
