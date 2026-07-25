;;; elot-gptel-lint-content-test.el --- Tests for M7.5 Step 7.5.2 -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-lint-content-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 7.5 Step 7.5.2:
;;
;; Tests for the optional `content' argument on `elot_lint' and
;; `elot_omn_validate' -- lets an LLM lint/validate its own draft
;; without a save roundtrip through the user's on-disk source file.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar elot-gptel-lint-content-test--repo-root nil)
(defvar elot-gptel-lint-content-test--fixtures nil)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file))
  (setq elot-gptel-lint-content-test--repo-root repo-root
        elot-gptel-lint-content-test--fixtures
        (expand-file-name "test/fixtures" repo-root)))

(require 'elot-gptel)

(defun elot-gptel-lint-content-test--read (name)
  "Return the contents of NAME under the fixtures directory."
  (let ((path (expand-file-name name elot-gptel-lint-content-test--fixtures)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;; ---------------------------------------------------------------------------
;;; Path resolution (covers `elot-gptel--resolve-file-path')
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-lint-content-test-resolve-nonexistent-path ()
  "`elot-gptel--resolve-file-path' accepts a yet-to-exist path inside the project."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (path (elot-gptel--resolve-file-path "drafts/new-ontology.org")))
    (should (stringp path))
    (should (file-name-absolute-p path))
    (should (string-suffix-p "new-ontology.org" path))))

(ert-deftest elot-gptel-lint-content-test-resolve-rejects-traversal ()
  "Traversal still refused for nonexistent paths."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root))
    (should-error (elot-gptel--resolve-file-path "../outside/foo.org")
                  :type 'user-error)))

(ert-deftest elot-gptel-lint-content-test-resolve-rejects-empty ()
  "Empty/nil path refused."
  (should-error (elot-gptel--resolve-file-path nil) :type 'user-error)
  (should-error (elot-gptel--resolve-file-path "") :type 'user-error))

;;; ---------------------------------------------------------------------------
;;; Content size cap
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-lint-content-test-size-cap-allows-small ()
  "Small content passes the size check."
  (should-not (elot-gptel--check-content-size "small")))

(ert-deftest elot-gptel-lint-content-test-size-cap-refuses-oversize ()
  "Content exceeding the cap is refused with `user-error'."
  (let ((elot-gptel-content-arg-max-bytes 100))
    (should-error (elot-gptel--check-content-size
                   (make-string 200 ?x))
                  :type 'user-error)))

(ert-deftest elot-gptel-lint-content-test-size-cap-disabled ()
  "Zero or negative cap disables the check."
  (let ((elot-gptel-content-arg-max-bytes 0))
    (should-not (elot-gptel--check-content-size
                 (make-string 10000 ?x)))))

;;; ---------------------------------------------------------------------------
;;; elot_lint with CONTENT
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-lint-content-test-clean-draft ()
  "A clean draft lints OK when supplied via CONTENT."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (content (elot-gptel-lint-content-test--read "minimal-ontology.org"))
         ;; Use a path that does NOT exist on disk -- the whole point
         ;; of CONTENT is to lint an in-flight draft.
         (out (elot-gptel-tool-lint
               "drafts/scratch-minimal.org" "all" nil content)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))
    (should (or (string= out "OK: no lint issues")
                (string-match-p "Summary: 0 errors" out)))))

(ert-deftest elot-gptel-lint-content-test-broken-draft ()
  "Errors in CONTENT are surfaced just like errors in an on-disk file."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (content (elot-gptel-lint-content-test--read "gptel-lint-broken.org"))
         (out (elot-gptel-tool-lint
               "drafts/scratch-broken.org" "all" nil content)))
    (should (stringp out))
    (should-not (string= out "OK: no lint issues"))
    (should (string-match-p "elot/prefix-table" out))))

(ert-deftest elot-gptel-lint-content-test-content-overrides-disk ()
  "CONTENT, when supplied, is linted in place of the on-disk file.
Supply a clean draft but point FILE at the deliberately-broken
fixture -- the resulting lint must reflect the clean CONTENT,
not the broken file."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (clean (elot-gptel-lint-content-test--read "minimal-ontology.org"))
         (out (elot-gptel-tool-lint
               "test/fixtures/gptel-lint-broken.org" "all" nil clean)))
    (should (stringp out))
    (should-not (string-match-p "elot/prefix-table" out))
    (should (or (string= out "OK: no lint issues")
                (string-match-p "Summary: 0 errors" out)))))

(ert-deftest elot-gptel-lint-content-test-content-traversal-refused ()
  "CONTENT does not bypass the project-traversal guard."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (out (elot-gptel-tool-lint
               "../../etc/passwd" "all" nil "* foo")))
    (should (stringp out))
    (should (string-prefix-p "ERROR:" out))
    (should (string-match-p "outside project" out))))

(ert-deftest elot-gptel-lint-content-test-content-size-cap ()
  "Oversized CONTENT is refused with a structured ERROR line."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (elot-gptel-content-arg-max-bytes 64)
         (big (make-string 200 ?x))
         (out (elot-gptel-tool-lint
               "drafts/scratch.org" "all" nil big)))
    (should (stringp out))
    (should (string-prefix-p "ERROR:" out))
    (should (string-match-p "too large" out))))

(ert-deftest elot-gptel-lint-content-test-empty-content-falls-through ()
  "An empty/whitespace-only CONTENT string falls through to on-disk."
  (let* ((default-directory elot-gptel-lint-content-test--repo-root)
         (file (expand-file-name "minimal-ontology.org"
                                 elot-gptel-lint-content-test--fixtures))
         (out (elot-gptel-tool-lint file "all" nil "")))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))))

;;; ---------------------------------------------------------------------------
;;; Tool spec / dispatcher arity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-lint-content-test-spec-declares-content-arg ()
  "The `elot_lint' tool spec declares the optional `content' string arg."
  (let* ((spec (cdr (assoc "elot_lint" elot-gptel--tool-specs)))
         (args (plist-get spec :args))
         (content-arg (cl-find-if
                       (lambda (a) (equal (plist-get a :name) "content"))
                       args)))
    (should content-arg)
    (should (eq (plist-get content-arg :type) 'string))
    (should (plist-get content-arg :optional))))

(ert-deftest elot-gptel-lint-content-test-omn-spec-declares-content-arg ()
  "The `elot_omn_validate' tool spec declares the optional `content' arg."
  (let* ((spec (cdr (assoc "elot_omn_validate" elot-gptel--tool-specs)))
         (args (plist-get spec :args))
         (content-arg (cl-find-if
                       (lambda (a) (equal (plist-get a :name) "content"))
                       args)))
    (should content-arg)
    (should (eq (plist-get content-arg :type) 'string))
    (should (plist-get content-arg :optional))))

(ert-deftest elot-gptel-lint-content-test-dispatcher-arity-lint ()
  "Dispatcher for elot-gptel-tool-lint accepts 4 args (file sev cats content)."
  (let* ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-lint))
         (default-directory elot-gptel-lint-content-test--repo-root)
         (content (elot-gptel-lint-content-test--read "minimal-ontology.org"))
         (out (funcall thunk "drafts/dispatch.org" "all" nil content)))
    (should (stringp out))
    (should-not (string-prefix-p "ERROR:" out))))

(ert-deftest elot-gptel-lint-content-test-dispatcher-arity-omn-validate ()
  "Dispatcher for elot-gptel-tool-omn-validate accepts (file profile content)."
  (let* ((thunk (elot-gptel--tool-thunk 'elot-gptel-tool-omn-validate)))
    (should (functionp thunk))
    ;; Pure-arity check: invoking against a nonexistent path with
    ;; CONTENT must yield a deterministic structured response (either
    ;; the no-ROBOT ERROR or, on machines with ROBOT, an OK / error
    ;; line) rather than a wrong-number-of-arguments backtrace.
    (let* ((default-directory elot-gptel-lint-content-test--repo-root)
           (content "* foo\n")
           (out (funcall thunk "drafts/scratch.org" nil content)))
      (should (stringp out)))))

(provide 'elot-gptel-lint-content-test)
;;; elot-gptel-lint-content-test.el ends here
