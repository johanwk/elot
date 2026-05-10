;;; elot-sparql-roundtrip-test.el --- End-to-end SPARQL/ROBOT tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; ELPA-SUBMISSION-PLAN.org Milestone 1, Sub-step 1.6.2.
;;
;; End-to-end regression tests for ELOT's SPARQL advice against a
;; real ROBOT invocation, driven by the fixture
;; `test/fixtures/sparql-roundtrip.org' (Sub-step 1.6.1).
;;
;; The fixture is copied into a temp directory together with its
;; pre-tangled `my-ont.omn' (so we don't need to drive
;; `org-babel-tangle' from a non-Org context, and the source tree
;; stays clean).  For each of the seven `Block N:' source blocks
;; we capture the authored `#+RESULTS' as the expectation, force
;; re-execution via `org-babel-execute-src-block', and compare the
;; new `#+RESULTS' against the expectation by checking semantic
;; content (substring matching) rather than exact whitespace.
;;
;; All tests are gated on:
;;   - `ob-sparql' / `sparql-mode' being loadable (otherwise
;;     `org-babel-execute:sparql' is undefined and there is
;;     nothing for the ELOT advice to wrap);
;;   - sqlite support (transitively required by elot-mode setup);
;;   - `elot-robot-available-p' (Java + ROBOT jar / executable).
;;
;; They skip cleanly on CI runners that do not have ROBOT or
;; that lack the optional `sparql-mode' / `ob-sparql' packages.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

;; `emacs --batch' does not initialise package.el, so ELPA-installed
;; libraries like `sparql-mode' / `ob-sparql' are not on `load-path'
;; out of the box.  Pull them in explicitly so the round-trip tests
;; do not skip on a developer machine that has them installed via
;; the user's normal package manager.  This is a no-op when
;; package.el is unavailable or when the packages are already on
;; the load path (e.g. installed system-wide).
(require 'package)
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(require 'sparql-mode nil t)
(require 'ob-sparql nil t)
(require 'elot)
(require 'elot-mode)
(require 'elot-sources)  ; for `elot-robot-available-p'

(defvar elot-spxrt-test--fixture-src
  (expand-file-name "fixtures/sparql-roundtrip.org"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the checked-in SPARQL round-trip fixture.")

(defvar elot-spxrt-test--omn-src
  (expand-file-name "fixtures/my-ont.omn"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to the pre-tangled OMN that the fixture queries via :url.")


;;;; Skip helpers

(defun elot-spxrt-test--ob-sparql-or-skip ()
  "Skip the current ERT test unless `ob-sparql' is loadable.
Without it, `org-babel-execute:sparql' is unbound and the ELOT
advice has nothing to wrap, so the round-trip cannot run."
  (unless (and (require 'sparql-mode nil t)
               (require 'ob-sparql  nil t)
               (fboundp 'org-babel-execute:sparql))
    (ert-skip "ob-sparql / sparql-mode not available")))

(defun elot-spxrt-test--robot-or-skip ()
  "Skip the current ERT test unless ROBOT is available."
  (elot-spxrt-test--ob-sparql-or-skip)
  (unless (and (fboundp 'elot-robot-available-p)
               (elot-robot-available-p))
    (ert-skip "ROBOT not available (set elot-robot-jar-path or install robot)")))


;;;; Workspace setup / teardown

(defun elot-spxrt-test--make-workspace ()
  "Copy the fixture and its pre-tangled OMN into a fresh temp dir.
Return the absolute path of the copied .org file."
  (let* ((dir (make-temp-file "elot-spxrt-" t))
         (org (expand-file-name "sparql-roundtrip.org" dir))
         (omn (expand-file-name "my-ont.omn"           dir)))
    (copy-file elot-spxrt-test--fixture-src org)
    (copy-file elot-spxrt-test--omn-src     omn)
    org))

(defun elot-spxrt-test--cleanup-workspace (org-path)
  "Delete the workspace directory containing ORG-PATH."
  (when (and org-path (file-exists-p org-path))
    (let ((dir (file-name-directory org-path)))
      (when (and dir (file-directory-p dir)
                 (string-match-p "elot-spxrt-" dir))
        (delete-directory dir t)))))


;;;; Block navigation and result extraction

(defun elot-spxrt-test--goto-block (n)
  "Move point to the start of the SPARQL src block under heading `Block N:'."
  (goto-char (point-min))
  (unless (re-search-forward (format "^\\*+ Block %d:" n) nil t)
    (error "Could not find heading for Block %d" n))
  (unless (re-search-forward "^[ \t]*#\\+begin_src sparql" nil t)
    (error "Could not find #+begin_src sparql under Block %d" n))
  (beginning-of-line))

(defun elot-spxrt-test--result-text ()
  "Return the textual content of the `#+RESULTS' block at point.
Point must be on a `#+begin_src' line.  Returns nil when there is
no result yet."
  (let ((res-pos (save-excursion (org-babel-where-is-src-block-result))))
    (when res-pos
      (save-excursion
        (goto-char res-pos)
        (forward-line 1)                ; past the #+RESULTS[: hash]: line
        (let ((start (point)))
          (cond
           ;; Wrapped result: #+begin_src ttl ... #+end_src
           ((looking-at "[ \t]*#\\+begin_src")
            (forward-line 1)
            (let ((body-start (point)))
              (re-search-forward "^[ \t]*#\\+end_src" nil t)
              (beginning-of-line)
              (buffer-substring-no-properties body-start (point))))
           ;; Table / scalar / example: read until blank line, next
           ;; heading, or next `#+'.
           (t
            (while (and (not (eobp))
                        (looking-at
                         "^\\(:\\| *|\\|[ \t]*$\\)")
                        (not (looking-at "^\\*+ "))
                        (not (looking-at "^[ \t]*#\\+")))
              (forward-line 1))
            ;; Trim a single trailing blank line that comes back as
            ;; part of the match above.
            (let ((end (point)))
              (when (and (> end start)
                         (save-excursion
                           (goto-char (1- end))
                           (looking-at "\n")))
                (setq end (1- end)))
              (buffer-substring-no-properties start end)))))))))

(defun elot-spxrt-test--execute-block-at-point (&optional capture-warnings)
  "Force-execute the src block starting at point.
When CAPTURE-WARNINGS is non-nil, return a list of
`display-warning' triples (TYPE MESSAGE LEVEL) emitted during the
execution; otherwise return nil."
  (if capture-warnings
      (let ((warnings nil))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &optional level &rest _)
                     (push (list type message level) warnings))))
          ;; First arg non-nil forces re-execution (defeats :cache).
          (org-babel-execute-src-block t))
        (nreverse warnings))
    (org-babel-execute-src-block t)
    nil))


;;;; Test driver

(defmacro elot-spxrt-test--with-fixture (&rest body)
  "Open a fresh copy of the fixture, enable `elot-mode', run BODY."
  (declare (indent 0))
  `(let* ((org-path (elot-spxrt-test--make-workspace))
          (org-confirm-babel-evaluate nil)
          (buf (find-file-noselect org-path)))
     (unwind-protect
         (with-current-buffer buf
           (org-mode)
           ;; `elot-mode' sets up `org-link-abbrev-alist-local' from
           ;; the buffer's `prefix-table' and installs the SPARQL
           ;; advice for the lifetime of this buffer.
           (elot-mode 1)
           ,@body)
       (with-current-buffer buf
         (set-buffer-modified-p nil))
       (kill-buffer buf)
       (elot-spxrt-test--cleanup-workspace org-path))))


;;;; Tests

(ert-deftest elot-spxrt/block-1-bare-select ()
  "Block 1: bare SELECT with no inline PREFIX returns the chases triple."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 1)
    (elot-spxrt-test--execute-block-at-point)
    (let ((result (elot-spxrt-test--result-text)))
      (should result)
      (should (string-match-p "ex:chases" result))
      (should (string-match-p "ex:dog"    result))
      (should (string-match-p "ex:cat"    result)))))

(ert-deftest elot-spxrt/block-2-inline-agreeing-prefix-dedup ()
  "Block 2: inline PREFIX matching the alist is silently de-duplicated."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 2)
    (let ((warnings (elot-spxrt-test--execute-block-at-point t))
          (result   (elot-spxrt-test--result-text)))
      (should result)
      (should (string-match-p "ex:chases" result))
      (should (string-match-p "ex:dog"    result))
      (should (string-match-p "ex:cat"    result))
      ;; Identical re-declarations must not warn.
      (should (null (cl-remove-if-not
                     (lambda (w) (eq (car w) 'elot-sparql))
                     warnings))))))

(ert-deftest elot-spxrt/block-3-inline-overrides-with-warning ()
  "Block 3: inline PREFIX overriding rdfs: warns and yields no domain/range."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 3)
    (let ((warnings (elot-spxrt-test--execute-block-at-point t))
          (result   (elot-spxrt-test--result-text)))
      (should result)
      ;; The chases OP itself still surfaces (rdf:type uses owl:, not rdfs:).
      (should (string-match-p "ex:chases" result))
      ;; But domain/range come back unbound — no ex:dog / ex:cat anywhere.
      (should-not (string-match-p "ex:dog" result))
      (should-not (string-match-p "ex:cat" result))
      ;; And we must have surfaced an `elot-sparql' warning naming rdfs.
      (let ((relevant (cl-remove-if-not
                       (lambda (w) (eq (car w) 'elot-sparql))
                       warnings)))
        (should (= 1 (length relevant)))
        (should (string-match-p "rdfs" (nth 1 (car relevant))))))))

(ert-deftest elot-spxrt/block-4-unused-override-noop ()
  "Block 4: inline PREFIX redeclaring `ex:' changes only result-IRI shortening.
The body does not use `ex:' as a prefix in any triple pattern, so the
matched triples are the same as Block 1.  However, the override does
take effect when ob-sparql shortens the result IRIs: with `ex:' rebound
to a different URI, `http://example.org/...' values no longer match the
new `ex:' namespace and are therefore emitted as full IRIs in the
result table."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 4)
    (elot-spxrt-test--execute-block-at-point t)
    (let ((result (elot-spxrt-test--result-text)))
      (should result)
      ;; Full IRIs from the original `ex:' namespace surface verbatim.
      (should (string-match-p "http://example\\.org/chases" result))
      (should (string-match-p "http://example\\.org/dog"    result))
      (should (string-match-p "http://example\\.org/cat"    result))
      ;; And `ex:'-shortened forms must NOT appear, since the inline
      ;; override rebinds `ex:' to a different URI.
      (should-not (string-match-p "\\bex:chases\\b" result))
      (should-not (string-match-p "\\bex:dog\\b"    result))
      (should-not (string-match-p "\\bex:cat\\b"    result)))))

(ert-deftest elot-spxrt/block-5-construct-ttl-with-kill-prefixes ()
  "Block 5: CONSTRUCT with :format ttl and :post kill-prefixes."
  (elot-spxrt-test--robot-or-skip)
  ;; The :post hook calls the `kill-prefixes' Library-of-Babel block
  ;; (see elot-package/elot-lob.org).  Skip cleanly if the LOB block
  ;; is not reachable in this Emacs (e.g. running tests without
  ;; first having opened any ELOT buffer that ingested it).
  (let ((lob-file (and (fboundp 'locate-library)
                       (let ((lib (locate-library "elot-tangle")))
                         (and lib (concat (file-name-directory lib)
                                          "elot-lob.org"))))))
    (when (and lob-file (file-exists-p lob-file))
      (org-babel-lob-ingest lob-file)))
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 5)
    (elot-spxrt-test--execute-block-at-point)
    (let ((result (elot-spxrt-test--result-text)))
      (should result)
      ;; CONSTRUCTed triples must surface in the wrapped TTL block.
      (should (string-match-p "ex:chases"           result))
      (should (string-match-p "owl:ObjectProperty"  result))
      (should (string-match-p "ex:animal"           result))
      (should (string-match-p "rdfs:subClassOf"     result))
      ;; kill-prefixes strips @prefix declarations.
      (should-not (string-match-p "^[ \t]*@?prefix" result)))))

(ert-deftest elot-spxrt/block-6-empty-result-sentinel ()
  "Block 6: a SELECT with no rows returns the empty-result sentinel."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 6)
    (elot-spxrt-test--execute-block-at-point)
    (let ((result (elot-spxrt-test--result-text)))
      (should result)
      (should (string-match-p
               (regexp-quote elot--sparql-empty-result-sentinel)
               result)))))

(ert-deftest elot-spxrt/block-7-genuinely-new-namespace ()
  "Block 7: an inline-only PREFIX is preserved and shortens result IRIs."
  (elot-spxrt-test--robot-or-skip)
  (elot-spxrt-test--with-fixture
    (elot-spxrt-test--goto-block 7)
    (elot-spxrt-test--execute-block-at-point)
    (let ((result (elot-spxrt-test--result-text)))
      (should result)
      ;; Inline-declared `new:' must participate in result-IRI
      ;; shortening (Decisions log entry from Sub-step 1.6.1).
      (should (string-match-p "new:A"     result))
      (should (string-match-p "new:B"     result))
      (should (string-match-p "ex:chases" result))
      ;; The full IRI must NOT leak through unshortened.
      (should-not (string-match-p "http://it-is-new.com/" result)))))


(provide 'elot-sparql-roundtrip-test)
;;; elot-sparql-roundtrip-test.el ends here
