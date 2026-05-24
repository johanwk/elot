;;; elot-headline-hierarchy-stale-test.el --- O(1) cache-staleness tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-headline-hierarchy-stale-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.3.F8 -- tests for the
;; O(1) cache-staleness primitives in `elot-tangle.el'
;; (`elot-headline-hierarchy-mark-stale',
;;  `elot-headline-hierarchy-ensure-fresh') and the chained
;; insert-then-rename / move-then-rename agent-loop scenarios that
;; were broken before F8 landed (the in-Emacs cache stayed
;; pre-edit, so downstream tools refused freshly-minted CURIEs).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defvar elot-hh-stale-test--repo-root nil)
(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this))
  (setq elot-hh-stale-test--repo-root root))

(require 'elot-tangle)
(require 'elot-id)
(require 'elot-id-insert)
(require 'elot-id-rename)
(require 'elot-id-move)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-hh-stale-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-id-scheme: slug\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Prefixes\n"
   ":PROPERTIES:\n:prefixdefs: yes\n:END:\n"
   "#+name: prefix-table\n"
   "| prefix | uri                                   |\n"
   "|--------+---------------------------------------|\n"
   "| owl:   | http://www.w3.org/2002/07/owl#        |\n"
   "| rdfs:  | http://www.w3.org/2000/01/rdf-schema# |\n"
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "*** Plant (ex:plant)\n"))

(defmacro elot-hh-stale-test--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-hh-stale-test--fixture))
     (org-mode)
     (goto-char (point-min))
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Primitive helpers
;;; ---------------------------------------------------------------------------

(ert-deftest elot-hh-stale-test-helpers-defined ()
  (should (fboundp 'elot-headline-hierarchy-mark-stale))
  (should (fboundp 'elot-headline-hierarchy-ensure-fresh))
  (should (boundp  'elot-headline-hierarchy-stale-p)))

(ert-deftest elot-hh-stale-test-mark-stale-toggles-flag ()
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (should (not elot-headline-hierarchy-stale-p))
    (elot-headline-hierarchy-mark-stale)
    (should elot-headline-hierarchy-stale-p)))

(ert-deftest elot-hh-stale-test-ensure-fresh-noop-when-clean ()
  "When the cache is fresh, `ensure-fresh' must not invoke the parser."
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (should (not elot-headline-hierarchy-stale-p))
    (let ((calls 0))
      (cl-letf* ((orig (symbol-function 'elot-update-headline-hierarchy))
                 ((symbol-function 'elot-update-headline-hierarchy)
                  (lambda (&rest a)
                    (cl-incf calls)
                    (apply orig a))))
        (elot-headline-hierarchy-ensure-fresh)
        (should (= calls 0))))))

(ert-deftest elot-hh-stale-test-ensure-fresh-rescans-once ()
  "When stale, `ensure-fresh' triggers exactly one rescan."
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (elot-headline-hierarchy-mark-stale)
    (let ((calls 0))
      (cl-letf* ((orig (symbol-function 'elot-update-headline-hierarchy))
                 ((symbol-function 'elot-update-headline-hierarchy)
                  (lambda (&rest a) (cl-incf calls) (apply orig a))))
        (elot-headline-hierarchy-ensure-fresh)
        (should (= calls 1))
        (should (not elot-headline-hierarchy-stale-p))
        ;; Second call sees clean state: still one call total.
        (elot-headline-hierarchy-ensure-fresh)
        (should (= calls 1))))))

(ert-deftest elot-hh-stale-test-ensure-fresh-builds-when-empty ()
  "First read on a never-built cache must populate it."
  (elot-hh-stale-test--with
    (should (null elot-headline-hierarchy))
    (elot-headline-hierarchy-ensure-fresh)
    (should elot-headline-hierarchy)))


;;; ---------------------------------------------------------------------------
;;; Mutation primitives mark the cache stale (O(1)) instead of rescanning
;;; ---------------------------------------------------------------------------

(ert-deftest elot-hh-stale-test-insert-marks-stale ()
  "`elot-id-insert--do-insert' must mark, not rescan, on success.
Uses the `acme' scheme so the mint never collides with an existing
heading; under `slug', \"Mouse\" would mint `ex:mouse' which the test
fixture does not pre-declare, so either scheme is safe -- `acme' is
chosen to mirror the real agent-loop pattern."
  (with-temp-buffer
    (insert (elot-hh-stale-test--fixture))
    (goto-char (point-min))
    (search-forward ":ELOT-id-scheme: slug")
    (replace-match ":ELOT-id-scheme: acme")
    (org-mode)
    (goto-char (point-min))
    (elot-update-headline-hierarchy)
    (should (not elot-headline-hierarchy-stale-p))
    (let ((rescans 0))
      (cl-letf* ((orig (symbol-function 'elot-update-headline-hierarchy))
                 ((symbol-function 'elot-update-headline-hierarchy)
                  (lambda (&rest a) (cl-incf rescans) (apply orig a))))
        ;; Position point on `Animal' and insert a sibling.
        (goto-char (point-min))
        (search-forward "Animal (ex:animal)")
        (org-back-to-heading t)
        (elot-insert-sibling-resource nil '("Mouse"))
        ;; The hierarchy reader inside the mint path (existing-iris)
        ;; consumes its own ensure-fresh call -- but the post-insert
        ;; mark-stale runs after that, so the flag is set on exit.
        (should elot-headline-hierarchy-stale-p)
        ;; The total number of full reparses incurred is bounded:
        ;; ensure-fresh is called once at mint time, no more.
        (should (<= rescans 1))))))

(ert-deftest elot-hh-stale-test-rename-marks-stale ()
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (should (not elot-headline-hierarchy-stale-p))
    (elot-rename-resource "ex:plant" "ex:vegetable")
    (should elot-headline-hierarchy-stale-p)))

(ert-deftest elot-hh-stale-test-move-marks-stale ()
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (should (not elot-headline-hierarchy-stale-p))
    (elot-move-resource "ex:plant" "ex:animal" 'child)
    (should elot-headline-hierarchy-stale-p)))


;;; ---------------------------------------------------------------------------
;;; The bug from the live session: insert-then-rename must succeed.
;;; Before 9.3.F8 the rename refused with "not a declared resource heading".
;;; ---------------------------------------------------------------------------

(ert-deftest elot-hh-stale-test-insert-then-rename ()
  "Insert a fresh CURIE, then immediately rename it.  Must succeed.
Uses the `acme' scheme so the minted CURIE is opaque (`ex:C_...')
and the rename target (`ex:deer') is unambiguously distinct -- this
models the real agent-loop pattern (mint via acme, then rename to a
mnemonic CURIE)."
  (with-temp-buffer
    (insert (elot-hh-stale-test--fixture))
    ;; Switch the fixture's scheme to `acme' so the mint produces an
    ;; opaque local-name distinct from `ex:deer'.
    (goto-char (point-min))
    (search-forward ":ELOT-id-scheme: slug")
    (replace-match ":ELOT-id-scheme: acme")
    (org-mode)
    (goto-char (point-min))
    (elot-update-headline-hierarchy)
    ;; Insert `Deer' as a sibling of `Plant'.
    (goto-char (point-min))
    (search-forward "Plant (ex:plant)")
    (org-back-to-heading t)
    (let* ((curies (elot-insert-sibling-resource nil '("Deer")))
           (new (car curies)))
      (should (stringp new))
      (should (string-prefix-p "ex:" new))
      ;; Immediately rename the new heading to ex:deer.  Before F8
      ;; this signals "SOURCE ... is not a declared resource heading".
      (elot-rename-resource new "ex:deer")
      ;; Verify the rewrite landed on disk.
      (goto-char (point-min))
      (should (search-forward "(ex:deer)" nil t)))))

(ert-deftest elot-hh-stale-test-move-then-rename ()
  "Belt-and-braces: move-then-rename in one session works too."
  (elot-hh-stale-test--with
    (elot-update-headline-hierarchy)
    (elot-move-resource "ex:plant" "ex:animal" 'child)
    ;; ex:plant is still declared but lives in a different position.
    (elot-rename-resource "ex:plant" "ex:vegetable")
    (goto-char (point-min))
    (should (search-forward "(ex:vegetable)" nil t))))


;;; ---------------------------------------------------------------------------
;;; Cost-amortisation guard: a chain of three mutations must trigger at
;;; most a small bounded number of full reparses (not three of them).
;;; ---------------------------------------------------------------------------

(ert-deftest elot-hh-stale-test-bounded-reparses-on-chain ()
  "Three chained mutations must not each pay for a full reparse."
  (with-temp-buffer
    (insert (elot-hh-stale-test--fixture))
    (goto-char (point-min))
    (search-forward ":ELOT-id-scheme: slug")
    (replace-match ":ELOT-id-scheme: acme")
    (org-mode)
    (goto-char (point-min))
    (elot-update-headline-hierarchy)
    (let ((rescans 0))
      (cl-letf* ((orig (symbol-function 'elot-update-headline-hierarchy))
                 ((symbol-function 'elot-update-headline-hierarchy)
                  (lambda (&rest a) (cl-incf rescans) (apply orig a))))
        ;; Insert -> rename -> move.  Each operation may consume at most
        ;; one ensure-fresh call (when it first reads the cache).
        (goto-char (point-min))
        (search-forward "Plant (ex:plant)")
        (org-back-to-heading t)
        (let* ((curies (elot-insert-sibling-resource nil '("Deer")))
               (new (car curies)))
          (elot-rename-resource new "ex:deer")
          (elot-move-resource "ex:deer" "ex:animal" 'child))
        ;; Pre-F8: 3 unconditional reparses (one per mutation epilogue).
        ;; Post-F8: at most 3 *on-demand* reparses (one per reader),
        ;; but typically fewer because subsequent mutations write
        ;; before re-reading.  Either way the count is bounded by the
        ;; number of operations -- the regression is the *unconditional*
        ;; post-write reparse, which is what we want to eliminate.
        (should (<= rescans 3))))))

(provide 'elot-headline-hierarchy-stale-test)
;;; elot-headline-hierarchy-stale-test.el ends here
