;;; elot-gptel-delete-apply-test.el --- M9.9 Slice B.1 core primitive tests  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-gptel-delete-apply-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 9 Step 9.9 Slice B.1 -- tests for
;; the `elot-gptel--delete-apply' core primitive.  Pure buffer
;; mutator: locates SUBJECT's heading via the rename module's
;; marker helper, then dispatches to either
;; `elot-id-remove-heading-promote-children' (cascade=reparent) or
;; `elot-id-delete-heading-subtree' (cascade=delete).  No tool
;; spec, no reference scan, no revalidate -- those layers are
;; B.2's job.
;;
;; Covers:
;;   - cascade=reparent: heading + payload gone, direct children
;;     promoted one outline level, descendants of those children
;;     ride along intact, return plist carries :promoted-children
;;   - cascade=reparent on a leaf is a clean no-op surgery
;;     (no children to promote; heading removed)
;;   - cascade=delete: heading + entire subtree gone, return plist
;;     carries :deleted-descendants
;;   - cascade=delete on a leaf reports zero descendants
;;   - malformed cascade signals
;;   - unknown subject signals
;;   - atomicity: an error mid-surgery rolls back via
;;     `atomic-change-group' (the primitive provides its own).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(let* ((this (or load-file-name buffer-file-name))
       (root (file-name-directory
              (directory-file-name (file-name-directory this)))))
  (add-to-list 'load-path (expand-file-name "elot-package" root))
  (add-to-list 'load-path (file-name-directory this)))

(require 'elot-tangle)
(require 'elot-id-move)
(require 'elot-id-rename)
(require 'elot-gptel)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-gptel-da-test--fixture ()
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
   "| ex:    | http://example.org/                   |\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "*** Dog (ex:dog)\n"
   " - SubClassOf :: ex:animal\n"
   " - rdfs:comment :: friendly\n"
   "**** Puppy (ex:puppy)\n"
   "**** Beagle (ex:beagle)\n"
   "***** TinyBeagle (ex:tinyBeagle)\n"
   "*** Cat (ex:cat)\n"))

(defmacro elot-gptel-da-test--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-gptel-da-test--fixture))
     (org-mode)
     (elot-update-headline-hierarchy)
     (goto-char (point-min))
     ,@body))


;;; ---------------------------------------------------------------------------
;;; Defined / dispatch
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-da-test-defined ()
  (should (fboundp 'elot-gptel--delete-apply))
  (should (fboundp 'elot-id-remove-heading-promote-children))
  (should (fboundp 'elot-id-delete-heading-subtree)))

(ert-deftest elot-gptel-da-test-rejects-bad-cascade ()
  (elot-gptel-da-test--with
    (should-error (elot-gptel--delete-apply "ex:dog" 'bogus))))

(ert-deftest elot-gptel-da-test-rejects-unknown-subject ()
  (elot-gptel-da-test--with
    (should-error (elot-gptel--delete-apply "ex:nope" 'reparent))))


;;; ---------------------------------------------------------------------------
;;; cascade=reparent
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-da-test-reparent-leaf ()
  "Reparent on a leaf removes the heading; no children to promote."
  (elot-gptel-da-test--with
    (let ((r (elot-gptel--delete-apply "ex:cat" 'reparent)))
      (should (eq (plist-get r :cascade) 'reparent))
      (should (= 0 (plist-get r :promoted-children)))
      (goto-char (point-min))
      (should-not (re-search-forward "(ex:cat)" nil t)))))

(ert-deftest elot-gptel-da-test-reparent-promotes-children ()
  "Reparent on Dog promotes Puppy and Beagle one outline level
so they sit alongside Dog's former siblings (Animal, Cat).
Beagle's child TinyBeagle rides along intact at one level
shallower than before."
  (elot-gptel-da-test--with
    (let ((r (elot-gptel--delete-apply "ex:dog" 'reparent)))
      (should (eq (plist-get r :cascade) 'reparent))
      (should (= 2 (plist-get r :promoted-children)))
      ;; Dog's heading is gone.
      (goto-char (point-min))
      (should-not (re-search-forward "^\\*+ Dog " nil t))
      ;; Puppy and Beagle now sit at the level Dog used to (level 3).
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* Puppy " nil t))
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\* Beagle " nil t))
      ;; TinyBeagle stays nested under Beagle, now at level 4.
      (goto-char (point-min))
      (should (re-search-forward "^\\*\\*\\*\\* TinyBeagle " nil t)))))


;;; ---------------------------------------------------------------------------
;;; cascade=delete
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-da-test-delete-leaf ()
  "Delete on a leaf reports zero descendants."
  (elot-gptel-da-test--with
    (let ((r (elot-gptel--delete-apply "ex:cat" 'delete)))
      (should (eq (plist-get r :cascade) 'delete))
      (should (= 0 (plist-get r :deleted-descendants)))
      (goto-char (point-min))
      (should-not (re-search-forward "(ex:cat)" nil t)))))

(ert-deftest elot-gptel-da-test-delete-subtree ()
  "Delete on Dog removes Dog and every descendant heading."
  (elot-gptel-da-test--with
    (let ((r (elot-gptel--delete-apply "ex:dog" 'delete)))
      (should (eq (plist-get r :cascade) 'delete))
      ;; Three descendants: Puppy, Beagle, TinyBeagle.
      (should (= 3 (plist-get r :deleted-descendants)))
      (dolist (gone '("ex:dog" "ex:puppy" "ex:beagle" "ex:tinyBeagle"))
        (goto-char (point-min))
        (should-not (re-search-forward (regexp-quote gone) nil t)))
      ;; Sibling headings are untouched.
      (goto-char (point-min))
      (should (re-search-forward "(ex:animal)" nil t))
      (goto-char (point-min))
      (should (re-search-forward "(ex:cat)" nil t)))))


;;; ---------------------------------------------------------------------------
;;; Atomicity
;;; ---------------------------------------------------------------------------

(ert-deftest elot-gptel-da-test-atomic-rollback-on-signal ()
  "Signal raised inside the helper rolls the buffer back to its
pre-call contents via `atomic-change-group'."
  (elot-gptel-da-test--with
    (let ((before (buffer-substring-no-properties (point-min) (point-max))))
      ;; Stub the subtree-deletion helper to raise mid-surgery.  The
      ;; primitive's own `atomic-change-group' must roll back any
      ;; modification already applied by the time the signal fires.
      (cl-letf (((symbol-function 'elot-id-delete-heading-subtree)
                 (lambda (_m)
                   (insert "JUNK\n")
                   (error "boom"))))
        (should-error (elot-gptel--delete-apply "ex:dog" 'delete)))
      (should (string= before
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))))


;;; elot-gptel-delete-apply-test.el ends here
