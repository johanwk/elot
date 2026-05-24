;;; elot-move-resource-test.el --- Tests for elot-move-resource  -*- lexical-binding: t; -*-

;; Usage:  cd test && emacs --batch -L ../elot-package -L . \
;;           -l ert -l elot-move-resource-test.el \
;;           -f ert-run-tests-batch-and-exit

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.1 -- tests for the
;; resource-move command `elot-move-resource'.  Pure-Elisp; no ROBOT
;; required.  The fixture is a minimal ELOT ontology with two
;; classes, an object property, a datatype and an individual, so
;; the section-kind compatibility table is exercised.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(defconst elot-id-move-test--repo-root
  (let ((this (or load-file-name buffer-file-name
                  (locate-library "elot-move-resource-test"))))
    (unless this
      (error "elot-move-resource-test: cannot determine source location"))
    (file-name-directory
     (directory-file-name
      (file-name-directory (file-truename this))))))

(add-to-list 'load-path
             (expand-file-name "elot-package" elot-id-move-test--repo-root))

(require 'elot-id-insert)
(require 'elot-id-move)


;;; ---------------------------------------------------------------------------
;;; Fixture
;;; ---------------------------------------------------------------------------

(defun elot-id-move-test--fixture ()
  (concat
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal)\n"
   "**** Dog (ex:dog)\n"
   "**** Cat (ex:cat)\n"
   "*** Plant (ex:plant)\n"
   "** Object properties\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-object-property-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** chases (ex:chases)\n"
   "** Datatypes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-datatypes\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** myType (ex:myType)\n"
   "*** otherType (ex:otherType)\n"
   "** Individuals\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-individuals\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** scooby (ex:scooby)\n"))

(defmacro elot-id-move-test--with (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-id-move-test--fixture))
     (org-mode)
     (goto-char (point-min))
     (require 'elot-tangle nil 'noerror)
     ,@body))

(defun elot-id-move-test--heading-line (curie)
  "Return the heading line (with stars) for CURIE, or nil.
Tolerates trailing Org statistics cookies and tag strings via
the shared `elot-id-heading-curie-regexp' helper."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (elot-id-heading-curie-regexp curie) nil t)
         (buffer-substring-no-properties
          (line-beginning-position) (line-end-position)))))

(defun elot-id-move-test--heading-level (curie)
  (let ((line (elot-id-move-test--heading-line curie)))
    (and line
         (string-match "^\\(\\*+\\)" line)
         (length (match-string 1 line)))))

(defun elot-id-move-test--parent-curie (curie)
  "Return the CURIE of the parent heading of CURIE, or `:section: SECTION'.
Tolerates trailing Org statistics cookies and tag strings on the
target heading via `elot-id-heading-curie-regexp'."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward (elot-id-heading-curie-regexp curie) nil t)
      (error "elot-id-move-test--parent-curie: CURIE %s not found" curie))
    (org-back-to-heading t)
    (org-up-heading-safe)
    (let ((title (org-get-heading t t t t)))
      (cond
       ;; Recognise `Label (curie) [cookie]? :tags:?' on the parent.
       ((string-match
         "(\\([A-Za-z_][A-Za-z0-9_-]*:[A-Za-z_][A-Za-z0-9_.-]*\\))"
         title)
        (match-string 1 title))
       (t
        (concat ":section: " title))))))


;;; ---------------------------------------------------------------------------
;;; Validation / refusal paths
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-move-test-source-not-declared ()
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:nope" "ex:cat")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-target-not-declared ()
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:dog" "ex:nope")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-malformed-target ()
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:dog" "not-a-curie")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-source-equals-target ()
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:dog" "ex:dog")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-kind-mismatch-class-to-property ()
  "A class moved under an object property is refused."
  (elot-id-move-test--with
    (let ((err (should-error (elot-move-resource "ex:dog" "ex:chases")
                             :type 'user-error)))
      (should (string-match-p "kind mismatch" (cadr err))))))

(ert-deftest elot-id-move-test-no-op-already-parent ()
  "Moving Dog under Animal when Animal is already Dog's parent is refused."
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:dog" "ex:animal")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-into-own-subtree ()
  "Moving Animal into its child Dog is refused (would orphan)."
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:animal" "ex:dog")
                  :type 'user-error)))

(ert-deftest elot-id-move-test-datatype-child-refused ()
  "Under Datatypes, AS=child with a level-3 target is refused."
  (elot-id-move-test--with
    (should-error (elot-move-resource "ex:myType" "ex:otherType" 'child)
                  :type 'user-error)))


;;; ---------------------------------------------------------------------------
;;; Happy paths
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-move-test-move-class-as-child ()
  "Move ex:plant under ex:animal as a child (level 3 -> level 4)."
  (elot-id-move-test--with
    (should (= 3 (elot-id-move-test--heading-level "ex:plant")))
    (elot-move-resource "ex:plant" "ex:animal" 'child)
    (should (= 4 (elot-id-move-test--heading-level "ex:plant")))
    (should (string= "ex:animal"
                     (elot-id-move-test--parent-curie "ex:plant")))))

(ert-deftest elot-id-move-test-move-class-as-sibling ()
  "Move ex:dog as a sibling of ex:plant -- both end at level 3 under Classes."
  (elot-id-move-test--with
    (should (= 4 (elot-id-move-test--heading-level "ex:dog")))
    (elot-move-resource "ex:dog" "ex:plant" 'sibling)
    (should (= 3 (elot-id-move-test--heading-level "ex:dog")))
    (should (string= ":section: Classes"
                     (elot-id-move-test--parent-curie "ex:dog")))))

(ert-deftest elot-id-move-test-move-to-top-of-section ()
  "TARGET=\"top\" places the moved subtree under the section heading."
  (elot-id-move-test--with
    (elot-move-resource "ex:dog" "top")
    (should (= 3 (elot-id-move-test--heading-level "ex:dog")))
    (should (string= ":section: Classes"
                     (elot-id-move-test--parent-curie "ex:dog")))))

(ert-deftest elot-id-move-test-move-to-top-symbol ()
  "TARGET can also be the symbol `top'."
  (elot-id-move-test--with
    (elot-move-resource "ex:dog" 'top)
    (should (= 3 (elot-id-move-test--heading-level "ex:dog")))))

(ert-deftest elot-id-move-test-datatype-to-top-allowed ()
  "Datatype move to TARGET=\"top\" is permitted (the section-root
exemption -- under Datatypes, only the section-root form of the
child placement is allowed)."
  (elot-id-move-test--with
    (elot-move-resource "ex:otherType" "top")
    (should (= 3 (elot-id-move-test--heading-level "ex:otherType")))))


;;; ---------------------------------------------------------------------------
;;; Atomicity + hierarchy refresh
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-move-test-atomic-rollback ()
  "When the paste step signals, the buffer is restored to its pre-move state."
  (elot-id-move-test--with
    ;; `atomic-change-group' rolls back via the undo list, which is
    ;; disabled by default in temp buffers; enable it here.
    (buffer-enable-undo)
    (let ((before (buffer-string)))
      (cl-letf (((symbol-function 'org-paste-subtree)
                 (lambda (&rest _args)
                   (error "simulated paste failure"))))
        (should-error (elot-move-resource "ex:dog" "ex:plant" 'sibling)))
      (should (string= before (buffer-string))))))

(ert-deftest elot-id-move-test-hierarchy-refreshed ()
  "After a successful move, `elot-headline-hierarchy' reflects the new shape.
M9.3.F8: move now only marks the cache stale; readers refresh on
demand via `elot-headline-hierarchy-ensure-fresh', which this test
calls explicitly before walking the cached structure."
  (skip-unless (fboundp 'elot-update-headline-hierarchy))
  (elot-id-move-test--with
    (elot-update-headline-hierarchy)
    (elot-move-resource "ex:dog" "ex:plant" 'sibling)
    (when (fboundp 'elot-headline-hierarchy-ensure-fresh)
      (elot-headline-hierarchy-ensure-fresh))
    ;; After the move, walking the hierarchy must find ex:dog still
    ;; declared (and at the new level).
    (let (found-level)
      (cl-labels ((walk (n)
                    (let ((uri (plist-get n :uri)))
                      (when (and uri
                                 (equal (car (split-string uri "[ \t]+" t))
                                        "ex:dog"))
                        (setq found-level (plist-get n :level))))
                    (mapc #'walk (plist-get n :children))))
        (walk elot-headline-hierarchy))
      (should (= found-level 3)))))


;;; ---------------------------------------------------------------------------
;;; Plist return value
;;; ---------------------------------------------------------------------------

(ert-deftest elot-id-move-test-return-plist ()
  (elot-id-move-test--with
    (let ((res (elot-move-resource "ex:plant" "ex:animal" 'child)))
      (should (equal (plist-get res :source) "ex:plant"))
      (should (equal (plist-get res :target) "ex:animal"))
      (should (eq    (plist-get res :as) 'child))
      (should (string-match-p "Animal" (plist-get res :to-parent))))))


;;; ---------------------------------------------------------------------------
;;; Heading-decoration tolerance (statistics cookies, tags, TODO keywords)
;;; ---------------------------------------------------------------------------
;;;
;;; The CURIE-lookup regex must match resource headings even when Org
;;; has decorated them with a trailing statistics cookie (e.g.
;;; `Dog (ex:dog) [2/4]'), an Org tag string
;;; (`Dog (ex:dog) :mytag:'), or a leading TODO keyword.  Earlier
;;; versions required the line to /end/ with `)' and silently refused
;;; to find such headings -- the LLM acceptance test on pets.org hit
;;; this on Dog (ex:dog) [2/4].

(defun elot-id-move-test--decorated-fixture ()
  "Fixture with cookies / tags / TODO keywords on the target headings."
  (concat
   "#+TODO: TODO DONE\n"
   "* my-ont\n"
   ":PROPERTIES:\n"
   ":ID: my-ont\n"
   ":ELOT-context-type: ontology\n"
   ":ELOT-context-localname: my-ont\n"
   ":ELOT-default-prefix: ex\n"
   ":END:\n"
   "** Classes\n"
   ":PROPERTIES:\n"
   ":ID:       my-ont-class-hierarchy\n"
   ":resourcedefs: yes\n"
   ":END:\n"
   "*** Animal (ex:animal) [3/3]\n"
   "**** Dog (ex:dog) [2/4]\n"
   "**** Cat (ex:cat) :pet:fluffy:\n"
   "**** TODO Snake (ex:snake)\n"
   "*** Plant (ex:plant) [0/0]    :flora:\n"))

(defmacro elot-id-move-test--with-decorated (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert (elot-id-move-test--decorated-fixture))
     (org-mode)
     (goto-char (point-min))
     (require 'elot-tangle nil 'noerror)
     ,@body))

(ert-deftest elot-id-move-test-target-with-statistics-cookie ()
  "TARGET heading carrying a `[N/M]' cookie is still found."
  (elot-id-move-test--with-decorated
    (elot-move-resource "ex:cat" "ex:dog" 'child)
    (should (equal (elot-id-move-test--parent-curie "ex:cat") "ex:dog"))))

(ert-deftest elot-id-move-test-target-with-org-tags ()
  "TARGET heading carrying `:tag1:tag2:' is still found."
  (elot-id-move-test--with-decorated
    (elot-move-resource "ex:dog" "ex:cat" 'child)
    (should (equal (elot-id-move-test--parent-curie "ex:dog") "ex:cat"))))

(ert-deftest elot-id-move-test-source-with-todo-keyword ()
  "SOURCE heading carrying a `TODO' keyword is still found."
  (elot-id-move-test--with-decorated
    (elot-move-resource "ex:snake" "ex:cat" 'child)
    (should (equal (elot-id-move-test--parent-curie "ex:snake") "ex:cat"))))

(ert-deftest elot-id-move-test-target-cookie-and-tags ()
  "TARGET heading carrying both a cookie and a tag string is found."
  (elot-id-move-test--with-decorated
    (elot-move-resource "ex:dog" "ex:plant" 'child)
    (should (equal (elot-id-move-test--parent-curie "ex:dog") "ex:plant"))))

(ert-deftest elot-id-move-test-heading-regex-helper ()
  "`elot-id-heading-curie-regexp' matches the decorated heading shapes."
  (require 'elot-id)
  (let ((re-dog   (elot-id-heading-curie-regexp "ex:dog"))
        (re-cat   (elot-id-heading-curie-regexp "ex:cat"))
        (re-snake (elot-id-heading-curie-regexp "ex:snake"))
        (re-plant (elot-id-heading-curie-regexp "ex:plant")))
    ;; cookie
    (should (string-match-p re-dog   "**** Dog (ex:dog) [2/4]"))
    ;; tag
    (should (string-match-p re-cat   "**** Cat (ex:cat) :pet:fluffy:"))
    ;; TODO keyword
    (should (string-match-p re-snake "**** TODO Snake (ex:snake)"))
    ;; cookie + tag
    (should (string-match-p re-plant
                            "*** Plant (ex:plant) [0/0]    :flora:"))
    ;; Plain shape still matches (no decoration).
    (should (string-match-p re-dog "**** Dog (ex:dog)"))
    ;; Negative: the CURIE must be a whole parenthetical -- a
    ;; longer-local-name CURIE that happens to share the prefix
    ;; must not match.
    (should-not (string-match-p re-dog "**** Doggish (ex:doggish) [0/0]"))))


(provide 'elot-move-resource-test)
;;; elot-move-resource-test.el ends here
