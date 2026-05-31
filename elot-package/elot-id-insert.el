;;; elot-id-insert.el --- Interactive insert-resource commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Kluwer

;; Author: Johan W. Kluwer
;; URL: https://github.com/johanwk/elot
;; Keywords: languages tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 10 Step 10.6 -- interactive
;; insert-resource commands for ELOT outlines.
;;
;; Two interactive entry points:
;;
;;   M-x elot-insert-sibling-resource
;;   M-x elot-insert-child-resource
;;
;; Both accept a numeric prefix argument N (default 1) and insert N
;; new resource headings.  Before any identifier is minted, the user
;; is prompted in the minibuffer for one rdfs:label per heading.
;; The new heading takes the form
;;
;;     **** Label (prefix:localname)
;;
;; The label is fed into `elot-id-mint-batch' as `:labels' -- this
;; matters for the `slug' and `acme slug:t' schemes, whose local
;; names (and, for acme, the checksum) derive from the label.  Point
;; is left at end of the first inserted heading line.
;;
;; Kind is inferred from the level-2 ELOT resource section the
;; current heading sits under (Classes -> class, Object properties ->
;; object-property, ...).  `elot-insert-child-resource' refuses
;; under Datatypes (no inherent subtype relation among datatypes),
;; *except* when invoked directly on the section heading itself --
;; in which case the new "first child" is the first level-3 resource
;; of that section (the natural way to seed an empty section).
;;
;; If the heading at point carries a description list (`- key ::
;; value' rows), each inserted heading inherits a blank-valued copy
;; of that list (values stripped, checkboxes reset to `[ ]').
;; Nested annotation rows (the indented `- rdfs:comment ::' rows
;; under a `Domain' axiom) are *not* replicated.
;;
;; A tree-builder entry point composes the two primitives:
;;
;;   M-x elot-insert-labels-tree
;;
;; takes a list of nodes -- each either a bare LABEL string or
;; a (LABEL CHILD...) list -- and recursively builds an outline.
;; See its docstring for the shape and an example.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'elot-id)
(require 'elot-tangle nil 'noerror)

;; M9.3.F8: O(1) staleness markers for the shared headline-hierarchy
;; cache.  Defined in `elot-tangle.el'; declared here so byte-compile
;; is silent even when that module loads lazily.
(declare-function elot-headline-hierarchy-mark-stale "elot-tangle" ())
(declare-function elot-headline-hierarchy-ensure-fresh "elot-tangle" ())
(declare-function elot-update-headline-hierarchy "elot-tangle" ())
(defvar elot-headline-hierarchy)

(defconst elot-id-insert--section-kind-alist
  '(("Classes"               . class)
    ("Object properties"     . object-property)
    ("Data properties"       . data-property)
    ("Annotation properties" . annotation-property)
    ("Datatypes"             . datatype)
    ("Individuals"           . individual))
  "Map level-2 ELOT resource section title to entity kind symbol.")

(defconst elot-id-insert--child-allowed-kinds
  '(class object-property data-property annotation-property individual)
  "Kinds for which `elot-insert-child-resource' is permitted under a
level-3+ heading.  Datatypes remain sibling-only under their
respective resources (OWL has no inherent datatype sub-relation),
but a child insert directly on the level-2 section heading is
always allowed (it seeds the first resource).  Individuals are
included here even though OWL carries no semantic sub-relation
between named individuals: nested headings under the Individuals
section are permitted purely as a visual / editing aid.")

;;; ---------------------------------------------------------------------------
;;; Section + scheme + prefix lookup
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--section-name ()
  "Return the level-2 ELOT resource section title at point, or nil.
Walks up the outline from the current heading to the level-2
ancestor and returns its title verbatim.  Returns nil when point
is above level 2 (e.g. on the ontology root heading) or not in a
heading at all."
  (save-excursion
    (when (or (org-at-heading-p)
              (ignore-errors (org-back-to-heading t) t))
      (let ((lvl (org-current-level)))
        (while (and lvl (> lvl 2))
          (org-up-heading-safe)
          (setq lvl (org-current-level)))
        (when (and lvl (= lvl 2))
          (org-get-heading t t t t))))))

(defun elot-id-insert--scheme-spec ()
  "Return the scheme spec string from the containing ontology, or nil.
Uses Org property inheritance so multi-ontology files work."
  (let ((s (org-entry-get-with-inheritance "ELOT-id-scheme")))
    (and s (not (string-empty-p (string-trim s))) (string-trim s))))

(defun elot-id-insert--prefix ()
  "Return the CURIE prefix from `:ELOT-default-prefix:', or nil."
  (let ((p (org-entry-get-with-inheritance "ELOT-default-prefix")))
    (and p (not (string-empty-p (string-trim p))) (string-trim p))))

;;; ---------------------------------------------------------------------------
;;; Existing-IRI harvest (for collision-aware minting)
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--collect-iris-from-hierarchy ()
  "Walk `elot-headline-hierarchy' depth-first; return all :uri CURIEs.
Returns the list with duplicates removed, preserving first appearance."
  (let (acc)
    (cl-labels ((walk (n)
                  (let ((uri (plist-get n :uri)))
                    (when (and uri (stringp uri))
                      (let ((tok (car (split-string uri "[ \t]+" t))))
                        (when (and tok (not (string-empty-p tok)))
                          (push tok acc)))))
                  (mapc #'walk (plist-get n :children))))
      (let ((h (bound-and-true-p elot-headline-hierarchy)))
        (when h (walk h))))
    (delete-dups (nreverse acc))))

(defun elot-id-insert--existing-iris ()
  "Return CURIEs already declared in the current buffer.
Refreshes `elot-headline-hierarchy' when stale (via
`elot-headline-hierarchy-ensure-fresh') so freshly-inserted
identifiers from a prior call are visible to the next mint without
the unconditional reparse the pre-9.3.F8 implementation performed
on every entry."
  (cond
   ((fboundp 'elot-headline-hierarchy-ensure-fresh)
    (elot-headline-hierarchy-ensure-fresh))
   ((fboundp 'elot-update-headline-hierarchy)
    (ignore-errors (elot-update-headline-hierarchy))))
  (elot-id-insert--collect-iris-from-hierarchy))

;;; ---------------------------------------------------------------------------
;;; Description-list inheritance (blanked copy)
;;; ---------------------------------------------------------------------------

(defconst elot-id-insert--desc-line-re
  (concat "^ -[ \t]+"
          "\\(\\[[ Xx]\\]\\)?[ \t]*"
          "\\(.+?\\)[ \t]+::")
  "Regex matching a top-level ELOT description-list item.
Group 1 is the checkbox cookie (optional); group 2 is the tag
\(may contain colons -- ELOT tags are CURIEs).  The tag is
matched non-greedily up to the ` ::` separator.  Anchored to a
single leading space so nested annotation rows (indented with
3+ spaces) are excluded.")

(defun elot-id-insert--blank-desc-lines ()
  "Return blanked description-list lines for the headline at point.
Each returned line is a string of the form ` - [ ] TAG ::' (or
` - TAG ::' when no checkbox was present).  Only top-level items
are returned; nested annotation rows are skipped per Step 10.6
scope.  Rows whose tag is an OMN axiom keyword (member of
`elot-omn-all-keywords' -- Domain, Range, SubClassOf, Types,
Characteristics, DisjointClasses, ...) are also skipped per Step
9.3.F3: inheriting them blank-valued would produce ill-formed
OMN on tangle.  Annotation rows are inherited as before."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading-end (line-end-position))
           (subtree-end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
           (next-heading
            (save-excursion
              (goto-char heading-end)
              (if (re-search-forward "^\\*+ " subtree-end t)
                  (line-beginning-position)
                subtree-end)))
           lines)
      (goto-char heading-end)
      (forward-line 1)
      (while (< (point) next-heading)
        (when (looking-at elot-id-insert--desc-line-re)
          (let ((cbox (match-string 1))
                (tag  (match-string 2)))
            ;; Step 9.3.F3: skip OMN axiom keyword rows (Domain,
            ;; Range, SubClassOf, Characteristics, DisjointClasses,
            ;; Types, ...).  Inheriting them blank-valued produces
            ;; rows whose tangled OMN is malformed (9.3.F2 silently
            ;; drops them on the tangle path, but skipping the
            ;; inheritance up front means the new heading is born
            ;; clean and the lint warning from 9.3.F4 does not fire
            ;; on freshly-inserted resources).  Plain annotation
            ;; rows (rdfs:comment, iof-av:naturalLanguageDefinition,
            ;; ...) are still inherited blank, since an empty
            ;; annotation row tangles to a benign empty annotation
            ;; and is exactly the "fill this in" prompt the M10.6
            ;; design intended.
            (unless (and (bound-and-true-p elot-omn-all-keywords)
                         (member tag elot-omn-all-keywords))
              (push (concat " - "
                            (if cbox "[ ] " "")
                            tag
                            " ::")
                    lines))))
        (forward-line 1))
      (nreverse lines))))

;;; ---------------------------------------------------------------------------
;;; Heading composition + insertion-point navigation
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--make-heading-block (level curies labels desc-lines)
  "Return one string containing N stacked heading entries.
LEVEL is the asterisk count; CURIES is the list of CURIEs; LABELS is
the parallel list of rdfs:labels (same length as CURIES); DESC-LINES
is the (already blanked) description-list copy to attach under each
heading.  Each heading line has the shape

  STARS SPACE LABEL SPACE (CURIE) NEWLINE"
  (cl-loop for curie in curies
           for label in labels
           concat (concat (make-string level ?*)
                          " " label " (" curie ")\n"
                          (when desc-lines
                            (concat (mapconcat #'identity desc-lines "\n")
                                    "\n")))))

(defun elot-id-insert--goto-first-child-pos ()
  "Move point to the position where a new first child heading should go.
Assumes point is at (or will be moved back to) the current heading."
  (org-back-to-heading t)
  (let* ((cur-level (org-current-level))
         (target-level (1+ cur-level))
         (subtree-end (save-excursion (org-end-of-subtree t t) (point)))
         found)
    (forward-line 1)
    (while (and (< (point) subtree-end) (not found))
      (cond
       ((and (looking-at "^\\(\\*+\\) ")
             (= (length (match-string 1)) target-level))
        (setq found t))
       (t (forward-line 1))))
    (unless found
      (goto-char subtree-end))))

;;; ---------------------------------------------------------------------------
;;; Core insertion driver
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--validate-kind (kind child-p cur-level section)
  "Signal `user-error' when KIND / CHILD-P / CUR-LEVEL are incompatible.
A child insert on a level-2 section heading is always permitted
\(it seeds the first resource of an empty section).  Below that,
Datatypes refuse the child variant; nested headings under
Individuals are allowed as a visual / editing aid (OWL carries no
semantic sub-relation between named individuals)."
  (when (and child-p
             (>= cur-level 3)
             (not (memq kind elot-id-insert--child-allowed-kinds)))
    (user-error
     "elot-insert: cannot insert child resource under section %S (kind %s); use sibling instead"
     section kind)))

(defun elot-id-insert--read-labels (n child-p)
  "Prompt the user for N rdfs:labels in the minibuffer.
Returns the list of label strings (in order).  An empty response
re-prompts.  CHILD-P only affects the prompt wording."
  (let ((kind-word (if child-p "child" "sibling"))
        labels)
    (dotimes (i n)
      (let (s)
        (while (or (null s) (string-empty-p (string-trim s)))
          (setq s (read-string
                   (format "Label for new %s %d/%d: " kind-word (1+ i) n))))
        (push (string-trim s) labels)))
    (nreverse labels)))

(defun elot-id-insert--do-insert (child-p n &optional labels)
  "Insert resource headings (CHILD-P controls sibling vs child).
When LABELS is a non-empty list of strings, its length determines
the number of headings inserted and the numeric N argument is
ignored.  Otherwise N (default 1) headings are inserted and the
user is prompted in the minibuffer for one rdfs:label per heading.
Labels are forwarded to `elot-id-mint-batch' (so `slug' and `acme
slug:t' derive their local names from them) and rendered verbatim
in the heading title before the (prefix:localname) suffix.
Returns the list of minted CURIEs."
  (when (and labels (listp labels))
    (setq n (length labels)))
  (unless (and (integerp n) (> n 0)) (setq n 1))
  (unless (derived-mode-p 'org-mode)
    (user-error "elot-insert: not in an Org buffer"))
  (save-excursion
    (unless (or (org-at-heading-p)
                (ignore-errors (org-back-to-heading t) t))
      (user-error "elot-insert: point is not inside an Org heading")))
  (let* ((section (elot-id-insert--section-name))
         (kind    (and section
                       (cdr (assoc section elot-id-insert--section-kind-alist))))
         (cur-level (save-excursion
                      (org-back-to-heading t)
                      (org-current-level))))
    (cond
     ((null section)
      (user-error
       "elot-insert: point is not inside an ELOT resource section (one of: %s)"
       (mapconcat #'car elot-id-insert--section-kind-alist ", ")))
     ((null kind)
      (user-error
       "elot-insert: section %S is not a recognised ELOT resource section"
       section)))
    (elot-id-insert--validate-kind kind child-p cur-level section)
    (let* ((spec (or (elot-id-insert--scheme-spec)
                     (progn
                       (message
                        "elot-insert: no :ELOT-id-scheme: declared; prompting for one...")
                       (call-interactively #'elot-id-set-scheme)
                       (or (elot-id-insert--scheme-spec)
                           (user-error
                            "elot-insert: no :ELOT-id-scheme: declared on the ontology heading")))))
           (parsed (elot-id-parse-spec spec))
           (scheme-name (car parsed))
           (scheme-params (cdr parsed))
           (prefix (elot-id-insert--prefix))
           (existing (elot-id-insert--existing-iris))
           (desc-lines (elot-id-insert--blank-desc-lines))
           (labels (or labels (elot-id-insert--read-labels n child-p)))
           (_ (unless (and (listp labels)
                           (= (length labels) n)
                           (cl-every (lambda (s)
                                       (and (stringp s)
                                            (not (string-empty-p
                                                  (string-trim s)))))
                                     labels))
                (user-error
                 "elot-insert: LABELS must be a list of %d non-empty strings" n)))
           (context (list :kind kind
                          :scheme-params scheme-params
                          :prefix prefix
                          :existing-iris existing))
           (target-level (if child-p (1+ cur-level) cur-level))
           (curies (elot-id-mint-batch scheme-name n context :labels labels))
           ;; Acme schemes attach a slug-loss warning via a text
           ;; property; strip it so the CURIE text stays clean.
           (curies-clean (mapcar #'substring-no-properties curies))
           insert-pos)
      (save-excursion
        (org-back-to-heading t)
        (if child-p
            (elot-id-insert--goto-first-child-pos)
          (org-end-of-subtree t t))
        (unless (bolp) (insert "\n"))
        (setq insert-pos (point))
        (insert (elot-id-insert--make-heading-block
                 target-level curies-clean labels desc-lines)))
      ;; Position point at end of the first inserted heading line.
      (goto-char insert-pos)
      (end-of-line)
      ;; M9.3.F8: mark the buffer-local hierarchy cache stale so the
      ;; next reader (e.g. `elot-id-insert--existing-iris' on the
      ;; following tree level, or `elot-id-rename--declared-curies'
      ;; in a chained rename call) rebuilds exactly once.  Without
      ;; this the cache would still show the pre-insert view of the
      ;; buffer until something explicitly rescans.
      (when (fboundp 'elot-headline-hierarchy-mark-stale)
        (elot-headline-hierarchy-mark-stale))
      curies-clean)))

;;; ---------------------------------------------------------------------------
;;; Public interactive commands
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Per-ontology scheme picker (Step 10.7)
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--ontology-heading-marker ()
  "Return a marker at the ontology heading containing point, or nil.
Walks up the outline from point looking for the nearest heading
whose `:ELOT-context-type:' property is `ontology'.  Returns nil
when point is not inside any heading or no ontology ancestor
exists."
  (save-excursion
    (when (or (org-at-heading-p)
              (ignore-errors (org-back-to-heading t) t))
      (catch 'found
        (while t
          (when (equal (org-entry-get nil "ELOT-context-type") "ontology")
            (throw 'found (point-marker)))
          (unless (org-up-heading-safe)
            (throw 'found nil)))))))

(defun elot-id-insert--find-ontology-headings ()
  "Return an alist of (LOCALNAME . MARKER) for every ontology heading.
Primary source is `elot-headline-hierarchy' -- every top-level
child whose `:elot-context-type' is `\"ontology\"' contributes one
entry, using `:elot-context-localname' (falling back to `:title')
as LOCALNAME and `:marker' as MARKER.  Refreshes the hierarchy
once when it is empty.  Falls back to a regex buffer scan only
when no hierarchy is available (e.g. `elot-tangle' not loaded)."
  (cond
   ((fboundp 'elot-headline-hierarchy-ensure-fresh)
    (elot-headline-hierarchy-ensure-fresh))
   ((and (not (bound-and-true-p elot-headline-hierarchy))
         (fboundp 'elot-update-headline-hierarchy))
    (ignore-errors (elot-update-headline-hierarchy))))
  (let ((h (bound-and-true-p elot-headline-hierarchy))
        acc)
    (if h
        (dolist (n (plist-get h :children))
          (when (equal (plist-get n :elot-context-type) "ontology")
            (let ((name (or (plist-get n :elot-context-localname)
                            (plist-get n :title)
                            "(unnamed)"))
                  (marker (plist-get n :marker)))
              (when marker
                (push (cons name marker) acc)))))
      ;; Fallback: regex scan (only when no hierarchy is available).
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
         (when (equal (org-entry-get nil "ELOT-context-type") "ontology")
           (let ((name (or (org-entry-get nil "ELOT-context-localname")
                           (org-get-heading t t t t)
                           "(unnamed)")))
             (push (cons name (point-marker)) acc))))))
    (nreverse acc)))

(defun elot-id-insert--pick-ontology-marker ()
  "Return a marker at the ontology heading to edit.
When point is inside an ontology subtree, use that one.
Otherwise prompt over all ontology headings in the buffer.
Signals `user-error' when the buffer contains no ontology heading."
  (or (elot-id-insert--ontology-heading-marker)
      (let ((alist (elot-id-insert--find-ontology-headings)))
        (cond
         ((null alist)
          (user-error
           "elot-id-set-scheme: no `:ELOT-context-type: ontology' heading in buffer"))
         ((= (length alist) 1) (cdar alist))
         (t (let ((name (completing-read
                         "Set scheme for ontology: "
                         (mapcar #'car alist) nil t)))
              (cdr (assoc name alist))))))))

(defun elot-id-insert--scheme-names ()
  "Return registered scheme names as a list of strings."
  (mapcar (lambda (pair) (symbol-name (car pair)))
          elot-id-schemes))

(defun elot-id-insert--scheme-annotation (name)
  "Return the one-line description for scheme NAME (string), or nil."
  (let* ((sym (intern name))
         (scheme (cdr (assq sym elot-id-schemes))))
    (and scheme
         (let ((d (elot-id-scheme-description scheme)))
           (and d (concat "  " d))))))

(defun elot-id-insert--read-scheme-params (scheme-sym)
  "Run the scheme-specific second prompt for SCHEME-SYM.
Returns the token-list tail of the spec string (i.e. everything
after the scheme name), or nil for a bare scheme.  Built-in
schemes are dispatched by name; custom schemes may set
`:read-params-fn' in their `metadata' plist to a zero-argument
function that returns the same shape."
  (let* ((scheme (cdr (assq scheme-sym elot-id-schemes)))
         (custom (and scheme
                      (plist-get (elot-id-scheme-metadata scheme)
                                 :read-params-fn))))
    (cond
     ((functionp custom) (funcall custom))
     ((eq scheme-sym 'uuid) nil)
     ((eq scheme-sym 'slug) nil)
     ((eq scheme-sym 'counter)
      (let ((tmpl (read-string "Counter template (e.g. GO_0000000): "
                               nil nil "0000000")))
        (when (string-empty-p (string-trim tmpl))
          (user-error "elot-id-set-scheme: template must be non-empty"))
        (list tmpl)))
     ((eq scheme-sym 'acme)
      (if (y-or-n-p "Include slug derived from label? ")
          (list "slug:t")
        nil))
     (t
      (let ((s (read-string
                (format "Parameters for scheme `%s' (blank for none): "
                        scheme-sym))))
        (and (not (string-empty-p (string-trim s)))
             (split-string s "[ \t]+" t)))))))

(defun elot-id-insert--format-spec (scheme-sym tokens)
  "Render SCHEME-SYM + TOKENS as a single spec string."
  (if tokens
      (concat (symbol-name scheme-sym) " "
              (mapconcat #'identity tokens " "))
    (symbol-name scheme-sym)))

;;;###autoload
(defun elot-id-set-scheme ()
  "Set the `:ELOT-id-scheme:' property of the containing ontology heading.
Prompts for the scheme name (from the registered schemes) and any
scheme-specific parameters (counter template, acme slug toggle,
...), then writes the resulting spec string to the ontology
heading via `org-entry-put'.  When the heading already carries
an `:ELOT-id-scheme:' value, asks for confirmation before
overwriting it.

When point is inside an ontology subtree, that ontology is the
target.  Otherwise the user is prompted to pick one from among
the buffer's ontology headings (multi-ontology files supported).

After the property is written, `elot-headline-hierarchy' is
refreshed so subsequent inserts and gptel mint calls see the new
value immediately."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "elot-id-set-scheme: not in an Org buffer"))
  (let* ((marker (elot-id-insert--pick-ontology-marker))
         (existing
          (save-excursion
            (goto-char marker)
            (org-entry-get nil "ELOT-id-scheme")))
         (existing (and existing (not (string-empty-p (string-trim existing)))
                        (string-trim existing)))
         (default-name (and existing (car (split-string existing "[ \t]+" t))))
         (names (elot-id-insert--scheme-names))
         (completion-extra-properties
          (list :annotation-function #'elot-id-insert--scheme-annotation))
         (chosen (completing-read
                  (if existing
                      (format "Scheme (current: %s): " existing)
                    "Scheme: ")
                  names nil t nil nil default-name))
         (scheme-sym (intern chosen))
         (tokens (elot-id-insert--read-scheme-params scheme-sym))
         (spec (elot-id-insert--format-spec scheme-sym tokens)))
    (when (and existing
               (not (string= existing spec))
               (not (y-or-n-p
                     (format "Overwrite `:ELOT-id-scheme: %s' with `%s'? "
                             existing spec))))
      (user-error "elot-id-set-scheme: cancelled"))
    (save-excursion
      (goto-char marker)
      (org-entry-put nil "ELOT-id-scheme" spec))
    ;; M9.3.F8: O(1) staleness mark.  Setting the property changes
    ;; the ontology heading's :elot-id-scheme cell in the parsed
    ;; hierarchy, but the next reader can rebuild on demand.
    (cond
     ((fboundp 'elot-headline-hierarchy-mark-stale)
      (elot-headline-hierarchy-mark-stale))
     ((fboundp 'elot-update-headline-hierarchy)
      (ignore-errors (elot-update-headline-hierarchy))))
    (let ((name (save-excursion
                  (goto-char marker)
                  (or (org-entry-get nil "ELOT-context-localname")
                      (org-get-heading t t t t)))))
      (message "Scheme for ontology %s set to `%s'" name spec))
    spec))

;;;###autoload
(defun elot-id-show-scheme ()
  "Echo the `:ELOT-id-scheme:' of the ontology containing point.
Reports the resolved spec string (as Org property inheritance
sees it) or `(unset)' when no scheme is declared.

When the declared scheme name does not match any registered
scheme (see `elot-id-schemes'), or the spec string cannot be
parsed, emit a `display-warning' and flag the echoed result
as `[INVALID: ...]'."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "elot-id-show-scheme: not in an Org buffer"))
  (let* ((marker (elot-id-insert--pick-ontology-marker))
         (spec (save-excursion
                 (goto-char marker)
                 (org-entry-get-with-inheritance "ELOT-id-scheme")))
         (spec (and spec (string-trim spec)))
         (name (save-excursion
                 (goto-char marker)
                 (or (org-entry-get nil "ELOT-context-localname")
                     (org-get-heading t t t t)))))
    (cond
     ((or (null spec) (string-empty-p spec))
      (message "Ontology %s: scheme = (unset)" name))
     (t
      (let* ((parsed (condition-case err
                         (elot-id-parse-spec spec)
                       (error (cons nil err))))
             (scheme-name (car parsed))
             (known (and scheme-name
                         (assq scheme-name elot-id-schemes))))
        (cond
         ((null scheme-name)
          (display-warning
           'elot-id
           (format "Ontology %s: `:ELOT-id-scheme:' value %S is unparseable"
                   name spec)
           :warning)
          (message "Ontology %s: scheme = %s  [INVALID: unparseable]"
                   name spec))
         ((not known)
          (display-warning
           'elot-id
           (format "Ontology %s: scheme `%s' is not registered.  Known schemes: %s"
                   name scheme-name
                   (mapconcat (lambda (cell) (symbol-name (car cell)))
                              elot-id-schemes ", "))
           :warning)
          (message "Ontology %s: scheme = %s  [INVALID: unknown scheme `%s']"
                   name spec scheme-name))
         (t
          (message "Ontology %s: scheme = %s" name spec))))))
    spec))

;;; ---------------------------------------------------------------------------
;;; Public interactive commands
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun elot-insert-sibling-resource (&optional n labels)
  "Insert resource headings as siblings of the heading at point.
When LABELS is a non-empty list of strings, its length determines
the number of headings inserted and N is ignored -- useful when
calling from code, e.g.

    (elot-insert-sibling-resource nil \\='(\"Hedgehog\" \"Otter\" \"Badger\"))

Otherwise N (default 1, or the numeric prefix argument when
interactive) headings are inserted and the user is prompted in
the minibuffer for one rdfs:label per heading.  The label is fed
into the configured identifier scheme and rendered verbatim in
the heading title.  The new headings appear after the current
heading's subtree, at the same outline level.  See
`elot-id-insert' Commentary for the full contract (scheme
resolution, description-list inheritance, kind restrictions)."
  (interactive "p")
  (elot-id-insert--do-insert nil (or n 1) labels))

;;; ---------------------------------------------------------------------------
;;; Tree builder
;;; ---------------------------------------------------------------------------

(defun elot-id-insert--normalize-tree-node (node)
  "Return NODE as a cons (LABEL . CHILDREN).
A bare string is treated as a leaf node (no children).  A list
must have a string head; its tail is the list of child nodes."
  (cond
   ((stringp node) (cons (string-trim node) nil))
   ((and (consp node) (stringp (car node)))
    (cons (string-trim (car node)) (cdr node)))
   (t (user-error
       "elot-insert-labels-tree: malformed node %S (expected STRING or (LABEL CHILD...))"
       node))))

(defun elot-id-insert--goto-heading-for-curie (curie)
  "Move point to the heading line whose title ends in `(CURIE)'.
Searches the whole buffer.  Signals an error if not found."
  (goto-char (point-min))
  (unless (re-search-forward
           (elot-id-heading-curie-regexp curie) nil t)
    (error "elot-insert-labels-tree: cannot locate inserted heading for %s"
           curie))
  (beginning-of-line))

;;;###autoload
(defun elot-insert-labels-tree (tree &optional as)
  "Insert a tree of new resource headings described by TREE.
TREE is a list of nodes.  Each node is either a bare LABEL string
\(leaf) or a list (LABEL CHILD-NODE ...) whose tail is recursively
the same shape:

    (\"Animal\"
     (\"Dog\" \"Beagle\" \"Poodle\")
     (\"Cat\" \"Persian\" (\"Siamese\" \"Modern\" \"Traditional\"))
     \"Snake\")

inserts an `Animal' branch with three direct children; `Dog' gets
two leaf children, `Cat' gets two children one of which has two
of its own, and `Snake' is a leaf.

AS controls how the top-level nodes are placed relative to the
heading at point:
  `sibling' (default) -- same outline level, after subtree;
  `child'             -- one level deeper, as first children.
Within the recursion every nested level is always inserted as
children of its parent.

Each level reuses `elot-insert-sibling-resource' /
`elot-insert-child-resource' so all the usual ELOT contracts hold:
scheme + prefix resolution from the ontology heading, kind
inferred from the level-2 section, description-list inheritance
from the heading at point, and refusal of child inserts under
Datatypes (a deeply-nested Datatype with children will surface
the error at the offending recursive call).  Nested headings
under Individuals are permitted as a visual / editing aid.

When called from code, returns the list of CURIEs minted at the
top level (the buffer carries the full tree at point of return)."
  (interactive
   (let* ((default
            "(\"Animal\" (\"Dog\" \"Beagle\" \"Poodle\") \"Cat\" \"Snake\")")
          (form (read-from-minibuffer
                 "Tree (list of nodes): " default nil t)))
     (list (if (listp form) form (list form)) 'sibling)))
  (unless (and (listp tree) tree)
    (user-error "elot-insert-labels-tree: TREE must be a non-empty list"))
  (let* ((nodes (mapcar #'elot-id-insert--normalize-tree-node tree))
         (labels (mapcar #'car nodes))
         (curies (elot-id-insert--do-insert
                  (eq as 'child) (length labels) labels)))
    (save-excursion
      (cl-loop for node in nodes
               for curie in curies
               for children = (cdr node)
               when children
               do (elot-id-insert--goto-heading-for-curie curie)
                  (elot-insert-labels-tree children 'child)))
    curies))

;;;###autoload
(defun elot-insert-child-resource (&optional n labels)
  "Insert resource headings as first children of the heading at point.
When LABELS is a non-empty list of strings, its length determines
the number of headings inserted and N is ignored -- useful when
calling from code, e.g.

    (elot-insert-child-resource nil \\='(\"Hedgehog\" \"Otter\"))

Otherwise N (default 1, or the numeric prefix argument when
interactive) headings are inserted and the user is prompted in
the minibuffer for one rdfs:label per heading.  Refuses with
`user-error' when invoked on a level-3+ heading whose section is
Datatypes (no inherent subtype relation among datatypes).  Nested
headings under Individuals are allowed as a visual / editing aid
(OWL carries no semantic sub-relation between named individuals).
Invoked on a level-2 section heading the child variant
always succeeds and seeds the first resource."
  (interactive "p")
  (elot-id-insert--do-insert t (or n 1) labels))

(provide 'elot-id-insert)
;;; elot-id-insert.el ends here
