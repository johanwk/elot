;;; elot-id-move.el --- Move a resource heading within an ontology  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Kluwer

;; Author: Johan W. Kluwer
;; URL: https://github.com/johanwk/elot
;; Keywords: languages tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; ELOT-GPTEL-PLAN.org Milestone 12 Step 12.1 -- relocate a resource
;; heading (with its whole subtree) to a new parent within the same
;; ontology, within the current file.  The CURIE is unchanged.
;;
;; Public entry points:
;;
;;   M-x elot-move-resource              ; prompts for SOURCE / TARGET / AS
;;   (elot-move-resource SOURCE TARGET &optional AS)
;;
;; SOURCE is a CURIE string (e.g. `ex:dog') declared on a resource
;; heading inside a `:resourcedefs: yes' subtree.  TARGET is either
;; another CURIE string (the new parent or previous-sibling), or the
;; literal string "top" / the symbol `top' to mean "move to the
;; section root of SOURCE's kind".  AS is `child' (default; the moved
;; subtree becomes a child of TARGET) or `sibling' (the moved subtree
;; is inserted right after TARGET's whole subtree, at TARGET's level).
;;
;; Scope guards:
;;
;;   - SOURCE must be a resource heading inside a `:resourcedefs: yes'
;;     ancestor whose title is one of the six recognised ELOT section
;;     names (`Classes' / `Object properties' / `Data properties' /
;;     `Annotation properties' / `Datatypes' / `Individuals').
;;   - TARGET's section kind must equal SOURCE's section kind --
;;     classes only land under classes, properties only under
;;     properties of the same kind, etc.  Mismatch -> `user-error'
;;     naming both kinds.
;;   - Datatypes and Individuals are sibling-only at level 3+ -- the
;;     `child' variant under those sections is refused /unless/ TARGET
;;     is the section root itself (the same "seed an empty section"
;;     exemption already in `elot-id-insert').
;;   - SOURCE = TARGET, or TARGET already the current parent: refused
;;     as a no-op.
;;   - SOURCE ambiguous (declared in more than one ontology subtree of
;;     the buffer): refused with a list of candidate ontology
;;     localnames.  M12.2 will lift this; M12.1 is single-ontology.
;;
;; The move is performed inside `atomic-change-group' (cut-subtree +
;; paste-subtree with explicit LEVEL) so a mid-flight failure rolls
;; the buffer back to its pre-move state.  After a successful move
;; `elot-update-headline-hierarchy' is re-run so the buffer-local
;; cache reflects the new shape.
;;
;; Out of scope for M12.1: cross-ontology moves in the same buffer
;; (M12.2), cross-file moves (M12.3), CURIE rewrite for cross-prefix
;; moves (delegate to `elot-rename-resource', Step 12.4).  The
;; companion LLM-facing tool `elot_move_resource' wraps this command
;; in a later milestone -- see the plan's Step 12.1 "LLM-facing tool"
;; section.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'elot-id-insert)  ; for the section-kind alist
(require 'elot-tangle nil 'noerror)

(defvar elot-headline-hierarchy)
(declare-function elot-update-headline-hierarchy "elot-tangle" ())
;; M9.3.F8: O(1) staleness markers.
(declare-function elot-headline-hierarchy-mark-stale "elot-tangle" ())
(declare-function elot-headline-hierarchy-ensure-fresh "elot-tangle" ())

;; Label-lookup completion (loaded lazily; absent in batch tests).
(defvar elot-attriblist-ht)
(defvar elot-label-lookup-tmp-attriblist-ht)
(declare-function elot-label-lookup-annotations "elot-label-display" (label))
(declare-function elot-attriblist-label-value
                  "elot-label-display" (idstring prop))

;;; ---------------------------------------------------------------------------
;;; CURIE shape
;;; ---------------------------------------------------------------------------

(defconst elot-id-move--curie-re
  "\\`\\([A-Za-z_][A-Za-z0-9_-]*\\):\\([A-Za-z_][A-Za-z0-9_.-]*\\)\\'"
  "Strict CURIE shape, used for source/target validation.")

(defun elot-id-move--curie-p (s)
  "Return non-nil when S is a CURIE-shaped string."
  (and (stringp s) (string-match-p elot-id-move--curie-re s)))

(defun elot-id-move--top-target-p (target)
  "Return non-nil when TARGET names the section root (`top')."
  (or (eq target 'top)
      (and (stringp target)
           (member (downcase (string-trim target)) '("top")))))

;;; ---------------------------------------------------------------------------
;;; Section + kind lookup
;;; ---------------------------------------------------------------------------

(defun elot-id-move--section-kind-at-point ()
  "Return (KIND . SECTION-MARKER) for the resource heading at point.
KIND is one of the symbols in
`elot-id-insert--section-kind-alist' (`class',
`object-property', `data-property', `annotation-property',
`datatype', `individual'); SECTION-MARKER points at the level-2
section heading (the `:resourcedefs: yes' ancestor).  Returns nil
when point is not inside a recognised resource section."
  (save-excursion
    (when (or (org-at-heading-p)
              (ignore-errors (org-back-to-heading t) t))
      (catch 'found
        (let ((lvl (org-current-level)))
          (while (and lvl (>= lvl 2))
            (when (and (= lvl 2)
                       (equal (org-entry-get nil "resourcedefs") "yes"))
              (let* ((title (org-get-heading t t t t))
                     (kind  (cdr (assoc title
                                        elot-id-insert--section-kind-alist))))
                (throw 'found (and kind (cons kind (point-marker))))))
            (unless (org-up-heading-safe)
              (throw 'found nil))
            (setq lvl (org-current-level))))
        nil))))

(defun elot-id-move--ontology-localname-at-point ()
  "Return the `:ELOT-context-localname:' of the ontology containing point.
Nil when no ontology ancestor exists."
  (save-excursion
    (when (or (org-at-heading-p)
              (ignore-errors (org-back-to-heading t) t))
      (catch 'found
        (while t
          (when (equal (org-entry-get nil "ELOT-context-type") "ontology")
            (throw 'found
                   (or (org-entry-get nil "ELOT-context-localname")
                       (org-get-heading t t t t))))
          (unless (org-up-heading-safe)
            (throw 'found nil)))))))

;;; ---------------------------------------------------------------------------
;;; Heading lookup by CURIE
;;; ---------------------------------------------------------------------------

(defun elot-id-move--find-heading-markers (curie)
  "Return a list of markers for every heading declaring CURIE.
Matches the ELOT convention `* Label (prefix:local)' at any
heading level.  Each marker points at the beginning of the
heading line."
  (let (markers)
    (save-excursion
      (goto-char (point-min))
      (let ((re (elot-id-heading-curie-regexp curie)))
        (while (re-search-forward re nil t)
          (push (copy-marker (line-beginning-position)) markers))))
    (nreverse markers)))

(defun elot-id-move--resolve-source-marker (source)
  "Return the marker for the heading declaring SOURCE.
Signals `user-error' when SOURCE is not declared, or when more
than one ontology in the buffer declares it (M12.1 is
single-ontology -- the user must split the call or wait for
M12.2)."
  (let ((markers (elot-id-move--find-heading-markers source)))
    (cond
     ((null markers)
      (user-error
       "elot-move-resource: SOURCE %s is not a declared resource heading in this buffer"
       source))
     ((= (length markers) 1)
      (car markers))
     (t
      (let ((onts (delete-dups
                   (mapcar (lambda (m)
                             (save-excursion
                               (goto-char m)
                               (or (elot-id-move--ontology-localname-at-point)
                                   "(unknown)")))
                           markers))))
        (user-error
         "elot-move-resource: SOURCE %s is declared in more than one ontology (%s); ambiguous"
         source (mapconcat #'identity onts ", ")))))))

(defun elot-id-move--resolve-target (source-marker source-kind target)
  "Return (MODE . MARKER) for TARGET.
MODE is one of: `heading' (TARGET is a resource heading) or
`section-root' (TARGET = \"top\"); MARKER points at the
resolved heading line.  Signals `user-error' on kind mismatch
or when TARGET is not declared."
  (cond
   ((elot-id-move--top-target-p target)
    (let ((sect (cdr (save-excursion
                       (goto-char source-marker)
                       (elot-id-move--section-kind-at-point)))))
      (unless sect
        (user-error
         "elot-move-resource: cannot resolve section root for SOURCE %s"
         source-marker))
      (cons 'section-root sect)))
   ((elot-id-move--curie-p target)
    (let ((markers (elot-id-move--find-heading-markers target)))
      (cond
       ((null markers)
        (user-error
         "elot-move-resource: TARGET %s is not a declared resource heading in this buffer"
         target))
       ((> (length markers) 1)
        (user-error
         "elot-move-resource: TARGET %s is declared more than once; ambiguous"
         target))
       (t
        (let* ((tm (car markers))
               (tkind (car (save-excursion
                             (goto-char tm)
                             (elot-id-move--section-kind-at-point)))))
          (unless tkind
            (user-error
             "elot-move-resource: TARGET %s is not inside a recognised resource section"
             target))
          (unless (eq tkind source-kind)
            (user-error
             "elot-move-resource: kind mismatch -- SOURCE is a %s, TARGET %s is a %s"
             source-kind target tkind))
          (cons 'heading tm))))))
   (t
    (user-error
     "elot-move-resource: TARGET must be a CURIE or the literal \"top\": %S"
     target))))

;;; ---------------------------------------------------------------------------
;;; Geometry helpers
;;; ---------------------------------------------------------------------------

(defun elot-id-move--current-parent-marker ()
  "Return a marker at the parent heading of the heading at point.
Returns nil when point is at a top-level heading."
  (save-excursion
    (org-back-to-heading t)
    (when (org-up-heading-safe)
      (copy-marker (line-beginning-position)))))

(defun elot-id-move--heading-level (marker)
  "Return the outline level of the heading at MARKER."
  (save-excursion
    (goto-char marker)
    (org-back-to-heading t)
    (org-current-level)))

(defun elot-id-move--source-inside-target-p (source-marker target-marker)
  "Return non-nil when TARGET-MARKER lies inside SOURCE-MARKER's subtree.
Moving SOURCE into its own descendant would be a malformed move."
  (save-excursion
    (goto-char source-marker)
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (and (>= (marker-position target-marker) beg)
           (<  (marker-position target-marker) end)))))

;;; ---------------------------------------------------------------------------
;;; Interactive completion (label-lookup-backed)
;;; ---------------------------------------------------------------------------

(defun elot-id-move--label-lookup-available-p ()
  "Return non-nil when buffer-local label-lookup data is populated."
  (and (bound-and-true-p elot-attriblist-ht)
       (hash-table-p elot-attriblist-ht)
       (> (hash-table-count elot-attriblist-ht) 0)))

(defun elot-id-move--read-curie (prompt &optional include-top initial)
  "Prompt for a resource via label completion and return its CURIE.
PROMPT is the completing-read prompt.  When INCLUDE-TOP is
non-nil, a synthetic \"top\" candidate is offered and returned
verbatim when selected (used for the TARGET prompt to express
\"move under the section root\").  INITIAL pre-seeds the
minibuffer when non-nil.

Falls back to a plain `read-string' (with INITIAL as default) when
`elot-attriblist-ht' is not populated -- e.g. before
`elot-label-display-setup' has run in the buffer."
  (require 'elot-label-display nil 'noerror)
  (if (not (elot-id-move--label-lookup-available-p))
      ;; Fallback: raw CURIE entry, matching the pre-convenience UX.
      (read-string
       (format "%s%s"
               prompt
               (if initial (format " (default %s)" initial) ""))
       nil nil initial)
    (let* ((coll elot-attriblist-ht)
           (labels (let (ls)
                     (maphash (lambda (k _v) (push k ls)) coll)
                     (sort ls #'string-lessp)))
           (candidates (if include-top (cons "top" labels) labels))
           (completion-extra-properties
            (append completion-extra-properties
                    (list :annotation-function
                          #'elot-label-lookup-annotations))))
      (setq elot-label-lookup-tmp-attriblist-ht coll)
      (let ((selected (completing-read prompt candidates nil nil nil nil
                                       initial)))
        (cond
         ((null selected) nil)
         ((and include-top (equal selected "top")) "top")
         ;; If the user typed a raw CURIE that wasn't a label, accept it.
         ((elot-id-move--curie-p selected) selected)
         (t (or (elot-attriblist-label-value selected "puri")
                selected)))))))

;;; ---------------------------------------------------------------------------
;;; Public entry point
;;; ---------------------------------------------------------------------------

;;;###autoload
(defun elot-move-resource (source target &optional as)
  "Move resource SOURCE (CURIE) to TARGET within the current ontology.

SOURCE is the CURIE of a declared resource heading in the
current buffer (e.g. `\"ex:dog\"').  TARGET is one of:

  - a CURIE string (e.g. `\"ex:mammal\"') -- the new parent (when
    AS is `child', the default) or the previous sibling (when
    AS is `sibling');
  - the literal string `\"top\"' (or the symbol `top') -- place
    the moved subtree at the top of SOURCE's section, directly
    under the level-2 section heading.

AS is `child' (default) or `sibling'.  Under `sibling', the
moved subtree is inserted right after TARGET's whole subtree,
at TARGET's outline level.

The CURIE is never rewritten -- this is a pure subtree
relocation.  For cross-prefix renames, use
`elot-rename-resource' (Step 12.4) first.

Refuses with `user-error' when:

  - SOURCE is not a declared resource heading;
  - SOURCE is declared in more than one ontology in the same
    buffer (M12.1 is single-ontology; lift via M12.2 or split
    the call);
  - TARGET is not a declared resource heading (and not `top');
  - SOURCE's section kind disagrees with TARGET's section kind;
  - SOURCE = TARGET, or TARGET already the current parent of
    SOURCE (no-op);
  - the move would place SOURCE inside its own subtree;
  - under Datatypes / Individuals, AS=`child' is requested with
    a level-3+ TARGET (no inherent sub-relationship -- TARGET
    must be the section root in those sections).

Returns a plist:
  (:source S :target T :as AS
   :from-parent OLDPARENT :to-parent NEWPARENT)
where OLDPARENT / NEWPARENT are the CURIE of the parent
heading (or the section title prefixed by `section: ' for the
section root)."
  (interactive
   (let* ((curie-at-point
           (or
            ;; Preferred: the CURIE on the enclosing resource heading,
            ;; so the pre-selection works from anywhere inside the
            ;; subtree (body text, description-list rows, sub-headings).
            (save-excursion
              (when (ignore-errors (org-back-to-heading t) t)
                (catch 'curie
                  (while (org-current-level)
                    (let ((title (org-get-heading t t t t)))
                      (when (and title
                                 (string-match
                                  "(\\([A-Za-z_][A-Za-z0-9_-]*:[A-Za-z_][A-Za-z0-9_.-]*\\))"
                                  title))
                        (let ((c (match-string-no-properties 1 title)))
                          (when (elot-id-move--section-kind-at-point)
                            (throw 'curie c)))))
                    (unless (org-up-heading-safe)
                      (throw 'curie nil)))
                  nil)))
            ;; Fallback: literal CURIE-shaped text under point.
            (save-excursion
              (skip-chars-backward "A-Za-z0-9_.:-")
              (and (looking-at
                    "\\([A-Za-z_][A-Za-z0-9_-]*\\):[A-Za-z_][A-Za-z0-9_.-]*")
                   (match-string-no-properties 0)))))
          (src (elot-id-move--read-curie
                (format "Source (label or CURIE)%s: "
                        (if curie-at-point
                            (format " [default %s]" curie-at-point) ""))
                nil curie-at-point))
          (tgt (elot-id-move--read-curie
                "Target (label, CURIE, or \"top\" for section root): "
                t nil))
          (as-str (completing-read "As: " '("child" "sibling")
                                   nil t nil nil "child")))
     (list src tgt (intern as-str))))
  (unless (derived-mode-p 'org-mode)
    (user-error "elot-move-resource: not in an Org buffer"))
  (setq as (or as 'child))
  (unless (memq as '(child sibling))
    (user-error "elot-move-resource: AS must be `child' or `sibling', got %S" as))
  (unless (elot-id-move--curie-p source)
    (user-error "elot-move-resource: SOURCE is not a valid CURIE: %S" source))
  (unless (or (elot-id-move--top-target-p target)
              (elot-id-move--curie-p target))
    (user-error
     "elot-move-resource: TARGET must be a CURIE or \"top\": %S" target))
  ;; Resolve source heading + its section kind.
  (let* ((source-marker (elot-id-move--resolve-source-marker source))
         (source-info
          (save-excursion
            (goto-char source-marker)
            (elot-id-move--section-kind-at-point)))
         (_ (unless source-info
              (user-error
               "elot-move-resource: SOURCE %s is not inside a `:resourcedefs: yes' section"
               source)))
         (source-kind (car source-info))
         (target-resolution
          (elot-id-move--resolve-target source-marker source-kind target))
         (target-mode (car target-resolution))
         (target-marker (cdr target-resolution))
         ;; Datatypes / Individuals: child under a level-3+ target is
         ;; refused (no inherent sub-relationship).
         (_ (when (and (eq as 'child)
                       (eq target-mode 'heading)
                       (memq source-kind '(datatype individual)))
              (user-error
               "elot-move-resource: cannot insert a %s as a CHILD of another %s (no inherent sub-relationship); use AS=sibling, or TARGET=\"top\""
               source-kind source-kind)))
         ;; No-op detection.
         (old-parent (save-excursion
                       (goto-char source-marker)
                       (elot-id-move--current-parent-marker)))
         (target-level (elot-id-move--heading-level target-marker))
         (paste-level  (cond
                        ((eq target-mode 'section-root) (1+ target-level))
                        ((eq as 'child)                  (1+ target-level))
                        (t                                target-level)))
         (source-level (elot-id-move--heading-level source-marker))
         (_ (when (= (marker-position source-marker)
                     (marker-position target-marker))
              (user-error "elot-move-resource: SOURCE equals TARGET")))
         (_ (when (and old-parent
                       (eq as 'child)
                       (eq target-mode 'heading)
                       (= (marker-position old-parent)
                          (marker-position target-marker))
                       (= source-level paste-level))
              (user-error
               "elot-move-resource: TARGET %s is already SOURCE's parent (no-op)"
               target)))
         (_ (when (elot-id-move--source-inside-target-p
                   source-marker target-marker)
              (user-error
               "elot-move-resource: cannot move %s into its own subtree"
               source)))
         ;; Capture parent labels for the result plist before mutating.
         (from-parent-label
          (save-excursion
            (goto-char (or old-parent source-marker))
            (org-back-to-heading t)
            (org-get-heading t t t t)))
         (to-parent-label
          (save-excursion
            (goto-char target-marker)
            (org-back-to-heading t)
            (org-get-heading t t t t))))
    ;; Execute the move atomically.
    (atomic-change-group
      (save-excursion
        (goto-char source-marker)
        (org-back-to-heading t)
        (org-cut-subtree)
        ;; The cut may have moved point; re-resolve target by marker
        ;; (markers track buffer edits automatically).
        (goto-char target-marker)
        (org-back-to-heading t)
        (cond
         ((eq target-mode 'section-root)
          ;; Paste as the first child of the section heading.
          (forward-line 1)
          ;; Skip any property drawer attached to the section.
          (when (looking-at-p ":PROPERTIES:")
            (re-search-forward "^:END:[ \t]*$" nil t)
            (forward-line 1))
          (org-paste-subtree paste-level))
         ((eq as 'child)
          ;; Paste as the last child of TARGET (end of subtree).
          (org-end-of-subtree t t)
          (org-paste-subtree paste-level))
         (t  ; sibling
          (org-end-of-subtree t t)
          (org-paste-subtree paste-level))))
      ;; M9.3.F8: O(1) staleness mark so the cache is refreshed at
      ;; the *next* read.  Previously this site unconditionally
      ;; called the (potentially expensive) full reparse.
      (cond
       ((fboundp 'elot-headline-hierarchy-mark-stale)
        (elot-headline-hierarchy-mark-stale))
       ((fboundp 'elot-update-headline-hierarchy)
        (ignore-errors (elot-update-headline-hierarchy)))))
    (let ((msg (format
                "elot-move-resource: %s moved (%s) %s -> %s"
                source as
                (or from-parent-label "(top-level)")
                (or to-parent-label "(top-level)"))))
      (message "%s" msg))
    (list :source source
          :target target
          :as as
          :from-parent from-parent-label
          :to-parent to-parent-label)))

;;; ---------------------------------------------------------------------------
;;; Shared outline surgery -- heading removal with child promotion
;;; ---------------------------------------------------------------------------

;; Used by:
;;   - `elot-rename-resource' op=merge (`elot-id-rename.el')
;;   - `elot-delete-resource' cascade=reparent (Slice B of M9.9)
;;
;; The operation is a pure outline-level move: take the heading at
;; MARKER, promote each of its direct heading-nested children one
;; outline level (so they reattach to MARKER's former outline parent),
;; then delete MARKER's heading line and its description-list payload.
;;
;; Callers wrap this in their own `atomic-change-group' so a mid-flight
;; failure rolls the entire transaction back.

(defun elot-id-remove-heading-promote-children (marker)
  "Promote MARKER's heading-nested children one outline level and delete it.

MARKER must point at (or inside the immediate vicinity of) a
resource heading.  After this function returns the heading line
plus its description-list payload are gone, and every former
direct child subtree sits one outline level higher (i.e. as a
sibling of MARKER's former outline parent's other children -- or
equivalently, as a child of that parent).

In the F1 (=elot_replace_with_parent=) / merge call site that
former parent IS the replacement TARGET, which is the `safe
semantic weakening' the workflow relies on: a former
sub-Pantherinae becomes a sub-Cat by heading nesting alone, no
new `SubClassOf' row required.

For callers where MARKER's outline parent is /not/ a semantic
parent of the children (e.g. a direct =cascade=reparent= on a
class whose outline parent happens to be a section root), the
promoted children still land under whatever the outline parent
was; that geometry is the caller's responsibility (briefing
9.9.A.3-rename-merge-mode.org, `Out of scope').

Must be called inside an `atomic-change-group' so a failure
rolls the surgery back."
  (unless (markerp marker)
    (error "elot-id-remove-heading-promote-children: MARKER is not a marker: %S"
           marker))
  (save-excursion
    (goto-char marker)
    (org-back-to-heading t)
    ;; Collect direct children up front, then promote in /reverse/
    ;; order.  Promoting in forward order would lift the first child
    ;; to MARKER's outline level, where it sits in the buffer between
    ;; MARKER and the remaining children -- and `org-goto-first-child'
    ;; from MARKER would then fail to find the next still-nested
    ;; child (it stops at the first heading after MARKER, which is
    ;; now the promoted one, at MARKER's level rather than deeper).
    ;; Reverse-order promotion keeps the in-buffer position of the
    ;; un-promoted children stable across each step; markers track
    ;; the original positions through the rewrites.
    (let ((heading-pos (point))
          (children '()))
      (save-excursion
        (goto-char heading-pos)
        (when (and (fboundp 'org-goto-first-child)
                   (org-goto-first-child))
          (push (point-marker) children)
          ;; Walk remaining direct children.  `org-forward-heading-same-level'
          ;; moves point only when a same-level sibling exists; loop until
          ;; it stops advancing.
          (let ((prev nil))
            (while (progn
                     (setq prev (point))
                     (org-forward-heading-same-level 1 t)
                     (/= (point) prev))
              (push (point-marker) children)))))
      ;; `children' is now last-to-first (push order); promote each.
      (dolist (m children)
        (goto-char m)
        (org-promote-subtree)
        (set-marker m nil)))
    ;; MARKER's heading now has no children -- delete the heading line
    ;; plus its description-list payload, up to (but not including)
    ;; the next heading (which, post-promotion, is MARKER's former
    ;; first child -- now a sibling -- or its original next sibling).
    (goto-char marker)
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion
                 (outline-next-heading)
                 (if (eobp) (point) (point)))))
      (delete-region beg end))))

(defun elot-id-delete-heading-subtree (marker)
  "Delete the heading at MARKER together with its entire subtree.

MARKER must point at (or inside the immediate vicinity of) a
resource heading.  After this function returns the heading line,
its description-list payload, and every nested sub-heading +
sub-payload at deeper outline levels are gone.

Companion to `elot-id-remove-heading-promote-children': both
helpers remove the heading at MARKER, but this one consumes the
children too (the `cascade=delete' branch of the forthcoming
`elot_delete_resource' wrapper), where `--promote-children'
lifts them one outline level (the `cascade=reparent' branch and
the `op=merge' rename tail).

Must be called inside an `atomic-change-group' so a failure
rolls the surgery back.  Pure byte-level operation; the caller
is responsible for any reference-scan / lint / OMN revalidation
around the call site."
  (unless (markerp marker)
    (error "elot-id-delete-heading-subtree: MARKER is not a marker: %S"
           marker))
  (save-excursion
    (goto-char marker)
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion
                 (org-end-of-subtree t t)
                 (point))))
      (delete-region beg end))))

(provide 'elot-id-move)
;;; elot-id-move.el ends here
