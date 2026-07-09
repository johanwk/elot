;;; elot-subheading-relation-test.el --- Individual subheading-relation  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the individual "subheading-relation" feature described in
;; briefings/individual-SKOS-broader-relation.org.
;;
;; When an individual-declaring heading X carries an
;; :ELOT-subheading-relation: drawer property (e.g. "skos:broader"),
;; every individual heading *below* X -- at any nesting depth -- gets a
;; description-list axiom
;;
;;     Facts: <relation> <immediate-parent-uri>
;;
;; where the target is the *immediate* parent individual (not X).  The
;; declaring heading X itself gets no such Facts.  A deeper
;; re-declaration overrides the relation for that subtree.
;;
;; The test is a pure AST -> OMN string check: it parses a small Org
;; fixture into the ELOT headline hierarchy and runs
;; `elot-omn-resource-declarations' on the ontology node, asserting on
;; the returned OMN text.  No ROBOT / Java / network required.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'elot-tangle)

(defconst elot-sr-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file.")

(defun elot-sr-test--fx (name)
  "Absolute path of fixture NAME under test/fixtures/."
  (expand-file-name (concat "fixtures/" name) elot-sr-test--dir))

(defun elot-sr-test--omn (name)
  "Parse fixture NAME and return the OMN for its first ontology node.
Returns just the resource frames (no prefix block), mirroring the
entrypoint `elot-get-ontology-node-omn' but without prefixes so the
assertions never accidentally match a `Prefix:' line."
  (with-temp-buffer
    (insert-file-contents (elot-sr-test--fx name))
    (delay-mode-hooks (org-mode))
    (elot-update-headline-hierarchy)
    (let ((ont (car (plist-get elot-headline-hierarchy :children))))
      (should ont)
      (elot-omn-resource-declarations (list ont)))))

(defun elot-sr-test--frame (omn header)
  "Return the single OMN frame in OMN whose first line is HEADER.
Frames are separated by a blank line and never contain one internally,
so splitting on \"\\n\\n\" isolates them."
  (cl-find-if (lambda (chunk) (string-prefix-p header chunk))
              (split-string omn "\n\n" t)))

;;; ---------------------------------------------------------------------------
;;; Inheritance to arbitrary depth; target is always the immediate parent.

(ert-deftest elot-sr-inherits-relation-to-descendants ()
  "Every descendant individual gets Facts <relation> <immediate-parent>."
  (let* ((omn     (elot-sr-test--omn "skos-individuals.org"))
         (mammal  (elot-sr-test--frame omn "Individual: ex:mammal"))
         (dog     (elot-sr-test--frame omn "Individual: ex:dog"))
         (red     (elot-sr-test--frame omn "Individual: ex:red")))
    ;; All four individuals declared.
    (should (string-match-p "Individual: ex:animal" omn))
    (should mammal)
    (should dog)
    (should red)
    ;; Depth 2: immediate parent is the declaring heading (ex:animal).
    (should (string-match-p
             "Facts:[[:space:]]+skos:broader[[:space:]]+ex:animal" mammal))
    ;; Depth 3: relation inherited past a heading with no drawer of its
    ;; own; target is the immediate parent (ex:mammal), proving
    ;; "children of children" works.
    (should (string-match-p
             "Facts:[[:space:]]+skos:broader[[:space:]]+ex:mammal" dog))
    ;; Sibling subtree with a *different* relation -> per-subtree scoping.
    (should (string-match-p
             "Facts:[[:space:]]+skos:related[[:space:]]+ex:colour" red))))

;;; ---------------------------------------------------------------------------
;;; The declaring heading itself receives no Facts from this feature.

(ert-deftest elot-sr-declaring-heading-has-no-facts ()
  "A heading that *declares* the relation gets no Facts (no ancestor above)."
  (let* ((omn    (elot-sr-test--omn "skos-individuals.org"))
         (animal (elot-sr-test--frame omn "Individual: ex:animal"))
         (colour (elot-sr-test--frame omn "Individual: ex:colour")))
    (should animal)
    (should colour)
    (should-not (string-match-p "Facts:" animal))
    (should-not (string-match-p "Facts:" colour))))

;;; ---------------------------------------------------------------------------
;;; Negative: the Facts target is the immediate parent, not the ancestor
;;; that declared the relation.

(ert-deftest elot-sr-target-is-immediate-parent-not-ancestor ()
  "The depth-3 individual relates to its parent, not the declaring root."
  (let* ((omn (elot-sr-test--omn "skos-individuals.org"))
         (dog (elot-sr-test--frame omn "Individual: ex:dog")))
    (should dog)
    ;; ex:dog -> ex:mammal, emphatically NOT ex:dog -> ex:animal.
    (should (string-match-p
             "Facts:[[:space:]]+skos:broader[[:space:]]+ex:mammal" dog))
    (should-not (string-match-p "ex:animal" dog))))

;;; ---------------------------------------------------------------------------
;;; Absent the drawer property, individual nesting emits no Facts at all.

(ert-deftest elot-sr-no-relation-no-facts ()
  "Nested individuals without an :ELOT-subheading-relation: get no Facts.
Guards against regressing pre-existing files: this feature must be
completely inert unless the drawer property is present."
  (let ((omn (elot-sr-test--omn "skos-no-relation.org")))
    (should (string-match-p "Individual: ex:mammal" omn))
    (should (string-match-p "Individual: ex:dog" omn))
    (should-not (string-match-p "Facts:" omn))))

(provide 'elot-subheading-relation-test)
;;; elot-subheading-relation-test.el ends here
