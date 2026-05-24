;;; elot-lint.el --- ELOT lint checks for ontology buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Keywords: languages tools org ontology

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Lint checks for ELOT (Emacs Literate Ontology Tool) buffers.
;; Registers custom org-lint checkers for ontology-specific validation:
;; heading identifiers, prefix tables, required sections, description
;; list CURIEs, and axiom value checks.
;;
;; Autoloaded from elot-mode.el; loaded on demand when the user calls
;; M-x elot-org-lint.

;;; Code:

(require 'org-element)
(require 'org-lint)
(require 'elot-tangle)
(require 'elot-label-display)
(require 'elot-owl-grammar)

;; Register ELOT-specific SPARQL src-block header arguments so org-lint's
;; `unknown-header-argument' checker accepts them.  ELOT routinely uses
;; `:url' (to point the query at a tangled OMN file) and `:format' (to
;; select the result serialisation, e.g. ttl) on sparql blocks.
;;
;; Note: ob-sparql defines `org-babel-default-header-args:sparql' (which
;; supplies default *values*) but never binds `org-babel-header-args:sparql'
;; (which declares the set of *permitted* keys -- this is what org-lint's
;; `org-babel-check-src-block-header-arguments' consults).  We therefore
;; initialise the variable ourselves and append our extras idempotently.
(defvar org-babel-header-args:sparql nil
  "Permitted header arguments for SPARQL src-blocks.
ob-sparql does not bind this; ELOT initialises it so org-lint's
`unknown-header-argument' checker recognises `:url' and `:format'.")
(dolist (arg '((url . :any) (format . :any)))
  (unless (assq (car arg) org-babel-header-args:sparql)
    (push arg org-babel-header-args:sparql)))

(defun elot--resourcedefs-here-p ()
  "Return t if headline at point sets :resourcedefs: yes."
  (string-equal (org-entry-get nil "resourcedefs") "yes"))

(defun elot--inside-resourcedefs-p ()
  "Return t when point is inside any :resourcedefs: yes section."
  (string-equal (org-entry-get-with-inheritance "resourcedefs") "yes"))

(defun elot--inside-ontology-context-p ()
  "Return t when point is under a heading with :ELOT-context-type: ontology."
  (string-equal (org-entry-get-with-inheritance "ELOT-context-type") "ontology"))

(defun elot--inside-elot-scope-p ()
  "Return t when point is inside
an ELOT ontology context AND a :resourcedefs: section.
This is the combined guard for ELOT-specific lint checkers: only items
under a heading with :ELOT-context-type: ontology that also inherit
:resourcedefs: yes should be checked."
  (and (elot--inside-ontology-context-p)
       (elot--inside-resourcedefs-p)))

(defun elot--heading-nodeclare-p ()
  "Return t when the heading at point or any ancestor has tag :nodeclare:.
Walks up the outline tree checking each ancestor's tags."
  (save-excursion
    (let ((found nil))
      (while (and (not found) (org-up-heading-safe))
        (when (member "nodeclare" (org-get-tags nil t))
          (setq found t)))
      found)))

(defun elot--item-under-nodeclare-p (pos)
  "Return t when POS is under a heading tagged :nodeclare:.
Checks the immediately enclosing headline and its ancestors."
  (save-excursion
    (goto-char pos)
    (if (org-before-first-heading-p)
        nil
      (org-back-to-heading-or-point-min t)
      (or (member "nodeclare" (org-get-tags nil t))
          (elot--heading-nodeclare-p)))))

(declare-function elot-entity-from-header "elot-tangle")
(declare-function elot-unprefix-uri "elot-tangle")
(declare-function elot-context-type "elot-tangle")
(declare-function elot-context-localname "elot-tangle")
(declare-function elot-default-prefix "elot-tangle")
(declare-function elot-governing-hierarchy "elot-tangle")
(declare-function elot-update-link-abbrev "elot-tangle")

;; A function that goes in the ELOT menu. First we refresh elot-slurp
;; with elot-label-display-setup, to pick up recent changes that may
;; cause problems.
(declare-function elot-label-display-setup "elot-label-display")
(defun elot-org-lint ()
  "Refresh `elot-slurp', then do `org-lint'"
  (interactive)
  (elot-label-display-setup)
  (call-interactively #'org-lint))

(defun elot-check-nodeclare-id-prefix-label (tree)
  "ELOT rule: check ID, prefix, and label format under :resourcedefs:."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (elot--inside-elot-scope-p)
                   (not (elot--resourcedefs-here-p)))
          (let* ((title (org-get-heading nil t))
                 (tags  (org-element-property :tags hl))
                 (nodeclare (member "nodeclare" tags)))
            (unless nodeclare
              (let* ((entity (condition-case nil
                                 (elot-entity-from-header title 'noerror)
                               (error nil)))
                     (label (if (and title (string-match "\\`\\(.*\\) (\\([^()]+\\))\\'" title))
                                 (match-string 1 title)
                               title)))
                (cond
                 ((not entity)
                  (push (list (point)
                              (propertize "ERROR: No identifier found – add CURIE/URI or tag :nodeclare:"
                                          'face 'error))
                        issues))
                 ((not (stringp entity))
                  (push (list (point)
                              (propertize "ERROR: Malformed identifier value"
                                          'face 'error))
                        issues))
                 ((null (elot-unprefix-uri (substring-no-properties entity) org-link-abbrev-alist-local 'noerror))
                  (push (list (point)
                              (propertize "ERROR: Unknown prefix in identifier"
                                          'face 'error))
                        issues))
                 ((not (or (not (string-match-p "\"" label))
                           (string-match-p "^\"[^\"]+\"@[[:alpha:]-]+$" label)))
                  (push (list (point)
                              (propertize "WARNING: Label should have no quotes or be a \"text\"@lang string"
                                          'face 'warning))
                        issues)))))))
        nil)
      tree)
    issues))

(defun elot--check-omn-args (omn-args point issues)
  "Check :header-args:omn string OMN-ARGS for valid :tangle file and :noweb yes.
Add warnings or errors to ISSUES at POINT."
  ;; Check for :tangle filename
  (if (and (string-match ":tangle[ \t]+\\([^ \t:]+\\.omn\\)" omn-args)
         (match-string 1 omn-args))
    (let ((file (match-string 1 omn-args)))
      (condition-case nil
          (expand-file-name file)
        (error
         (push (list point
                     (propertize (format "ERROR: Invalid tangle file path: %s" file)
                                 'face 'error))
               issues))))
  (push (list point
              (propertize "ERROR: :tangle missing or invalid in :header-args:omn"
                          'face 'error))
        issues))
  ;; Check for :noweb yes followed by whitespace or end of string
  (unless (string-match ":noweb[ \t]+yes\\(\\s-\\|\\'\\)" omn-args)
    (push (list point
                (propertize "ERROR: :noweb yes missing or malformed in :header-args:omn"
                            'face 'error))
          issues))
  issues)


(defun elot-check-ontology-header (tree)
  "ELOT rule: check top-level ontology header properties."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (= (org-element-property :level hl) 1)
                   (string= (elot-context-type) "ontology"))
          (let* ((id (org-entry-get nil "ID"))
                 (localname (elot-context-localname))
                 (omn-args (org-entry-get nil "header-args:omn")))
            (when (or (null id) (string= id ""))
              (push (list (point)
                          (propertize "ERROR: Top-level heading missing :ID:" 'face 'error))
                    issues))
            (when (and id localname (not (string= id localname)))
              (push (list (point)
                          (propertize "WARNING: :ELOT-context-localname: should match :ID:" 'face 'warning))
                    issues))
            ;; Run helper check on :header-args:omn
            (setq issues (elot--check-omn-args omn-args (point) issues)))))
      tree)
    issues))

(org-lint-add-checker
 'elot/nodeclare-id-prefix-label
 "ELOT: heading must declare ID with known prefix and valid label, or carry :nodeclare:"
 #'elot-check-nodeclare-id-prefix-label
 :categories '(default elot)
 :trust 'high)

(org-lint-add-checker
 'elot/ontology-header
 "ELOT: top-level ontology heading must have required properties"
 #'elot-check-ontology-header
 :categories '(default elot)
 :trust 'high)

(defun elot-check-prefix-table (tree)
  "ELOT rule: ensure `elot-update-link-abbrev` sets useful abbrevs."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (and (= (org-element-property :level hl) 1)
                   (string= (elot-context-type) "ontology"))
          (save-excursion
            (goto-char (org-element-property :begin hl))
            (elot-update-link-abbrev)
            (when (or (null org-link-abbrev-alist-local)
                      (equal org-link-abbrev-alist-local '(("prefix" . "uri"))))
              (push (list (point)
                          (propertize "ERROR: prefix-table is missing or malformed"
                                      'face 'error))
                    issues))))))
    issues))

(org-lint-add-checker
 'elot/prefix-table
 "ELOT: ontology section must contain #+name: prefix-table that updates abbrevs"
 #'elot-check-prefix-table
 :categories '(default elot)
 :trust 'high)

(defun elot-check-ontology-presence (tree)
  "ELOT rule: ensure there is at least one top-level ontology section."
  (let ((found nil)
        issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) 1)
          (goto-char (org-element-property :begin hl))
          (when (string= (org-entry-get nil "ELOT-context-type") "ontology")
            (setq found t))))
      tree)
    (unless found
      (push (list 1  ;; top of buffer
                  (propertize "ERROR: No top-level heading with :ELOT-context-type: ontology"
                              'face 'error))
            issues))
    issues))


(org-lint-add-checker
 'elot/ontology-presence
 "ELOT: file must contain at least one ontology top-level heading"
 #'elot-check-ontology-presence
 :categories '(default elot)
 :trust 'high)


(defun elot-check-required-sections (tree)
  "ELOT rule: check all required section headers for ontology."
  (let (localname issues)
    ;; Find top-level ontology headline to get localname
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (when (and (= (org-element-property :level hl) 1)
                   (string= (org-entry-get nil "ELOT-context-type") "ontology"))
          (setq localname (org-entry-get nil "ELOT-context-localname"))))
      nil t) ;; stop after first match

    (when localname
      (let* ((suffixes '("-ontology-declaration"
                         "-datatypes"
                         "-class-hierarchy"
                         "-object-property-hierarchy"
                         "-data-property-hierarchy"
                         "-annotation-property-hierarchy"
                         "-individuals"))
             (required-ids (mapcar (lambda (suffix) (concat localname suffix)) suffixes))
             (headline-alist '()))
        ;; Build alist with ID as (resourcedefs begin)
        (org-element-map tree 'headline
          (lambda (hl)
            (goto-char (org-element-property :begin hl))
            (let ((id (org-entry-get nil "ID"))
                  (resourcedefs (org-entry-get nil "resourcedefs"))
                  (pos (point)))
              (when id
                (push (cons id (list resourcedefs pos))
                      headline-alist)))))

        ;; Check required sections
        (dolist (req-id required-ids)
          (let ((entry (assoc req-id headline-alist)))
            (if (null entry)
                (push (list (point-min)
                            (propertize (format "WARNING: Missing section with ID %s" req-id)
                                        'face 'warning))
                      issues)
              (let* ((resourcedefs (nth 0 (cdr entry)))
                     (pos (nth 1 (cdr entry))))
                (when (not (string= resourcedefs "yes"))
                  (push (list pos
                              (propertize (format "WARNING: Section %s should have :resourcedefs: yes" req-id)
                                          'face 'warning))
                        issues))))))))
    issues))


(org-lint-add-checker
 'elot/required-sections
 "ELOT: ontology must have required resource sections with proper properties"
 #'elot-check-required-sections
 :categories '(default elot)
 :trust 'high)


(defun elot-check-ontology-declaration-heading (tree)
  "ELOT rule: the ontology-declaration heading title must declare an identifier.
The heading whose :ID: ends with -ontology-declaration carries the
ontology IRI (and optional version IRI) inside its title, e.g.
\"** my-ont ontology (exo:my-ont exo:my-ont/0.0)\".  If the
parenthetical content does not parse to a valid CURIE/URI (or
CURIE+version pair), the tangler silently omits the
\"Ontology: ...\" block from the generated OMN file.  This check
catches that class of typo at lint time."
  (let (issues)
    (org-element-map tree 'headline
      (lambda (hl)
        (goto-char (org-element-property :begin hl))
        (let ((id (org-entry-get nil "ID")))
          (when (and id (string-suffix-p "-ontology-declaration" id))
            (let* ((title (org-get-heading nil t t t))
                   (entity (condition-case nil
                               (elot-entity-from-header title 'noerror)
                             (error nil))))
              (cond
               ((null entity)
                (push (list (point)
                            (propertize
                             (format
                              "ERROR: Ontology-declaration heading \"%s\" does not parse to a valid IRI/CURIE; the Ontology: block will be missing from the OMN output.  Expected e.g. \"label (prefix:localname prefix:localname/version)\"."
                              title)
                             'face 'error))
                      issues))
               ((not (stringp entity))
                (push (list (point)
                            (propertize
                             "ERROR: Malformed ontology identifier in heading"
                             'face 'error))
                      issues))
               ((null (elot-unprefix-uri
                       (substring-no-properties
                        ;; Take just the first token (the ontology IRI),
                        ;; not the optional version IRI that follows.
                        (car (split-string entity " " t)))
                       org-link-abbrev-alist-local 'noerror))
                (push (list (point)
                            (propertize
                             (format "ERROR: Unknown prefix in ontology identifier: %s" entity)
                             'face 'error))
                      issues)))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/ontology-declaration-heading
 "ELOT: ontology-declaration heading title must declare a valid IRI/CURIE"
 #'elot-check-ontology-declaration-heading
 :categories '(default elot)
 :trust 'high)


(defconst elot-known-annotation-properties
  '(;; RDFS built-ins
    "rdfs:label" "rdfs:comment" "rdfs:seeAlso" "rdfs:isDefinedBy"
    ;; OWL 2 built-in annotation properties (pre-declared by the OWL spec;
    ;; do not require a heading under <ontology>-annotation-property-hierarchy)
    "owl:versionInfo" "owl:deprecated" "owl:priorVersion"
    "owl:backwardCompatibleWith" "owl:incompatibleWith")
  "List of annotation properties allowed in description lists without declaration.
Covers the RDFS and OWL 2 built-ins that the OWL specification treats as
pre-declared annotation properties.  Any other CURIE used as a description-list
tag must be declared as an `owl:AnnotationProperty' in the active sources
(slurped into `elot-slurp') or the lint checker
`elot/description-list-curies' will warn.")

(defun elot-check-description-list-curies (tree)
  "Check CURIE terms in description lists are declared annotation properties."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((tag (org-element-property :tag item))
                     (term (org-element-interpret-data tag)))
                (when (and (stringp term)
                           (string-match "\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'" term))
                  (unless (or
                           ;; allowed exceptions
                           (member term elot-known-annotation-properties)
                           ;; declared in elot-slurp as AnnotationProperty
                           (cl-find term elot-slurp
                                    :key #'car
                                    :test #'string=
                                    :if (lambda (x)
                                          (let ((plist (nth 2 x)))
                                            (and (plist-get plist "rdf:type")
                                                 (string= (plist-get plist "rdf:type")
                                                          "owl:AnnotationProperty"))))))
                    (push (list (org-element-property :begin item)
                                (propertize (format "WARNING: Unknown or invalid annotation property: %s" term)
                                            'face 'warning))
                          issues))))))))
      tree)
    issues))


;; the following not in use (yet), for warning about unknown prefixes in contents of annotations 
;; (defun elot-check-annotation-value-prefixes (contents)
;;   "Check for unknown prefixes in annotation value CONTENTS.
;; Return a list of warning strings, or nil if no issues."
;;   (let (warnings)
;;     (let ((words (split-string contents "[ \n\t,]+" t)))
;;       (dolist (word words)
;;         (when (string-match "\\`\\([-_./[:alnum:]]*\\):[-_./[:alnum:]]*\\'" word)
;;           (let* ((prefix (match-string 1 word))
;;                  (prefix-colon (concat prefix ":"))
;;                  (known-prefix (or (member prefix-colon elot-owl-builtin-resources)
;;                                    (cl-find prefix-colon elot-slurp :key #'car :test #'string=))))
;;             (unless known-prefix
;;               (push (format "WARNING: Unknown prefix in annotation value: %s" word)
;;                     warnings))))))
;;     warnings))


(org-lint-add-checker
 'elot/description-list-curies
 "ELOT: check that CURIE terms in description lists are declared"
 #'elot-check-description-list-curies
 :categories '(default elot)
 :trust 'high)


(defun elot-string-balanced-parentheses-p (str)
  "Return t if STR has balanced parentheses, nil otherwise."
  (let ((count 0)
        (balanced t)
        (i 0))
    (while (and balanced (< i (length str)))
      (let ((ch (aref str i)))
        (cond
         ((= ch ?\() (setq count (1+ count)))
         ((= ch ?\)) (setq count (1- count))
          (when (< count 0)
            (setq balanced nil)))))
      (setq i (1+ i)))
    (and balanced (= count 0))))

(defun elot-check-axiom-value-curies (tree)
  "Check that CURIEs in the value of axioms (Manchester syntax) are declared
and not annotation properties, and that parentheses are balanced."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((hierarchy-id
                      (save-excursion
                        (goto-char (org-element-property :begin item))
                        (elot-governing-hierarchy)))
                     (in-annotation-section
                      (and hierarchy-id
                           (string-match-p "annotation-property-hierarchy$" hierarchy-id))))
                (let* ((tag (org-element-property :tag item))
                       (term (org-element-interpret-data tag))
                       ;; exclude sublists from contents
                       (contents (org-element-interpret-data
                                  (seq-remove (lambda (child)
                                                (eq (org-element-type child) 'plain-list))
                                              (org-element-contents item)))))
                  ;; Only apply check if term is a Manchester keyword.
                  ;; Exclude `Import': the identifier of an imported
                  ;; ontology is by nature external and won't appear in
                  ;; the local signature, so a "known CURIE" check is
                  ;; never appropriate there.
                  (when (and (member term elot-omn-all-keywords)
                             (not (string= term "Import")))
                    ;; Check CURIEs
                    (let ((curies (seq-filter (lambda (word)
                                                (and (string-match "\\`[-_./[:alnum:]]*:[-_/.[:alnum:]]*\\'" word)
                                                     (not (string-match "\\`https?://" word))))
                                              (split-string contents "[ \n\t,]+" t))))
                      (dolist (curie curies)
                        (let ((entry (cl-find curie elot-slurp :key #'car :test #'string=)))
                          (cond
                           ((or (member curie elot-owl-builtin-resources)
                                entry)
                            ;; Built-in or known: only warn if it's an annotation property
                            (when (and entry
                                       (not in-annotation-section)
                                       (string= (plist-get (nth 2 entry) "rdf:type" #'equal)
                                                "owl:AnnotationProperty"))
                              (push (list (org-element-property :begin item)
                                          (propertize (format "WARNING: Annotation property used in axiom: %s" curie)
                                                      'face 'warning))
                                    issues)))
                           (t
                            (push (list (org-element-property :begin item)
                                        (propertize (format "WARNING: Unknown CURIE in axiom: %s; declare it via M-x elot-id-insert-sibling-resource (or rename to an existing CURIE)" curie)
                                                    'face 'warning))
                                  issues))))))
                    ;; Check balanced parentheses
                    (unless (elot-string-balanced-parentheses-p contents)
                      (push (list (org-element-property :begin item)
                                  (propertize "WARNING: Unbalanced parentheses in axiom value"
                                              'face 'warning))
                            issues)))))))))
      tree)
    issues))



(org-lint-add-checker
 'elot/axiom-value-curies
 "ELOT: Check CURIEs in axiom description values are defined and not annotation properties"
 #'elot-check-axiom-value-curies
 :categories '(default elot)
 :trust 'high)



;;; ---- OWL Manchester Syntax validation via PEG grammar ----

(defconst elot-omn-keyword-parser-alist
  '(("SubClassOf"     . elot-parse-class-expression-list)
    ("EquivalentTo"   . elot-parse-class-expression-list)
    ("DisjointWith"   . elot-parse-class-expression-list)
    ("DisjointUnionOf" . elot-parse-class-expression-list)
    ("Domain"         . elot-parse-class-or-data-range)
    ("Range"          . elot-parse-class-or-data-range)
    ("Types"          . elot-parse-class-expression-list)
    ("InverseOf"      . elot-parse-property-expression-list)
    ("SubPropertyOf"  . elot-parse-property-expression-list)
    ("SubPropertyChain" . elot-parse-sub-property-chain)
    ("Facts"          . elot-parse-fact)
    ("SameAs"         . elot-parse-individual-iri-list)
    ("DifferentFrom"  . elot-parse-individual-iri-list)
    ;; Misc category keywords (description2List / property2List / individual2List)
    ("DisjointClasses"      . elot-parse-class-expression-2-list)
    ("EquivalentClasses"    . elot-parse-class-expression-2-list)
    ("DisjointProperties"   . elot-parse-property-expression-2-list)
    ("EquivalentProperties" . elot-parse-property-expression-2-list)
    ("SameIndividual"       . elot-parse-individual-2-list)
    ("DifferentIndividuals" . elot-parse-individual-2-list))
  "Alist mapping OMN keywords to their PEG parser entry-point functions.")

(defun elot-parse-class-or-data-range (input)
  "Parse INPUT as either a class expression list or a data range.
Tries `elot-parse-class-expression-list' first; if that fails, falls
back to `elot-parse-data-range'.  This allows \='Range\=' and \='Domain\='
to accept both class expressions (object properties) and data ranges
(data properties) without requiring section-context awareness.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (let ((result (elot-parse-class-expression-list input)))
    (if (eq result t)
        t
      (elot-parse-data-range input))))

(defun elot-parse-class-expression-list (input)
  "Parse INPUT as a comma-separated list of OWL class expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg class-expression-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-class-expression (input)
  "Parse INPUT as an OWL Manchester Syntax class expression.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg class-expression))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-property-expression-list (input)
  "Parse INPUT as a comma-separated list of OWL property expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg object-property-expression-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-property-expression (input)
  "Parse INPUT as an OWL Manchester Syntax object property expression.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg object-property-expression))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-sub-property-chain (input)
  "Parse INPUT as an OWL Manchester Syntax sub-property chain.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg sub-property-chain))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-data-range (input)
  "Parse INPUT as an OWL Manchester Syntax data range.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg data-range))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-fact (input)
  "Parse INPUT as an OWL Manchester Syntax fact.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg fact))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-individual-iri-list (input)
  "Parse INPUT as a comma-separated list of individuals.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg individual-iri-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-class-expression-2-list (input)
  "Parse INPUT as a comma-separated list of at least 2 class expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg class-expression-2-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-property-expression-2-list (input)
  "Parse INPUT as a comma-separated list of at least 2 property expressions.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg object-property-expression-2-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot-parse-individual-2-list (input)
  "Parse INPUT as a comma-separated list of at least 2 individuals.
Return t if the entire string is consumed, or the 1-based column
position where parsing stopped on failure."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (condition-case nil
        (with-peg-rules (elot-owl-grammar)
          (let ((ok (peg-run (peg individual-2-list))))
            (if (and ok (eobp)) t (point))))
      (error (point)))))

(defun elot--format-parse-error (term contents fail-pos)
  "Format an OMN parse error message for keyword TERM.
CONTENTS is the input string, FAIL-POS is the 1-based position
where parsing stopped (an integer), or nil if position is unknown."
  (if (and (integerp fail-pos)
           (<= fail-pos (length contents)))
      (let* ((col fail-pos)
             (before (substring contents 0 (min (1- col) (length contents))))
             (after (substring contents (min (1- col) (length contents))))
             ;; Truncate context for readability
             (before-ctx (if (> (length before) 20)
                             (concat "..." (substring before -20))
                           before))
             (after-ctx (if (> (length after) 20)
                            (concat (substring after 0 20) "...")
                          after)))
        (format "ERROR: Invalid %s at column %d: \"%s▶%s\""
                term col before-ctx after-ctx))
    ;; Fallback: no position info
    (format "ERROR: Invalid %s expression: %s"
            term
            (if (> (length contents) 60)
                (concat (substring contents 0 57) "...")
              contents))))

(defun elot-check-omn-syntax (tree)
  "ELOT rule: validate OWL Manchester Syntax in axiom description list values.
For each description list item whose tag is an OMN keyword with a known
parser (see `elot-omn-keyword-parser-alist'), parse the value and report
an error if it does not conform to the grammar.  When the parser reports
a failure position, the error message indicates the exact column.
Only items inside an ELOT ontology context (:ELOT-context-type: ontology)
and :resourcedefs: yes section are checked.  Headings tagged :nodeclare:
are skipped."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((tag (org-element-property :tag item))
                     (term (org-element-interpret-data tag))
                     (parser-fn (cdr (assoc term elot-omn-keyword-parser-alist))))
                (when parser-fn
                  ;; Extract the value text, excluding sublists (meta-annotations)
                  (let* ((contents-raw
                          (org-element-interpret-data
                           (seq-remove (lambda (child)
                                         (eq (org-element-type child) 'plain-list))
                                       (org-element-contents item))))
                         ;; Clean up: remove leading/trailing whitespace and
                         ;; trailing newlines left by org-element-interpret-data
                         (contents (string-trim contents-raw))
                         (result (when (not (string-empty-p contents))
                                   (funcall parser-fn contents))))
                    (when (and (not (string-empty-p contents))
                               (not (eq result t)))
                      (push (list (org-element-property :begin item)
                                  (propertize
                                   (elot--format-parse-error term contents result)
                                   'face 'error))
                            issues)))))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/omn-syntax
 "ELOT: validate OWL Manchester Syntax in axiom values"
 #'elot-check-omn-syntax
 :categories '(default elot)
 :trust 'high)


;;; ---- OMN keyword appropriateness per section type ----

;; Each section type maps to the OMN keywords that are valid in description
;; lists under its headings, per the OWL 2 Manchester Syntax specification
;; (W3C Recommendation, Section 2.5 Frames and Miscellaneous).

(defconst elot-omn-keywords-by-section
  '(("-datatypes"
     "EquivalentTo")
    ("-class-hierarchy"
     "SubClassOf" "EquivalentTo" "DisjointWith" "DisjointUnionOf" "HasKey")
    ("-object-property-hierarchy"
     "SubPropertyOf" "EquivalentTo" "DisjointWith" "Domain" "Range"
     "Characteristics" "InverseOf" "SubPropertyChain")
    ("-data-property-hierarchy"
     "SubPropertyOf" "EquivalentTo" "DisjointWith" "Domain" "Range"
     "Characteristics")
    ("-annotation-property-hierarchy"
     "SubPropertyOf" "Domain" "Range")
    ("-individuals"
     "Types" "Facts" "SameAs" "DifferentFrom"))
  "Alist mapping ELOT section ID suffixes to allowed OMN frame keywords.
Derived from the OWL 2 Manchester Syntax specification, Section 2.5.")

(defconst elot-omn-kind-to-section-suffix
  '(("rdfs:Datatype"           . "-datatypes")
    ("owl:Class"               . "-class-hierarchy")
    ("owl:ObjectProperty"      . "-object-property-hierarchy")
    ("owl:DatatypeProperty"    . "-data-property-hierarchy")
    ("owl:AnnotationProperty"  . "-annotation-property-hierarchy")
    ("owl:NamedIndividual"     . "-individuals"))
  "Bridge between `rdf:type' CURIEs and `elot-omn-keywords-by-section'
suffixes.  Single source of truth shared by `elot-lint' (section-keyed,
fires at the row's line/column) and `elot-gptel' (kind-keyed, drives
the LLM authoring helper `elot_axiom_keywords').  Step 9.2.b.0.i.")

(defun elot-omn-keywords-for-kind (kind)
  "Return the allowed OMN frame keywords for an `rdf:type' KIND CURIE.
KIND is a string such as \"owl:Class\" or \"owl:ObjectProperty\".
Returns nil for unrecognised kinds.  Bridges `elot-omn-kind-to-
section-suffix' with `elot-omn-keywords-by-section'."
  (let ((suffix (cdr (assoc kind elot-omn-kind-to-section-suffix))))
    (and suffix (cdr (assoc suffix elot-omn-keywords-by-section)))))

(declare-function elot-governing-section-id "elot-tangle")

(defun elot--section-suffix-from-id (section-id)
  "Extract the well-known section suffix from SECTION-ID.
Return e.g. \"-class-hierarchy\" from \"pizza-class-hierarchy\",
or nil if no known suffix matches."
  (when section-id
    (cond
     ((string-suffix-p "-datatypes" section-id) "-datatypes")
     ((string-suffix-p "-class-hierarchy" section-id) "-class-hierarchy")
     ((string-suffix-p "-object-property-hierarchy" section-id)
      "-object-property-hierarchy")
     ((string-suffix-p "-data-property-hierarchy" section-id)
      "-data-property-hierarchy")
     ((string-suffix-p "-annotation-property-hierarchy" section-id)
      "-annotation-property-hierarchy")
     ((string-suffix-p "-individuals" section-id) "-individuals"))))

(defun elot-check-omn-keyword-appropriateness (tree)
  "ELOT rule: check that OMN keywords are appropriate for their section context.
For each description list item whose tag is an OMN keyword, verify that the
keyword is valid for the enclosing ELOT section (Classes, Object properties,
etc.) according to the OWL 2 Manchester Syntax specification.
Only checks items inside an ELOT ontology context and :resourcedefs: section.
Headings tagged :nodeclare: are skipped."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((tag (org-element-property :tag item))
                     (term (org-element-interpret-data tag)))
                ;; Only check OMN frame keywords (not annotation CURIEs like
                ;; rdfs:comment).  Skip "misc" keywords (EquivalentClasses,
                ;; DisjointClasses, etc.) which are not bound to any frame
                ;; and are valid in any resource-defining section.
                (when (and (member term elot-omn-all-keywords)
                           (not (member term elot-omn-misc-keywords)))
                  (let* ((section-id
                          (save-excursion
                            (goto-char (org-element-property :begin item))
                            (elot-governing-section-id)))
                         (suffix (elot--section-suffix-from-id section-id))
                         (allowed (cdr (assoc suffix elot-omn-keywords-by-section))))
                    ;; Only flag if we are inside a known section and the
                    ;; keyword is NOT in the allowed list for that section
                    (when (and suffix allowed
                               (not (member term allowed)))
                      (let ((section-name
                             (cond
                              ((string= suffix "-datatypes") "Datatypes")
                              ((string= suffix "-class-hierarchy") "Classes")
                              ((string= suffix "-object-property-hierarchy")
                               "Object properties")
                              ((string= suffix "-data-property-hierarchy")
                               "Data properties")
                              ((string= suffix "-annotation-property-hierarchy")
                               "Annotation properties")
                              ((string= suffix "-individuals") "Individuals"))))
                        (push (list (org-element-property :begin item)
                                    (propertize
                                     (format "ERROR: \"%s\" is not valid in %s section (allowed: %s)"
                                             term section-name
                                             (string-join allowed ", "))
                                     'face 'error))
                              issues))))))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/omn-keyword-appropriateness
 "ELOT: check OMN keywords are appropriate for their section type"
 #'elot-check-omn-keyword-appropriateness
 :categories '(default elot)
 :trust 'high)


;;; ---- Axiom value-kind (category) check -- Step 9.2.b.0.iv ----
;;
;; `elot/omn-keyword-appropriateness' already catches the wrong KEY for
;; the subject's section (e.g. `Domain' on a Class).  `elot/axiom-value-
;; curies' already catches undeclared CURIEs in axiom values.  Both miss
;; the most common authoring mistake: a *declared* CURIE used in the
;; wrong OWL category position -- e.g. `SubClassOf :: ex:isAfraidOf'
;; when ex:isAfraidOf is an ObjectProperty.  This checker fills the gap.
;;
;; Tokeniser is conservative: only flags bare-CURIE leaves whose
;; declared `rdf:type' disagrees with the keyword's expected leaf kind.
;; Class expressions (`some' / `only' / parens / ...) are deferred to
;; the PEG grammar -- a leaf walk through them would risk false
;; positives without a real class-expression walker.

(defconst elot-omn-keyword-value-kind
  '(("-class-hierarchy"
     ("SubClassOf"      . "owl:Class")
     ("EquivalentTo"    . "owl:Class")
     ("DisjointWith"    . "owl:Class")
     ("DisjointUnionOf" . "owl:Class"))
    ("-object-property-hierarchy"
     ("SubPropertyOf"    . "owl:ObjectProperty")
     ("EquivalentTo"     . "owl:ObjectProperty")
     ("DisjointWith"     . "owl:ObjectProperty")
     ("InverseOf"        . "owl:ObjectProperty")
     ("SubPropertyChain" . "owl:ObjectProperty")
     ("Domain"           . "owl:Class")
     ("Range"            . "owl:Class"))
    ("-data-property-hierarchy"
     ("SubPropertyOf" . "owl:DatatypeProperty")
     ("EquivalentTo"  . "owl:DatatypeProperty")
     ("DisjointWith"  . "owl:DatatypeProperty")
     ("Domain"        . "owl:Class")
     ("Range"         . "rdfs:Datatype"))
    ("-annotation-property-hierarchy"
     ("SubPropertyOf" . "owl:AnnotationProperty"))
    ("-individuals"
     ("Types"         . "owl:Class")
     ("SameAs"        . "owl:NamedIndividual")
     ("DifferentFrom" . "owl:NamedIndividual")))
  "Per-section map of OMN frame keyword -> expected leaf `rdf:type'.
Drives `elot/axiom-keyword-range'.  Keywords absent from a
section's inner alist are deliberately not category-checked
(e.g. `HasKey' mixes OPs and DPs; `Facts' is a prop+value pair;
annotation-property Domain/Range accept any IRI).  Built-in
datatype CURIEs from `elot-owl-builtin-resources' are accepted
wherever `rdfs:Datatype' is expected.")

(defconst elot--axiom-bare-curie-re
  "\\`[A-Za-z_][-A-Za-z0-9_.]*:[-A-Za-z0-9_./]+\\'"
  "Match a single bare CURIE (no whitespace, no operators, no parens).
Leaves not matching this are deferred to the PEG grammar.")

(defun elot--axiom-kind-pretty (kind)
  "Render `rdf:type' KIND for diagnostic messages."
  (pcase kind
    ("owl:Class"              "Class")
    ("owl:ObjectProperty"     "ObjectProperty")
    ("owl:DatatypeProperty"   "DataProperty")
    ("owl:AnnotationProperty" "AnnotationProperty")
    ("owl:NamedIndividual"    "Individual")
    ("rdfs:Datatype"          "Datatype")
    (_                        (or kind "?"))))

(defun elot--axiom-value-leaf-tokens (keyword contents)
  "Split CONTENTS into bare-CURIE leaves for KEYWORD.
Returns a list of trimmed CURIE strings that match
`elot--axiom-bare-curie-re'.  Conjuncts that aren't bare CURIEs
(class expressions, IRIs, literals) are silently dropped --
they are deferred to the PEG grammar."
  (let* ((sep (if (string= keyword "SubPropertyChain")
                  "[[:space:]]+o[[:space:]]+"
                "[,\n]"))
         (parts (split-string (or contents "") sep t "[[:space:]]+")))
    (delq nil
          (mapcar (lambda (s)
                    (let ((tok (string-trim s)))
                      (and (string-match-p elot--axiom-bare-curie-re tok)
                           tok)))
                  parts))))

(defun elot-check-axiom-keyword-range (tree)
  "ELOT rule: check the OWL category of bare-CURIE leaves in axiom values.
For each description-list row whose KEY appears in
`elot-omn-keyword-value-kind' for the enclosing section, walk the
bare-CURIE leaves of the value and compare each leaf's declared
`rdf:type' (from `elot-slurp') against the keyword's expected
leaf kind.  Emit a WARNING when the declared kind disagrees;
defer non-bare conjuncts (class expressions etc.) to the PEG
grammar."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((section-id
                      (save-excursion
                        (goto-char (org-element-property :begin item))
                        (elot-governing-section-id)))
                     (suffix (elot--section-suffix-from-id section-id))
                     (per-section (cdr (assoc suffix elot-omn-keyword-value-kind)))
                     (tag (org-element-property :tag item))
                     (term (org-element-interpret-data tag))
                     (expected (cdr (assoc term per-section))))
                (when (and suffix per-section expected)
                  (let* ((contents (org-element-interpret-data
                                    (seq-remove
                                     (lambda (child)
                                       (eq (org-element-type child) 'plain-list))
                                     (org-element-contents item))))
                         (leaves (elot--axiom-value-leaf-tokens term contents)))
                    (dolist (curie leaves)
                      (let* ((entry (cl-find curie elot-slurp
                                             :key #'car :test #'string=))
                             (declared
                              (and entry
                                   (plist-get (nth 2 entry) "rdf:type"
                                              #'equal))))
                        (cond
                         ;; Undeclared CURIEs are flagged by
                         ;; `elot/axiom-value-curies' -- skip here.
                         ((null declared) nil)
                         ;; Datatype expected: accept any built-in
                         ;; xsd:* / rdf:* datatype CURIE without a
                         ;; declared kind in the buffer.
                         ((and (string= expected "rdfs:Datatype")
                               (member curie elot-owl-builtin-resources))
                          nil)
                         ;; Kinds agree.
                         ((string= declared expected) nil)
                         ;; Mismatch.
                         (t
                          (push
                           (list (org-element-property :begin item)
                                 (propertize
                                  (format
                                   "WARNING: %s is a%s %s but %s expects a%s %s"
                                   curie
                                   (if (string-match-p
                                        "\\`[AEIOU]"
                                        (elot--axiom-kind-pretty declared))
                                       "n" "")
                                   (elot--axiom-kind-pretty declared)
                                   term
                                   (if (string-match-p
                                        "\\`[AEIOU]"
                                        (elot--axiom-kind-pretty expected))
                                       "n" "")
                                   (elot--axiom-kind-pretty expected))
                                  'face 'warning))
                           issues))))))))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/axiom-keyword-range
 "ELOT: check OWL category of bare-CURIE leaves in axiom values"
 #'elot-check-axiom-keyword-range
 :categories '(default elot)
 :trust 'high)


;;; ---- Idiomatic-heading-nesting check (Step 7.5.4) ----

;; Catches a common LLM-authoring mistake (and a not-uncommon human one):
;; expressing SubClassOf / SubPropertyOf as a description-list row pointing
;; at a NAMED parent that is also declared elsewhere in the file as a
;; heading.  The idiomatic ELOT spelling for "X is a subclass of named Y"
;; is heading nesting -- place the X heading as a child of the Y heading
;; -- not a description-list axiom that duplicates the relationship.
;;
;; Scope is intentionally narrow: only headings that are *direct children*
;; of a level-2 :resourcedefs: yes section (Classes, Object properties,
;; Data properties, Annotation properties, Datatypes -- via the generic
;; resourcedefs flag) are checked.  Sub-children of those headings (e.g.
;; **** Dog under *** Animal) are exempt: there the user has already
;; committed to heading nesting for the surrounding taxonomy, and any
;; SubClassOf rows on them are typically anonymous expressions.
;;
;; Description-list rows whose value is an anonymous class expression
;; (containing OMN keywords like `some' / `and' / `only', or parentheses,
;; or square brackets, or whitespace separating tokens) are *not* flagged
;; -- that is the legitimate use of the description-list form.

(declare-function elot-update-headline-hierarchy "elot-tangle")
(defvar elot-headline-hierarchy)

(defconst elot--subclass-in-dl-curie-re
  "\\`\\(?:<[^<>[:space:]]+>\\|[A-Za-z_][-A-Za-z0-9_.]*:[-A-Za-z0-9_./]+\\)\\'"
  "Regexp matching a single bare CURIE or full <IRI> (no whitespace).
Used to recognise the legitimate-target case: the description-list
value names exactly one resource, with no anonymous class-expression
syntax around it.")

(defun elot--collect-declared-uris (node table)
  "Recursively add every :uri in NODE's subtree to hash TABLE.
Keys are the bare CURIE/IRI strings; values are t."
  (let ((uri (plist-get node :uri)))
    (when (and uri (stringp uri))
      ;; Composite "iri version-iri" forms: take first token only.
      (puthash (car (split-string uri " " t)) t table)))
  (dolist (child (plist-get node :children))
    (elot--collect-declared-uris child table)))

(defun elot-check-subclass-in-description-list (_tree)
  "ELOT rule: warn when a heading directly under a level-2 :resourcedefs:
section carries `SubClassOf' / `SubPropertyOf' naming a class/property
also declared elsewhere in this file as a heading.
Heading nesting is the idiomatic ELOT spelling for that relationship;
the description-list form is reserved for anonymous class expressions
and where multiple inheritance is needed.
Anonymous expressions and undeclared CURIEs are not flagged.
This checker consumes `elot-headline-hierarchy' (refreshed via
`elot-update-headline-hierarchy') rather than re-walking the Org tree."
  (when (fboundp 'elot-update-headline-hierarchy)
    (ignore-errors (elot-update-headline-hierarchy)))
  (let ((issues '())
        (declared (make-hash-table :test 'equal)))
    (when (and (boundp 'elot-headline-hierarchy) elot-headline-hierarchy)
      (elot--collect-declared-uris elot-headline-hierarchy declared)
      (cl-labels
          ((check-resource (resource)
             (let ((marker (plist-get resource :marker)))
               (dolist (desc (plist-get resource :descriptions))
                 (let ((key (car desc))
                       (val (cadr desc)))
                   (when (and (member key '("SubClassOf" "SubPropertyOf"))
                              (stringp val)
                              (string-match-p elot--subclass-in-dl-curie-re val)
                              (gethash val declared)
                              marker)
                     (push
                      (list (marker-position marker)
                            (propertize
                             (format
                              "WARNING: %s :: %s names a heading declared in this file; prefer heading nesting (place this heading as a child of the %s heading) over the description-list form."
                              key val val)
                             'face 'warning))
                      issues))))))
           (walk (node)
             (if (and (= (or (plist-get node :level) -1) 2)
                      (equal (plist-get node :resourcedefs) "yes"))
                 ;; Level-2 :resourcedefs: section: check its *direct*
                 ;; children only; grandchildren are exempt.
                 (dolist (resource (plist-get node :children))
                   (check-resource resource))
               (dolist (child (plist-get node :children))
                 (walk child)))))
        (walk elot-headline-hierarchy)))
    issues))

(org-lint-add-checker
 'elot/subclass-in-description-list
 "ELOT: prefer heading nesting over `SubClassOf :: NamedClass' description-list rows"
 #'elot-check-subclass-in-description-list
 :categories '(default elot)
 :trust 'high)


;;; ---- Blank OMN-axiom description-list rows (Step 9.3.F4) ----

;; Companion to the 9.3.F2 silent skip on the tangle path: flag any
;; description-list row whose KEY is in `elot-omn-all-keywords' (the
;; OWL 2 Manchester Syntax frame + miscellaneous axiom keywords --
;; Domain, Range, SubClassOf, SubPropertyOf, Types, Facts,
;; Characteristics, InverseOf, SubPropertyChain, DisjointClasses,
;; EquivalentClasses, ...) and whose VALUE is empty after string-trim.
;;
;; F2 prevents such rows from producing malformed OMN, but they are
;; almost always an authoring mistake (M10.6 description-list
;; inheritance leftovers, interrupted edits, or template copy-paste
;; that the author meant to fill in).  Severity `warning' rather than
;; `error' because the OMN output is now well-formed; the lint surface
;; is where the author gets a contextual nudge to either populate the
;; row or delete it.  Annotation rows with blank values (e.g.
;; `rdfs:comment ::' or `skos:example ::') are deliberately NOT
;; flagged -- they tangle to benign empty annotations and there are
;; legitimate authoring reasons to leave them as placeholders.

(defvar elot-omn-all-keywords)

(defun elot-check-blank-omn-axiom-row (tree)
  "ELOT rule: warn on blank OMN-axiom description-list rows.
Flags any `- KEY :: VALUE' description-list row inside an ELOT
ontology scope where KEY is a member of `elot-omn-all-keywords' and
VALUE (after `string-trim') is empty.  Companion to the 9.3.F2
silent-skip on the tangle path: the OMN is now well-formed, but the
row is almost certainly an authoring mistake worth surfacing.
Annotation rows (e.g. `rdfs:comment ::') are not in scope."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
            (goto-char (org-element-property :begin item))
            (when (elot--inside-elot-scope-p)
              (let* ((tag (org-element-property :tag item))
                     (term (and tag (org-element-interpret-data tag))))
                (when (and (stringp term)
                           (member term elot-omn-all-keywords))
                  (let* ((contents-raw
                          (org-element-interpret-data
                           (seq-remove (lambda (child)
                                         (eq (org-element-type child)
                                             'plain-list))
                                       (org-element-contents item))))
                         (contents (if contents-raw
                                       (string-trim contents-raw)
                                     "")))
                    (when (string-empty-p contents)
                      (push (list (org-element-property :begin item)
                                  (propertize
                                   (format
                                    "WARNING: Blank OMN axiom row `%s ::'; either populate the value or remove the row."
                                    term)
                                   'face 'warning))
                            issues)))))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/blank-omn-axiom-row
 "ELOT: warn about blank OMN-axiom description-list rows"
 #'elot-check-blank-omn-axiom-row
 :categories '(default elot)
 :trust 'high)


;;; ---- OOPS!-derived checks (P08, P24, P32) ----
;;
;; A small, deliberately-cheap sliver of the OOPS! pitfall catalogue
;; (https://oops.linkeddata.es/catalogue.jsp), reimplemented as pure
;; Elisp org-lint checkers so they run inline at authoring time
;; alongside the rest of `elot-org-lint'.  The full SPARQL-backed
;; OOPS! suite (planned under Milestone 8) remains the source of
;; truth for the catalogue at large; the three checkers below are
;; the subset that maps cleanly to patterns already visible in
;; `elot-headline-hierarchy', so no new index machinery is needed.
;; Each message ends with an OOPS! citation suffix so failures
;; self-document their authority.
;;
;; Pitfalls covered:
;;   - P08 Missing annotations
;;        https://oops.linkeddata.es/catalogue.jsp#P08
;;   - P24 Using recursive definition
;;        https://oops.linkeddata.es/catalogue.jsp#P24
;;   - P32 Several classes with the same label
;;        https://oops.linkeddata.es/catalogue.jsp#P32

(defconst elot--oops-p08-annotation-keys
  '("rdfs:label" "rdfs:comment"
    "skos:definition" "skos:prefLabel" "skos:altLabel"
    "iof-av:naturalLanguageDefinition"
    "dcterms:description")
  "Description-list keys that count as authored documentation for P08.
A resource heading whose :descriptions contain none of these is
considered undocumented, regardless of its title-derived label
(which is auto-generated and so does not satisfy the pitfall).")

(defconst elot--oops-p24-axiom-keys
  '("SubClassOf" "EquivalentTo" "DisjointWith" "DisjointUnionOf"
    "SubPropertyOf")
  "Description-list keys whose values are scanned for recursive
self-reference (OOPS! P24).")

(defun elot--oops-walk-resources (node fn)
  "Call FN on every resource node in NODE's subtree.
A resource node is one with a :uri.  Skips subtrees whose root
has :nodeclare in its tags.  FN receives the node plist."
  (let ((tags (plist-get node :tags)))
    (unless (member "nodeclare" tags)
      (when (plist-get node :uri)
        (funcall fn node))
      (dolist (child (plist-get node :children))
        (elot--oops-walk-resources child fn)))))

(defun elot--oops-curie-mentioned-p (curie value)
  "Return non-nil if string VALUE mentions CURIE on a word boundary.
Used by the P24 check so e.g. `ex:dog' is not matched inside
`ex:dogfood'."
  (and (stringp value)
       (stringp curie)
       (let ((case-fold-search nil))
         (string-match-p
          (concat "\\(?:^\\|[^-_./[:alnum:]]\\)"
                  (regexp-quote curie)
                  "\\(?:[^-_./[:alnum:]]\\|$\\)")
          value))))

(defun elot--oops-parse-label-literal (value)
  "Split an `rdfs:label' description-list VALUE into (TEXT . LANG).
Returns nil if VALUE is not a recognisable literal form.
- `\"text\"@lang' -> (\"text\" . \"lang\")
- `\"text\"'      -> (\"text\" . nil)
- bare text       -> (text  . nil)
Used by the P32 check so that `\"House\"@en' and `\"House\"@de'
are not flagged as colliding labels."
  (when (stringp value)
    (let ((v (string-trim value)))
      (cond
       ((string-match "\\`\"\\([^\"]*\\)\"@\\([[:alpha:]][-[:alnum:]]*\\)\\'" v)
        (cons (match-string 1 v) (match-string 2 v)))
       ((string-match "\\`\"\\([^\"]*\\)\"\\'" v)
        (cons (match-string 1 v) nil))
       ((not (string-empty-p v))
        (cons v nil))))))

(defconst elot--oops-resource-curie-re
  "(\\([A-Za-z_][-A-Za-z0-9_.]*:[^) \t]+\\))"
  "Regexp matching a CURIE inside parentheses in a resource heading title.
Group 1 captures the CURIE itself.")

(defun elot--oops-nodeclare-ranges (tree)
  "Return ((BEGIN . END) ...) for every `:nodeclare:'-tagged headline in TREE."
  (let (ranges)
    (org-element-map tree 'headline
      (lambda (hl)
        (when (member "nodeclare" (org-element-property :tags hl))
          (push (cons (org-element-property :begin hl)
                      (org-element-property :end hl))
                ranges))))
    ranges))

(defun elot--oops-in-ranges-p (pos ranges)
  "Non-nil if POS is inside any (BEGIN . END) interval in RANGES."
  (cl-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r))))
           ranges))

(defun elot--oops-headline-item-tags (hl)
  "Return the list of authored description-list KEY strings directly under HL.
Does not descend into child headlines or nested items.  This is the
*authored* view: it never includes the `rdf:type' and `rdfs:label'
rows that `elot-parse-headline-hierarchy' auto-derives from the
heading title."
  (delq nil
        (org-element-map (org-element-contents hl) 'item
          (lambda (item)
            (let ((tag (org-element-property :tag item)))
              (when tag
                (substring-no-properties
                 (string-trim (org-element-interpret-data tag))))))
          nil nil '(headline item))))

(defun elot-check-oops-missing-annotations (tree)
  "ELOT/OOPS! P08: warn on resource headings with no authored documentation.
A resource heading (one whose title carries a CURIE in parentheses)
is flagged when none of its *authored* description-list keys is in
`elot--oops-p08-annotation-keys'.  Headings under a `:nodeclare:'
ancestor are skipped.

The authored view is computed by re-walking the headline's contents
directly, so the `rdfs:label' that
`elot-parse-headline-hierarchy' auto-derives from the heading title
is correctly ignored (the title-derived label does not satisfy the
pitfall).

Citation: OOPS! P08 -- Missing annotations
https://oops.linkeddata.es/catalogue.jsp#P08."
  (let ((issues nil)
        (nodeclare-ranges (elot--oops-nodeclare-ranges tree)))
    (org-element-map tree 'headline
      (lambda (hl)
        (let* ((begin (org-element-property :begin hl))
               (title-raw (org-element-property :title hl))
               (title (substring-no-properties
                       (if (stringp title-raw)
                           title-raw
                         (org-element-interpret-data title-raw)))))
          (when (and (stringp title)
                     (string-match elot--oops-resource-curie-re title)
                     (not (member "nodeclare"
                                  (org-element-property :tags hl)))
                     (not (elot--oops-in-ranges-p begin nodeclare-ranges)))
            (let ((curie (match-string 1 title))
                  (keys  (elot--oops-headline-item-tags hl)))
              (unless (cl-some (lambda (k)
                                 (member k elot--oops-p08-annotation-keys))
                               keys)
                (push (list begin
                            (propertize
                             (format
                              "WARNING: %s has no authored annotation (rdfs:label / rdfs:comment / skos:definition / ...); the heading-title label is auto-derived and does not count.  [OOPS! P08, see https://oops.linkeddata.es/catalogue.jsp#P08]"
                              curie)
                             'face 'warning))
                      issues)))))))
    issues))

(org-lint-add-checker
 'elot/oops-missing-annotations
 "ELOT/OOPS! P08: resource heading lacks any authored annotation"
 #'elot-check-oops-missing-annotations
 :categories '(default elot)
 :trust 'high)

(defun elot-check-oops-recursive-definition (_tree)
  "ELOT/OOPS! P24: warn when an axiom value mentions its own subject CURIE.
For each resource heading, scan its description-list rows whose
key is in `elot--oops-p24-axiom-keys' and emit a warning if the
value contains the heading's own CURIE on a word boundary.
Citation: OOPS! P24 -- Using recursive definition
https://oops.linkeddata.es/catalogue.jsp#P24."
  (when (fboundp 'elot-update-headline-hierarchy)
    (ignore-errors (elot-update-headline-hierarchy)))
  (let (issues)
    (when (and (boundp 'elot-headline-hierarchy) elot-headline-hierarchy)
      (elot--oops-walk-resources
       elot-headline-hierarchy
       (lambda (node)
         (let* ((uri (car (split-string (or (plist-get node :uri) "") " " t)))
                (marker (plist-get node :marker)))
           (when (and uri marker (string-match-p ":" uri))
             (dolist (desc (plist-get node :descriptions))
               (let ((key (car desc))
                     (val (cadr desc)))
                 (when (and (member key elot--oops-p24-axiom-keys)
                            (elot--oops-curie-mentioned-p uri val))
                   (push (list (marker-position marker)
                               (propertize
                                (format
                                 "WARNING: %s :: %s references its own subject %s (recursive definition).  [OOPS! P24, see https://oops.linkeddata.es/catalogue.jsp#P24]"
                                 key val uri)
                                'face 'warning))
                         issues)))))))))
    issues))

(org-lint-add-checker
 'elot/oops-recursive-definition
 "ELOT/OOPS! P24: axiom value mentions the subject's own CURIE"
 #'elot-check-oops-recursive-definition
 :categories '(default elot)
 :trust 'high)

(defun elot-check-oops-duplicate-labels (_tree)
  "ELOT/OOPS! P32: warn when two resources carry the same `rdfs:label'.
Builds an index of (text . lang) -> list of CURIEs from
description-list `rdfs:label' rows; collisions only count within
the same language tag, so `\"House\"@en' and `\"House\"@de' do
not collide.  Wrapper headings and :nodeclare: subtrees are
skipped.
Citation: OOPS! P32 -- Several classes with the same label
https://oops.linkeddata.es/catalogue.jsp#P32."
  (when (fboundp 'elot-update-headline-hierarchy)
    (ignore-errors (elot-update-headline-hierarchy)))
  (let ((index (make-hash-table :test 'equal))
        issues)
    (when (and (boundp 'elot-headline-hierarchy) elot-headline-hierarchy)
      (elot--oops-walk-resources
       elot-headline-hierarchy
       (lambda (node)
         (unless (equal (plist-get node :resourcedefs) "yes")
           (let* ((uri (car (split-string (or (plist-get node :uri) "") " " t)))
                  (marker (plist-get node :marker)))
             (when (and uri marker)
               (dolist (desc (plist-get node :descriptions))
                 (when (equal (car desc) "rdfs:label")
                   (let ((parsed (elot--oops-parse-label-literal (cadr desc))))
                     (when parsed
                       (let* ((key (cons (car parsed) (cdr parsed)))
                              (cell (gethash key index)))
                         (puthash key
                                  (cons (cons uri (marker-position marker))
                                        cell)
                                  index)))))))))))
      (maphash
       (lambda (key entries)
         (when (> (length entries) 1)
           (let* ((text (car key))
                  (lang (cdr key))
                  (curies (mapcar #'car entries))
                  (label-display (if lang
                                     (format "\"%s\"@%s" text lang)
                                   (format "\"%s\"" text))))
             (dolist (e entries)
               (push (list (cdr e)
                           (propertize
                            (format
                             "WARNING: rdfs:label %s is shared by %d resources (%s); each label should identify exactly one resource.  [OOPS! P32, see https://oops.linkeddata.es/catalogue.jsp#P32]"
                             label-display (length entries)
                             (string-join curies ", "))
                            'face 'warning))
                     issues)))))
       index))
    issues))

(org-lint-add-checker
 'elot/oops-duplicate-labels
 "ELOT/OOPS! P32: two or more resources share the same rdfs:label"
 #'elot-check-oops-duplicate-labels
 :categories '(default elot)
 :trust 'high)


(provide 'elot-lint)
;;; elot-lint.el ends here
