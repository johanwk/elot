;;; elot-lint.el --- ELOT lint checks for ontology buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Kluewer

;; Author: Johan W. Kluewer <johan.w.kluwer@gmail.com>
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

(defun elot--resourcedefs-here-p ()
  "Return t if headline at point sets :resourcedefs: yes."
  (string-equal (org-entry-get nil "resourcedefs") "yes"))

(defun elot--inside-resourcedefs-p ()
  "Return t when point is inside any :resourcedefs: yes section."
  (string-equal (org-entry-get-with-inheritance "resourcedefs") "yes"))

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
        (when (and (elot--inside-resourcedefs-p)
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
                 (prefix (elot-default-prefix))
                 (omn-args (org-entry-get nil "header-args:omn")))
            (when (or (null id) (string= id ""))
              (push (list (point)
                          (propertize "ERROR: Top-level heading missing :ID:" 'face 'error))
                    issues))
            (when (and id localname (not (string= id localname)))
              (push (list (point)
                          (propertize "WARNING: :ELOT-context-localname: should match :ID:" 'face 'warning))
                    issues))
            (when (null (assoc prefix org-link-abbrev-alist-local))
              (push (list (point)
                          (propertize "WARNING: :ELOT-default-prefix: is not defined in org-link-abbrev-alist-local"
                                      'face 'warning))
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


(defconst elot-known-annotation-properties
  '("rdfs:label" "rdfs:comment" "rdfs:seeAlso" "rdfs:isDefinedBy")
  "List of annotation properties allowed in description lists without declaration.")

(defun elot-check-description-list-curies (tree)
  "Check CURIE terms in description lists are declared annotation properties."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
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
                        issues)))))))
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
               (type (org-element-property :type parent))
               (hierarchy-id
                (save-excursion
                  (goto-char (org-element-property :begin item))
                  (elot-governing-hierarchy)))
               (in-annotation-section
                (and hierarchy-id
                     (string-match-p "annotation-property-hierarchy$" hierarchy-id))))
          (when (eq type 'descriptive)
            (let* ((tag (org-element-property :tag item))
                   (term (org-element-interpret-data tag))
                   ;; exclude sublists from contents
                   (contents (org-element-interpret-data
                              (seq-remove (lambda (child)
                                            (eq (org-element-type child) 'plain-list))
                                          (org-element-contents item)))))
              ;; Only apply check if term is a Manchester keyword
              (when (member term elot-omn-all-keywords)
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
                        ;; Built-in or known: only warn if it’s an annotation property
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
                                    (propertize (format "WARNING: Unknown CURIE in axiom: %s" curie)
                                                'face 'warning))
                              issues))))))
                ;; Check balanced parentheses
                (unless (elot-string-balanced-parentheses-p contents)
                  (push (list (org-element-property :begin item)
                              (propertize "WARNING: Unbalanced parentheses in axiom value"
                                          'face 'warning))
                        issues)))))))
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
    ("Domain"         . elot-parse-class-expression-list)
    ("Range"          . elot-parse-class-expression-list)
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
a failure position, the error message indicates the exact column."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
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
                          issues))))))))
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
etc.) according to the OWL 2 Manchester Syntax specification."
  (let (issues)
    (org-element-map tree 'item
      (lambda (item)
        (let* ((parent (org-element-property :parent item))
               (type (org-element-property :type parent)))
          (when (eq type 'descriptive)
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
                            issues)))))))))
      tree)
    issues))

(org-lint-add-checker
 'elot/omn-keyword-appropriateness
 "ELOT: check OMN keywords are appropriate for their section type"
 #'elot-check-omn-keyword-appropriateness
 :categories '(default elot)
 :trust 'high)


(provide 'elot-lint)
;;; elot-lint.el ends here
