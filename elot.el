;;; elot.el --- Emacs Literate Ontology Tool (ELOT)   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.2") ... and several more ...)
;; Keywords: org, ontology

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is for authoring OWL ontologies using org-mode.

;;;; Installation

;; Install the packages ... .  Then put this file in your `load-path', and put this in
;; your init file:

(require 'ob-lob) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions
(require 'htmlize) ; fontify blocks
(require 'omn-mode) ; OMN support

;;;; Usage

;; ... create a new file, use <template inserting function> to insert a template ontology ...

;; [[file:elot-lob.org::defun-desc-lists][defun-desc-lists]]
(defun org-elt-exists (x elt)
  (org-element-map x elt #'identity))
(defun org-elt-item-tag-str (x)
  "for an item in an org-element-map, return the item tag"
  (if (org-element-property :tag x)
      (substring-no-properties (org-element-interpret-data (org-element-property :tag x)))))
(defun org-elt-item-pars-str (x)
  "for an item in an org-element map, return the paragraphs as one string"
  (replace-regexp-in-string "\\([^
]\\)\n[ \t]*" "\\1 "
 (string-trim (apply 'concat
                (org-element-map x '(paragraph plain-list)
                  (lambda (y) (substring-no-properties 
                               (org-element-interpret-data y)))
                  nil nil 'plain-list)))))
(defun org-elt-item-str (x)
  (list (org-elt-item-tag-str x) (org-elt-item-pars-str x)))
(defun org-descriptions-in-section-helper ()
  (org-element-map (org-element-parse-buffer) 'item
    (lambda (y) (if (org-element-property :tag y)
                    (append (org-elt-item-str y)
                            (if (org-elt-exists (cdr y) 'item)
                                (org-element-map (cdr y) 'item
                                  (lambda (z) (if (org-element-property :tag z)
                                                  (org-elt-item-str z))) nil nil 'item))
                            ))) nil nil 'item))

(defun org-descriptions-in-section ()
  "return any description list items in current section as a list of strings"
  (interactive)
                                        ; narrow our area of interest to the current section, before any subsection
  (let ((section-begin) (section-end))
    (save-restriction 
      (save-excursion
        (unless (org-at-heading-p) (org-previous-visible-heading 1))
        (setq section-begin (org-element-property :contents-begin (org-element-at-point)))
        (outline-next-heading)
        (setq section-end (point))
        (if (or (null section-begin) (<= section-end section-begin))
            nil ; maybe this outline section is empty
          (progn
            (narrow-to-region section-begin section-end)
                                        ; return all paragraphs--description items as pairs in a list
            (org-descriptions-in-section-helper)))))))

(defun org-subsection-descriptions ()
  "return a plist for the outline at point, of headlines paired with plists of description-list items and values."
  (save-restriction
    (save-excursion
      (unless (org-at-heading-p) (org-previous-visible-heading 1)) ; ensure we are at a heading
      (org-narrow-to-subtree)
      (outline-next-heading)  ; don't include the section that has the target property id itself
      (let (ret)
        (while (let ((heading (substring-no-properties (org-get-heading nil t)))
                     (descriptions (org-descriptions-in-section)))
                 (unless (or (string-match-p "COMMENT" heading)
                             (member "nodeclare" (org-get-tags (point) t)))
                   (setq ret
                         (cons
                          (if descriptions
                              (list heading descriptions)
                            (list heading))
                          ret)))
                 (outline-next-heading)))
        (nreverse ret)))))
;; defun-desc-lists ends here

;; [[file:elot-lob.org::defun-puri][defun-puri]]
(defconst puri-re "^\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9-.]+\\)$")

(defun unprefix-uri (puri abbrev-alist)
 "Replace prefix in puri with full form from abbrev-alist, if there's a match."
 (if (eq abbrev-alist nil) puri
   (if (string-match puri-re puri)
       (let* ((this-prefix (match-string-no-properties 1 puri))
              (this-localname (match-string-no-properties 2 puri))
              (this-ns (cdr (assoc this-prefix abbrev-alist))))
         (if this-ns
             (concat "<" this-ns this-localname ">")
           puri))
     puri)))

(defun annotation-string-or-uri (str)
  "str is wanted as an annotation value in Manchester Syntax. Expand uri, or return number, or wrap in quotes."
  ; maybe this entry contains string representation of meta-annotations, remove them
  (setq str (replace-regexp-in-string " - [^ ]+ ::.*$" "" str))
  ;; maybe there's macros in the string, expand them
  (if (string-match "{{{.+}}}" str)
    (let ((omt org-macro-templates))
      (with-temp-buffer 
        (insert str) (org-macro-replace-all omt) 
        (setq str (buffer-string)))))
   (cond (; a number -- return the string
          (string-match "^[[:digit:]]+[.]?[[:digit:]]*$" str)
          (concat "  " str))
         (; a bare URI, which org-mode wraps in double brackets -- wrap in angles
          (string-match "^[[][[]\\(https?[^ ]*\\)[]][]]$" str)
          (concat "  <" (match-string 1 str) ">"))
         (; a bare URI, but no double brackets -- wrap in angles
          (string-match "^\\(https?[^ ]*\\)$" str)
          (concat "  <" (match-string 1 str) ">"))
         (; a bare URI, in angles
          (string-match "^<\\(https?[^ ]*\\)>$" str)
          (concat "  " (match-string 1 str)))
        (; true -- make it an explicit boolean
          (string-match "true" str) " \"true\"^^xsd:boolean")
        (; false -- make it an explicit boolean
          (string-match "false" str) " \"false\"^^xsd:boolean")
        (; string with datatype -- return unchanged
          (string-match "^\".*\"^^[-_[:alnum:]]*:[-_[:alnum:]]+$" str)
          (concat "  " str))
        (; not a puri -- normal string, wrap in quotes
         (equal str (unprefix-uri str org-link-abbrev-alist-local))
         ; escape all quotes with \", note this gives invalid results if some are already escaped
         (concat "  \"" (replace-regexp-in-string "\"" "\\\\\"" str) "\""))
        (; else, a puri -- wrap in angles
         t (concat "  " (unprefix-uri str org-link-abbrev-alist-local)))))

(defun omn-restriction-string (str)
  "str is wanted as OMN value. Strip any meta-annotations. Otherwise return unchanged."
  (setq str (replace-regexp-in-string " - [^ ]+ ::.*$" "" str))
  str)
;; defun-puri ends here

;; [[file:elot-lob.org::defun-resource-headings][defun-resource-headings]]
; http://stackoverflow.com/questions/17179911/emacs-org-mode-tree-to-list
(defun org-list-siblings ()
  "List siblings in current buffer starting at point.
  Note, you can always (goto-char (point-min)) to collect all siblings."
  (interactive)
  (let (ret)
    (unless (org-at-heading-p) 
      (org-forward-heading-same-level nil t))
    (while (progn
             (unless (looking-at "[*]* *COMMENT")
               (setq ret
                     (if (member "nodeclare" (org-get-tags (point) t)) ; tagged to be skipped, proceed down
                         (cons (save-excursion
                                         (when (org-goto-first-child)
                                           (org-list-siblings))) ret)
                       (cons (append (list
                                        ; the nil t arguments for tags yes, todos no, todos no, priorities no
                                        (substring-no-properties (org-get-heading nil t t t)))
                                       (save-excursion
                                         (when (org-goto-first-child)
                                           (org-list-siblings))))
                               ret))))
             (org-goto-sibling)))
    (nreverse ret)))

(defun entity-from-header (str)
  "Get an entity from a header string.
The headers can be of two kinds. With prefix 'abc',
 - abc:MyClassName
 - my class name (abc:MyClassName)

Maybe also with tags :hello: on the right. Return abc:MyClassName in both cases."
  (if (string-match "(\\([-_[:alnum:]]*:[-_[:alnum:]]+\\))" str) ; the resource id is in parentheses
      (match-string 1 str)
    (if (string-match "^\\([-_[:alnum:]]*:[-_[:alnum:]]+\\)" str) ; return string up to whitespace
        (match-string 1 str)
      (if (string-match "(\\([-_[:alnum:]]*:[-_[:alnum:]]+ [-_[:alnum:]]*:[-_/.[:alnum:]]+\\))" str) ; two ids in parentheses, for ontology
          (match-string 1 str)
        (concat "Malformed_" str)))))
;; defun-resource-headings ends here

;; [[file:elot-lob.org::defun-resource-declaration][defun-resource-declaration]]
(defun omn-declare (str owl-type)
   "Given a string STR and an OWL type owl-type, write a Manchester Syntax entity declaration. Add rdfs:label annotation. If a parenthesis is given, use that as resource id."
  ; check whether we have a label and a resource in parentheses
   (let* ((suri (entity-from-header str))
          ;; (prefix (if (string-match "\\(.*\\):\\(.*\\)" suri)
          ;;             (match-string 1 suri) ""))
          ;; (localname (if (string= prefix "") suri (match-string 2 suri)))
          ;; (label (if (string-match "\\(.+\\) (.*)" str)
          ;;            (match-string 1 str) localname))
          )
     (concat owl-type ": " suri)))
     ;;               "\n    Annotations: rdfs:label \"" label "\"")))

(defun annotation-entries (l &optional sep)
  "l is a list of puri--string pairs, each perhaps with a trailing list of similar, meta-annotation pairs. sep is 2 x indent blanks"
  (let ((indent (make-string (if sep (* 2 sep) 6) ?\ ))
        ;; l-uri-entries is the description list after purging any
        ;; items that have a prefix that isn't included as a LINK
        ;; entry, which goes into org-link-abbrev-alist-local. Note
        ;; that expanded URIs in brackets <...> are let through.
        (l-uri-entries
         (cl-remove-if (lambda (x) (string-equal (car x)
                                                 (unprefix-uri (car x) org-link-abbrev-alist-local)))
            l)))
  (if (atom l) "\n"
    (concat "\n" indent "Annotations: " 
            (mapconcat (lambda (y)
                         (concat
                          (if (consp (caddr y)) ; we have meta-annotations
                              (concat (annotation-entries (cddr y) 4) "\n " indent))
                          (car y)
                          (annotation-string-or-uri (cadr y))))
                       l-uri-entries
                       (concat ",\n " indent))))))

(defun restriction-entries (l)
  "l is a list of puri--string pairs, except we'll pick up Manchester Syntax vocabulary and use as such"
  (let ((indent (make-string 2 ?\ ))
        (l-omn-entries
         (cl-remove-if-not (lambda (x) (member (car x)
                             '("Characteristics" "Domain" "Range" "SubClassOf" "SubPropertyOf" "EquivalentTo" "DisjointWith" "InverseOf" "SubPropertyChain" "Import")))
                           l)))
    (if (atom l) "\n"
      (concat "\n" indent
            (mapconcat (lambda (y)
                         (concat
                          (car y) ": "
                          (if (consp (caddr y)) ; we have meta-annotations
                              (concat (annotation-entries (cddr y) 4) "\n " indent))
                          (if (string-equal (car y) "Import") ; ontology import special case
                              (annotation-string-or-uri (cadr y))
                          (omn-restriction-string (cadr y)))
                          ))
                       l-omn-entries
                       (concat "\n" indent))))))

(defun omn-annotate (l)
  (let* ((str (car l))
         (suri (entity-from-header str))
         (prefix (if (string-match "\\(.*\\):\\(.*\\)" suri)
                     (match-string 1 suri) ""))
         (localname (if (string= prefix "") suri (match-string 2 suri)))
         (label (if (string-match "\\(.+\\) (.*)" str)
                    (match-string 1 str) localname))
         (resource-annotations
          (cons (list "rdfs:label" label) (cadr l))))
    (annotation-entries resource-annotations)))

(defun omn-restrict (l)
  (restriction-entries (cadr l)))

(defun resource-declarations (l owl-type)
  "Take a possibly list of identifiers with annotations, declare to be of owl-type."
  (mapconcat
   (lambda (x) 
     (concat
      (omn-declare (car x) owl-type)
      ; if annotations, add to the annotation block that has been started with rdfs:label
      (omn-annotate x)
      (omn-restrict x)
      ))
   l "\n"))

(defun resource-declarations-from-header (header-id owl-type)
  "HEADER-ID is an org location id, OWL-TYPE is Class, etc."
  (save-excursion
    (org-id-goto header-id)
    (let ((entity-l (org-subsection-descriptions)))
      (resource-declarations entity-l owl-type))))
;;(cdr (org-subsection-descriptions))))
;; defun-resource-declaration ends here

;; [[file:elot-lob.org::*Default local variables][Default local variables:2]]
(defun update-link-abbrev ()
  (setq-local org-link-abbrev-alist-local 
	      (mapcar (lambda (x) 
			(cons (replace-regexp-in-string ":" "" (car x)) (cadr x)))
		      (cl-remove 'hline (org-babel-ref-resolve "prefix-table")))
	      ))
;; Default local variables:2 ends here

;; [[file:elot-lob.org::defun-class-patterns][defun-class-patterns]]
(defun class-oneof-from-header (l)
  "L a list of class resources like ((super (((sub) (sub) ... (sub)))))."
  (let ((owl-type "Class") (owl-subclause "SubClassOf"))
    (concat "\n" owl-type ": " (entity-from-header (car l))
            "\n    " owl-subclause ": "
            (mapconcat (lambda (x)
                         (entity-from-header (car x)))
                       (cdr l) " or "))))

(defun class-disjoint-from-header (l)
  "L a list of class resources like ((super (((sub) (sub) ... (sub)))))."
    (concat "\nDisjointClasses: "
            "\n    "
            (mapconcat (lambda (x)
                         (entity-from-header (car x)))
                       (cdr l) ", ")))
;; defun-class-patterns ends here

;; [[file:elot-lob.org::defun-resource-taxonomy][defun-resource-taxonomy]]
(defun org-tags-in-string (str)
  "Return list of any tags in org-mode :asdf:lksjdf: from STR"
  (if (string-match ".*\\W+:\\(.*\\):" str)
      (split-string (match-string 1 str) ":")))

(defun resource-taxonomy-from-l (l owl-type owl-subclause)
  (if (listp (car l))
      (mapconcat (lambda (x) (resource-taxonomy-from-l x owl-type owl-subclause)) l "")
    (if (and (stringp (car l)) (stringp (caadr l)))
        (concat 
          ;simple subclass clauses
          (mapconcat (lambda (x)
                      (concat "\n" owl-type ": "
                              (entity-from-header (car x))
                              "\n    " owl-subclause ": "
                              (entity-from-header (car l))))
                    (cdr l) "")
          ;one-of pattern
          (if (member "oneof" (org-tags-in-string (car l))) (class-oneof-from-header l))
          ;disjoint pattern
          (if (member "disjoint" (org-tags-in-string (car l))) (class-disjoint-from-header l))
          (resource-taxonomy-from-l (cdr l) owl-type owl-subclause)))))

(defun resource-taxonomy-from-header (header-id owl-type owl-relation)
  "HEADER-ID is an org location id, OWL-TYPE is Class, etc., OWL-RELATION is SubClassOf, etc."
  (save-excursion
    (org-id-goto header-id)
    (org-goto-first-child)
    (let ((hierarchy-l (org-list-siblings)))
      (resource-taxonomy-from-l hierarchy-l owl-type owl-relation))))
;; defun-resource-taxonomy ends here

;; [[file:elot-lob.org::defun-latex-export][defun-latex-export]]
;; (defun ontology-resource-section (level numbered-p)
;;   (if numbered-p
;;       (if (= 1 level) "\\section{%s}" 
;;          (if (= 2 level) "\\subsection{%s}" "\\subsubsection{%s}"))
;;    "\\subsubsection*{%s}"))
(defun ontology-resource-section (level numbered-p)
  (if numbered-p
      (if (= 1 level) "\\chapter{%s}" 
        (if (= 2 level) "\\section{%s}"
          (if (= 3 level) "\\subsection{%s}" 
            "\\subsubsection{%s}")))
          ;;     (if (= 5 level) "\\paragraph{%s}"
          ;;          "\\subparagraph{%s}")))))
    (if (= 1 level) "\\addchap{%s}"  ; Koma-script command, see https://tex.stackexchange.com/questions/193767/how-to-use-unnumbered-chapters-with-koma-script/193799#193799
      (if (= 2 level) "\\addsec{%s}"
        (if (= 3 level) "\\subsection*{%s}"
           "\\subsubsection*{%s}")))
        ;;     (if (= 5 level) "\\paragraph*{%s}"
        ;;       "\\subparagraph{%s}")))))))
    ))

(defun latex-filter-headline-dots (text backend info)
  "Ensure dots in headlines."
  (when (org-export-derived-backend-p backend 'latex)
    (let* ((prop-point (next-property-change 0 text))
           (this-element (plist-get (text-properties-at prop-point text) :parent))
           (this-element-level (org-element-property :level this-element))
           (resourcedef-p (org-export-get-node-property :RESOURCEDEFS this-element t)))
      (when (and resourcedef-p (> this-element-level 2))
        (string-match "section\\(.?\\){" text)
        (replace-match (concat "section\\1{\\\\itshape{}" 
         (apply 'concat (make-list (- this-element-level 3) ".\\\\space{}")))
                       nil nil text)
        ))))
;; defun-latex-export ends here

;; [[file:elot-lob.org::defun-get-heading-nocookie][defun-get-heading-nocookie]]
(defun org-get-heading-nocookie (&optional no-tags no-todo no-priority no-comment)
  (replace-regexp-in-string " \\[[[:digit:]/%]+\\]$" ""
                            (org-get-heading no-tags no-todo no-priority no-comment)))
;; defun-get-heading-nocookie ends here

;; [[file:elot-lob.org::defun-get-description-entry][defun-get-description-entry]]
(defun org-get-description-entry (tag)
  (save-excursion
    (if (search-forward-regexp tag nil t)
        (let* ((element (org-element-at-point))
               (beg (org-element-property :contents-begin element))
               (end (org-element-property :contents-end element))
               (entry-text (buffer-substring-no-properties beg end)))
           (replace-regexp-in-string "\n\s*" " " entry-text)))))
;; defun-get-description-entry ends here

;; [[file:elot-lob.org::defun-ELOT-latex-derived-backend][defun-ELOT-latex-derived-backend]]
;; see https://emacs.stackexchange.com/questions/55231/org-mode-export-html-add-name-attirbute-to-checkbox-input
  (org-export-define-derived-backend 'ELOT-latex 'latex
    :translate-alist '((item . my-item-translator)))
  (defvar item-process nil)

  (defun my-item-translator (item c info)
    (let* ((item-tag-maybe (car (org-element-property :tag item)))
           (item-tag-stringp (stringp item-tag-maybe))
           (item-tag (if item-tag-stringp (substring-no-properties item-tag-maybe) item-tag-maybe)))
      (if (and item-tag-stringp (string= item-tag "item-translate-start")) (setq item-process t))
      (if (and item-tag-stringp (string= item-tag "item-translate-stop")) (setq item-process nil))
    (when (and item-process item-tag-stringp)
      (progn
        ;(message (substring-no-properties item-tag))
        (setf (plist-get (cadr item) :checkbox) nil)  ; set checkbox here
        (let ((tag-mapped (assoc item-tag (quote
(("iof-av:isPrimitive" . "primitive?")
 ("iof-av:naturalLanguageDefinition" . "definition")
 ("iof-av:primitiveRationale" . "why primitive")
 ("iof-av:usageNote" . "usage note")
 ("owl:deprecated" . "deprecated?")
 ("rdfs:seeAlso" . "see also")
 ("skos:example" . "example")
 ("skos:scopeNote" . "scope note")
 ("skos:altLabel" . "alternative label")
 ("iof-av:explanatoryNote" . "explanatory note")
 ("rdfs:comment" . "comment")
 ("rdfs:isDefinedBy" . "defined by")
 ("iof-av:firstOrderLogicDefinition" . "first-order logic definition")
 ("iof‑av:semiFormalNaturalLanguageDefinition" . "semi-formal definition")
 ("iof-av:semiFormalNaturalLanguageAxiom" . "semi-formal axiom")
 ("iof-av:adaptedFrom" . "adapted from")
 ("iof-av:synonym" . "synonym"))
                                         ))))
            (if tag-mapped
                (setf (plist-get (cadr item) :tag) (cdr tag-mapped)))
            )))
    (unless (and item-tag-stringp
                 (or (string= item-tag "item-translate-start") (string= item-tag "item-translate-stop")))
      (org-latex-item item c info))))
;; defun-ELOT-latex-derived-backend ends here

;; [[file:elot-lob.org::*Passthrough execute for ttl blocks][Passthrough execute for ttl blocks:1]]
(defun org-babel-execute:passthrough (body params) body)
(unless (fboundp 'org-babel-execute:ttl)                
  (defalias 'org-babel-execute:ttl 'org-babel-execute:passthrough))
;; Passthrough execute for ttl blocks:1 ends here
