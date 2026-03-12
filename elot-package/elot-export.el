;;; elot-export.el --- ELOT OMN export orchestration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Keywords: languages outlines tools org ontology

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

;; This module provides a new export orchestration layer for ELOT.
;; The main entry point is `elot-export-omn-for-ontology', which
;; produces complete Manchester Syntax (OMN) output for an ontology
;; heading without requiring a manually maintained :OMN: drawer.
;;
;; The existing :OMN: drawer mechanism in ELOT is unaffected; this
;; module adds an alternative path that calls the same underlying
;; functions (`elot-resource-declarations-from-header',
;; `elot-resource-taxonomy-from-header', `elot-prefix-block-from-alist')
;; but without the noweb boilerplate.
;;
;; Usage:
;;   M-x elot-tangle-ontology   ; export OMN for ontology at point

;;; Code:

(require 'elot)

;;;; Section specification table

(defconst elot-export--section-specs
  '(("ontology-declaration"         "Ontology"           "")
    ("datatypes"                     "Datatype"           "")
    ("class-hierarchy"               "Class"              "SubClassOf")
    ("object-property-hierarchy"     "ObjectProperty"     "SubPropertyOf")
    ("data-property-hierarchy"       "DataProperty"       "SubPropertyOf")
    ("annotation-property-hierarchy" "AnnotationProperty" "SubPropertyOf")
    ("individuals"                   "Individual"         ""))
  "Canonical list of ELOT resource section specifications.
Each entry is a list (ID-SUFFIX OWL-TYPE OWL-RELATION).
ID-SUFFIX is matched case-insensitively against the end of a section's
custom_id property to determine its OWL type.
OWL-RELATION is the empty string for sections that have no taxonomy step.")

;;;; Internal helpers

(defun elot-export--section-spec-for-id (id)
  "Return (OWL-TYPE OWL-RELATION) for section ID, or nil if unrecognized.
Matching is performed case-insensitively on the suffix of ID."
  (let ((lower-id (downcase id)))
    (cl-some (lambda (spec)
               (when (string-suffix-p (car spec) lower-id)
                 (list (cadr spec) (caddr spec))))
             elot-export--section-specs)))

(defun elot-export--section-sort-key (owl-type)
  "Return the canonical sort index for OWL-TYPE.
Lower indices appear earlier in the OMN output."
  (or (cl-position owl-type
                   (mapcar #'cadr elot-export--section-specs)
                   :test #'string=)
      999))

(defun elot-export--section-comment (owl-type)
  "Return the OMN section comment header string for OWL-TYPE declarations."
  (pcase owl-type
    ("Ontology"           "Ontology declaration")
    ("Datatype"           "Datatype declarations")
    ("Class"              "Class declarations")
    ("ObjectProperty"     "Object property declarations")
    ("DataProperty"       "Data property declarations")
    ("AnnotationProperty" "Annotation property declarations")
    ("Individual"         "Individual declarations")
    (_                    (concat owl-type " declarations"))))

(defun elot-export--find-resourcedef-sections (heading-pos)
  "Return resourcedef section specs in the ontology subtree at HEADING-POS.
Walks the direct children of the ontology heading and collects those
with property `:resourcedefs: yes' whose custom_id matches a known
section suffix from `elot-export--section-specs'.

Returns a list of (CUSTOM-ID OWL-TYPE OWL-RELATION) sorted in the
canonical OMN section order defined by `elot-export--section-specs'."
  (save-excursion
    (goto-char heading-pos)
    (let ((heading-level (org-current-level))
          (sections nil))
      (save-restriction
        (org-narrow-to-subtree)
        (while (outline-next-heading)
          ;; Only inspect direct children (one heading level deeper).
          (when (= (org-current-level) (1+ heading-level))
            (let* ((custom-id (org-entry-get (point) "custom_id"))
                   (resourcedefs (org-entry-get (point) "resourcedefs"))
                   (spec (when custom-id
                           (elot-export--section-spec-for-id custom-id))))
              (when (and spec (string= resourcedefs "yes"))
                (push (list custom-id (car spec) (cadr spec)) sections))))))
      (sort (nreverse sections)
            (lambda (a b)
              (< (elot-export--section-sort-key (cadr a))
                 (elot-export--section-sort-key (cadr b))))))))

(defun elot-export--collect-user-omn-blocks (heading-pos)
  "Return user-written omn source block bodies in the subtree at HEADING-POS.
Source blocks inside the :OMN: drawer of the heading are excluded.
Returns a list of non-empty, right-trimmed content strings."
  (save-excursion
    (goto-char heading-pos)
    (save-restriction
      (org-narrow-to-subtree)
      (let ((omn-start nil) (omn-end nil) (blocks nil))
        ;; Locate the :OMN: drawer so we can skip its contents.
        (goto-char (point-min))
        (when (re-search-forward "^:OMN:" nil t)
          (setq omn-start (match-beginning 0))
          (when (re-search-forward "^:END:" nil t)
            (setq omn-end (point))))
        ;; Walk all #+begin_src omn ... #+end_src blocks in the subtree.
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+begin_src +omn\\b" nil t)
          (let ((block-start (match-beginning 0)))
            ;; Skip blocks that fall inside the :OMN: drawer.
            (unless (and omn-start omn-end
                         (>= block-start omn-start)
                         (<= block-start omn-end))
              (forward-line 1)
              (let ((content-start (point)))
                (when (re-search-forward "^[ \t]*#\\+end_src" nil t)
                  (let ((body (buffer-substring-no-properties
                               content-start (line-beginning-position))))
                    (unless (string-blank-p body)
                      (push (string-trim-right body) blocks))))))))
        (nreverse blocks)))))

;;;; Public API

;;;###autoload
(defun elot-export-omn-for-ontology (&optional pos)
  "Generate complete OMN output for the ontology at POS.
POS defaults to point.  The heading at POS must have property
`:ELOT-context-type: ontology'.

Returns the full Manchester Syntax string for this ontology,
including prefixes, ontology declaration, all resource declarations,
all resource taxonomies, and any user-written `omn' source blocks
found in the subtree.

This function replaces the need for a manually written :OMN: drawer
with noweb references.  The existing :OMN: drawer mechanism continues
to work unchanged; this function provides an alternative."
  (save-excursion
    (when pos (goto-char pos))
    (unless (org-at-heading-p)
      (org-previous-visible-heading 1))
    ;; Verify we are at an ontology heading.
    (let ((ctx-type (org-entry-get (point) "ELOT-context-type")))
      (unless (string= ctx-type "ontology")
        (error "Heading at point does not have :ELOT-context-type: ontology")))
    ;; Refresh buffer-local prefix abbreviations from prefix-table.
    ;; Wrapped in save-excursion because org-babel-ref-resolve (called
    ;; inside) may move point as a side effect.
    (save-excursion (elot-update-link-abbrev))
    (let* ((heading-pos (point))
           (localname (or (org-entry-get (point) "ELOT-context-localname") ""))
           ;; Find resource sections in canonical order.
           (sections (elot-export--find-resourcedef-sections heading-pos))
           ;; Preamble comment matching the style used in :OMN: drawers.
           (header-comment
            (concat "##\n## This is the " localname " ontology\n"
                    "## This document is in OWL 2 Manchester Syntax, "
                    "see https://www.w3.org/TR/owl2-manchester-syntax/\n"
                    "##"))
           ;; Prefix declarations (empty when no prefix-table is present).
           (prefix-block
            (when org-link-abbrev-alist-local
              (concat "\n## Prefixes\n"
                      (elot-prefix-block-from-alist
                       org-link-abbrev-alist-local 'omn))))
           ;; OMN declarations for each resource section.
           (decl-parts
            (mapcar
             (lambda (sec)
               (let* ((header-id (car sec))
                      (owl-type  (cadr sec)))
                 (concat "\n## " (elot-export--section-comment owl-type) "\n"
                         (elot-resource-declarations-from-header
                          header-id owl-type))))
             sections))
           ;; Taxonomy axioms for sections that have a non-empty OWL-RELATION.
           (taxo-parts
            (delq nil
                  (mapcar
                   (lambda (sec)
                     (let* ((header-id    (car sec))
                            (owl-type     (cadr sec))
                            (owl-relation (caddr sec)))
                       (when (and owl-relation (not (string-empty-p owl-relation)))
                         (elot-resource-taxonomy-from-header
                          header-id owl-type owl-relation))))
                   sections)))
           ;; User-written omn source blocks outside the :OMN: drawer.
           (user-omn (elot-export--collect-user-omn-blocks heading-pos)))
      ;; Assemble everything in standard OMN order.
      (string-join
       (delq nil
             (append
              (list header-comment)
              (list prefix-block)
              decl-parts
              (when taxo-parts
                (list (concat "\n## Resource taxonomies\n"
                              (string-join taxo-parts "\n"))))
              user-omn))
       "\n"))))

;;;###autoload
(defun elot-tangle-ontology (&optional pos)
  "Export OMN for the ontology at POS and write to the tangle file.
POS defaults to point.  The output file path is read from the
`:header-args:omn:' property of the ontology heading (the value of
the `:tangle' parameter).  The path is interpreted relative to the
directory of the current buffer's file."
  (interactive)
  (save-excursion
    (when pos (goto-char pos))
    (unless (org-at-heading-p)
      (org-previous-visible-heading 1))
    (let* ((header-args (org-entry-get-with-inheritance "header-args:omn"))
           (tangle-file
            (when (and header-args
                       (string-match ":tangle +\\([^ ]+\\)" header-args))
              (match-string 1 header-args)))
           (output-file
            (when tangle-file
              (expand-file-name
               tangle-file
               (file-name-directory
                (or (buffer-file-name) default-directory))))))
      (unless output-file
        (error "Cannot determine tangle file from :header-args:omn: property"))
      (let ((omn-content (elot-export-omn-for-ontology pos)))
        (with-temp-file output-file
          (insert omn-content))
        (message "Wrote OMN to %s" output-file)
        output-file))))

(provide 'elot-export)
;;; elot-export.el ends here
