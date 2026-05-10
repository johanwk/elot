;;; elot-mode.el --- ELOT minor mode for ontology authoring  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
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

;; `elot-mode' is a buffer-local minor mode for authoring OWL
;; ontologies in Org-mode using the ELOT (Emacs Literate Ontology
;; Tool) conventions.
;;
;; Enabling the mode sets up:
;;  - Syntax table tweaks (`:' and `_' as word-constituent)
;;  - Buffer-local hooks for tangling, saving, and display
;;  - Label-display overlays (toggled with F5)
;;  - Org babel Library-of-Babel ingestion
;;
;; Disabling the mode cleanly tears down all of the above.
;;
;; See `elot-mode-map' for keybindings.

;;; Code:

(require 'elot-tangle)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)
(require 'xref)
(require 'easymenu)
(require 'tempo)
(require 'org-tempo)
(require 'ob-lob)

;; Load the full ELOT package (SPARQL, LaTeX export, OWL
;; import, etc.) when available.  The `nil t' arguments mean "don't
;; signal an error if elot.el is not on `load-path'", so users who
;; only have the zero-dependency core still get a working `elot-mode'.
;; Features guarded by `fboundp' in the ELOT menu degrade gracefully.
(require 'elot nil t)

;; Lint is optional -- only loaded when the user invokes it
(autoload 'elot-org-lint "elot-lint" "Refresh `elot-slurp', then run `org-lint'." t)

(defgroup elot
    nil
    "Customization group for Emacs Literate Ontology Tool (ELOT)."
    :prefix "elot-"
    :group 'org)

  (defcustom elot-label-display-size-threshold (* 500 1024)
    "Buffer size (bytes) above which `elot-mode' asks before enabling label-display.
When a file is larger than this threshold, the user is prompted
with a yes/no question.  Files at or below the threshold get
label-display automatically.  Set to 0 to always ask, or nil to
never ask."
    :type '(choice (const :tag "Never ask -- always enable" nil)
                   (integer :tag "Byte threshold"))
    :group 'elot)

(defun elot-mode--set-buffer-locals ()
  "Set buffer-local variables for an ELOT buffer.
These correspond to the `setq-local' block that was in `elot-defaults.el',
minus the LaTeX-specific settings."
  (setq-local
   org-confirm-babel-evaluate nil
   org-export-allow-bind-keywords t
   org-babel-default-inline-header-args '((:exports . "code"))
   tempo-interactive t
   time-stamp-line-limit 100
   time-stamp-format "%Y-%m-%d %H:%M"
   time-stamp-active t
   time-stamp-start "(version of "
   time-stamp-end ")"
   org-startup-folded 'show2levels
   org-export-with-sub-superscripts nil     ; preserve "_" in exports
   org-export-headline-levels 8             ; deep numbering
   org-export-with-section-numbers 8))      ; deep numbering

(defun elot--in-elot-buffer-p ()
  "Return non-nil when the current buffer is an active ELOT Org buffer.
Used as an `:enable' guard in the ELOT menu to grey out entries that
only make sense inside an ELOT Org file (tangle, lint, templates,
xref navigation, etc.) when the menu is visible via
`elot-global-label-display-mode' in a non-ELOT buffer."
  (and (derived-mode-p 'org-mode)
       (bound-and-true-p elot-mode)))

(defvar elot-mode-syntax-table
    (let ((st (make-syntax-table org-mode-syntax-table)))
      (modify-syntax-entry ?: "w" st)
      (modify-syntax-entry ?_ "w" st)
      st)
    "Syntax table for `elot-mode'.
  Treats `:' and `_' as word-constituent characters.")

(tempo-define-template "elot-doc-header"
                       '("#+title: " (p "Document title: " doctitle) > n
                       "#+subtitle: An OWL ontology" > n
                       "#+author: " (p "Author name: " authname) > n
                       "#+date: Created " (format-time-string "%Y-%m-%d %H:%M") > n
                       "#+call: theme-elot()" n n)
                       "<odh"
                       "ELOT document header"
                       'org-tempo-tags)

(tempo-define-template "elot-ont-skeleton"
 '(n > "* " (p "Ontology identifier localname: " ontlocalname) > n
     ":PROPERTIES:" > n
     ":ID: " (s ontlocalname) > n
     ":ELOT-context-type: ontology" > n
     ":ELOT-context-localname: " (s ontlocalname) > n
     ":ELOT-default-prefix: " (p "Namespace prefix for resources in this ontology (without the \":\") " resprefix) > n
     ":header-args:omn: :tangle ./" (s ontlocalname) ".omn :noweb yes" > n
     ":header-args:emacs-lisp: :tangle no :exports results" > n
     ":header-args: :padline yes" > n
     ":END:" > n
"** Prefixes
:PROPERTIES:
:prefixdefs: yes
:END:

#+caption: OWL ontology prefixes
| prefix    | uri                                                                            |
|-----------+--------------------------------------------------------------------------------|
| owl:      | http://www.w3.org/2002/07/owl#                                                 |
| rdf:      | http://www.w3.org/1999/02/22-rdf-syntax-ns#                                    |
| xml:      | http://www.w3.org/XML/1998/namespace                                           |
| xsd:      | http://www.w3.org/2001/XMLSchema#                                              |
| rdfs:     | http://www.w3.org/2000/01/rdf-schema#                                          |
| skos:     | http://www.w3.org/2004/02/skos/core#                                           |
| dc:       | http://purl.org/dc/elements/1.1/                                               |
| dcterms:  | http://purl.org/dc/terms/                                                      |" > n
"| " (s resprefix)
":       | " (p "Resource namespace in full (\"http ...\") " resns) "                                                            |" > n
"| " (p "Namespace prefix for the ontology itself (without the \":\") " ontprefix)
":       | " (p "Ontology namespace in full (\"http ...\") " ontns) "                                                            |" >  n
"
** " (s ontlocalname) " ontology (" (s ontprefix) ":" (s ontlocalname) " " (s ontprefix) ":" (s ontlocalname) "/0.0)
:PROPERTIES:
:ID:       " (s ontlocalname) "-ontology-declaration
:custom_id: " (s ontlocalname) "-ontology-declaration
:resourcedefs: yes
:END:
 - owl:versionInfo :: 0.0 start of " (s ontlocalname) "
 - dcterms:title :: \"" (s ontlocalname) " ontology\"@en
 - dcterms:license :: [[https://creativecommons.org/licenses/by-sa/4.0/]]
 - dcterms:creator :: {{{author}}}
 - dcterms:modified ::  {{{time(\"%Y-%m-%d\",t)}}}^^xsd:date
 - dcterms:publisher :: https://example.org/thepublisher
 - dc:rights :: Copyright info here
 - dcterms:description :: The " (s ontlocalname) " ontology is ...
 - rdfs:comment :: The " (s ontlocalname) " ontology is ...
** Datatypes
:PROPERTIES:
:ID:       " (s ontlocalname) "-datatypes
:custom_id: " (s ontlocalname) "-datatypes
:resourcedefs: yes
:END:
** Classes
:PROPERTIES:
:ID:       " (s ontlocalname) "-class-hierarchy
:custom_id: " (s ontlocalname) "-class-hierarchy
:resourcedefs: yes
:END:
*** My class (" (s resprefix) ":MyClass)
 - rdfs:comment :: Leave a comment here
** Object properties
:PROPERTIES:
:ID:       " (s ontlocalname) "-object-property-hierarchy
:custom_id: " (s ontlocalname) "-object-property-hierarchy
:resourcedefs: yes
:END:
** Data properties
:PROPERTIES:
:ID:       " (s ontlocalname) "-data-property-hierarchy
:custom_id: " (s ontlocalname) "-data-property-hierarchy
:resourcedefs: yes
:END:
** Annotation properties
:PROPERTIES:
:ID:       " (s ontlocalname) "-annotation-property-hierarchy
:custom_id: " (s ontlocalname) "-annotation-property-hierarchy
:resourcedefs: yes
:END:
*** owl:versionInfo
*** dcterms:title
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dcterms:license
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dcterms:creator
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dcterms:modified
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dcterms:publisher
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dcterms:description
 - rdfs:isDefinedBy :: http://purl.org/dc/terms/
*** dc:rights
 - rdfs:isDefinedBy :: http://purl.org/dc/elements/1.1/
*** skos:example
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** skos:prefLabel
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** skos:altLabel
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** skos:definition
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
** Individuals
:PROPERTIES:
:ID:       " (s ontlocalname) "-individuals
:custom_id: " (s ontlocalname) "-individuals
:resourcedefs: yes
:END:
"
(progn (elot-update-link-abbrev)
       (save-buffer) (org-macro-initialize-templates)
       (org-cycle-set-startup-visibility)
       (goto-char (point-min))
       (search-forward "dcterms:description :: ") (outline-show-entry) ""))
 "<ods"
 "ELOT ontology sections skeleton"
 'org-tempo-tags)
;;
;; end of template 'elot-ont-skeleton'
;;

(tempo-define-template "elot-class-skos"
 '(
   (org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Class label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ")" > n
   " - [ ] skos:definition :: " > n
   " - [ ] skos:example :: " > )
 "<ocp"
 "ELOT class heading with SKOS annotations"
 'org-tempo-tags)

(tempo-define-template "elot-property-skos"
 '((org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Property label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ")" > n
   " - [ ] skos:definition :: " > n
   " - [ ] skos:example :: " > )
 "<op"
 "ELOT property heading with SKOS annotations"
 'org-tempo-tags)

(tempo-define-template "elot-block-robot-metrics"
 '(
   (org-open-line 1) p
   "#+call: robot-metrics(omnfile=\"" (elot-context-localname) ".omn\") :eval never-export" >
   (progn (message "Execute blocks with C-c C-c") ""))
 "<obm"
 "ELOT ontology metrics from ROBOT"
 'org-tempo-tags)

(tempo-define-template "elot-block-sparql-select"
 '(
   (org-open-line 1)
"#+name: " (p "Select query name: ") > n
"#+begin_src sparql :url \"" (elot-context-localname) ".omn\" :eval never-export :exports results
  select
  {

  }
#+end_src" n
   (progn (message "Execute blocks with C-c C-c") ""))
 "<obs"
 "ELOT SPARQL SELECT from OMN "
 'org-tempo-tags)

(tempo-define-template "elot-block-sparql-construct"
 '(
   (org-open-line 1)
"#+name: " (p "Construct query name: ") > n
"#+begin_src sparql :url \"" (elot-context-localname) ".omn\" :eval never-export :exports results"
" :format ttl :wrap \"src ttl\" :cache yes :post kill-prefixes(data=*this*) :eval never-export
  construct {

  } {

  }
#+end_src" n
   (progn (message "Execute blocks with C-c C-c") ""))
 "<obc"
 "ELOT SPARQL CONSTRUCT from OMN "
 'org-tempo-tags)

(tempo-define-template "elot-block-rdfpuml-diagram"
 '(
   (org-open-line 1)
   "#+name: rdfpuml:" (p "Name of Turtle source block for diagram: " ttl-source) > n
   "#+call: rdfpuml-block(ttlblock=\"" (s ttl-source) "\") :eval never-export" > n
   "#+caption: " (p "Caption: ") > n
   "#+results: rdfpuml:" (s ttl-source) > n
   (progn (message "Execute blocks with C-c C-c") ""))
 "<obm"
 "ELOT ontology metrics from ROBOT"
 'org-tempo-tags)

(tempo-define-template "elot-table-of-resources"
 '(
   (org-open-line 1)
   "#+name: tbl:" (p "Name of resource table: " tblname) > n
   "| id  | super | rdfs:label |" > n
   "|-----+-------+------------|" > n
   "| " (elot-default-prefix) ": | " (elot-default-prefix) ":   |            |" > n
   (progn (previous-line) (forward-word)
          (message "Insert outline with M-x elot-headings-from-table") ""))
 "<otr"
 "ELOT table of resources"
 'org-tempo-tags)

(defun elot--coerce-literal (header value)
  "Return VALUE coerced per HEADER decoration.
If HEADER has no decoration the original VALUE is returned
unchanged (so numbers stay numbers, symbols stay symbols, etc.)."
  (cond
   ;; language tag
   ((string-match "@\\([[:alnum:]-]+\\)\\'" header)
    (let ((lang (match-string 0 header)))             ; includes leading @
      (if (or (null value) (and (stringp value) (string= value "")))
          value                                       ; keep empty cell empty
        (format "\"%s\"%s" (format "%s" value) lang))))

   ;; datatype
   ((string-match "\\^\\^\\(.+\\)\\'" header)
    (let ((dtype (match-string 0 header)))            ; includes leading ^^
      (if (or (null value) (and (stringp value) (string= value "")))
          value
        (format "\"%s\"%s" (format "%s" value) dtype))))

   (t value)))                                        ; no decoration -> untouched

(defun elot--strip-decoration (header)
  "Return HEADER without trailing @lang or ^^dtype part."
  (cond ((string-match "@[[:alnum:]-]+\\'" header)
         (substring header 0 (match-beginning 0)))
        ((string-match "\\^\\^.+\\'" header)
         (substring header 0 (match-beginning 0)))
        (t header)))

(defun elot--table->forest (mini-table)
  "Convert MINI-TABLE (Org-babel list) to a forest of plists.
- Header suffixes `@lang' / `^^datatype' decorate the cell value
  but are removed from the stored key.
- Warns if a row's SUPER value never appears as an ID.
- Skips `hline` markers that `org-table-to-lisp` inserts."
  (let* ((headers (car mini-table))
         (rows    (cdr mini-table))
         (id-col  (cl-position "id"    headers :test #'string=))
         (sup-col (cl-position "super" headers :test #'string=))
         (id->obj (make-hash-table :test 'equal))
         triples)

    ;; -- pass 1: build node plists and register them ---------------
    (dolist (row rows)
      (when (listp row)                         ; skip `hline`
        (let* ((id  (nth id-col  row))
               (sup (nth sup-col row))
               (pl  (append
                     ;; copy columns: stripped key / decorated value
                     (apply #'append
                            (cl-mapcar (lambda (hdr val)
                                         (list (elot--strip-decoration hdr)
                                               (elot--coerce-literal hdr val)))
                                       headers row))
                     (list :subs nil))))
          (push (list id sup pl) triples)
          (puthash id pl id->obj))))

    (setq triples (nreverse triples))

    ;; -- pass 2: link children to parents, warn on missing parents -
    (dolist (tr triples)
      (cl-destructuring-bind (id sup child) tr
        (unless (string= sup "")
          (let ((parent (gethash sup id->obj)))
            (if parent
                (setf (plist-get parent :subs)
                      (append (plist-get parent :subs) (list child)))
              (message "elot--table->forest: WARNING -- parent id \"%s\" referenced by \"%s\" not found"
                       sup id)
              (setf (nth 1 tr) ""))))))         ; promote to root

    ;; -- pass 3: collect root nodes in original order --------------
    (let (forest)
      (dolist (tr triples)
        (cl-destructuring-bind (_id sup pl) tr
          (when (string= sup "") (push pl forest))))
      (nreverse forest))))

(defun elot--prop (plist key)
  "Return KEY's value in PLIST, comparing keys with `string=`."
  (cl-loop for (k v) on plist by #'cddr
           when (and (stringp k) (string= k key))
           return v))

(defun elot-forest->org (forest &optional level)
  "Render FOREST (from `elot--table->forest`) as Org headlines.
LEVEL is the asterisk depth for root nodes (default 4)."
  (setq level (or level 4))
  (let ((lines '()))
    (cl-labels
        ((emit (node depth)
           (let* ((stars   (make-string (+ level depth) ?*))
                  (id      (elot--prop node "id"))
                  (label   (elot--prop node "rdfs:label"))
                  (name    (if (and label (not (string= label "")))
                               (format "%s (%s)" label id)
                             id)))
             ;; headline
             (push (concat stars " " name) lines)
             ;; description list (skip empty values and certain keys)
             (cl-loop for (k v) on node by #'cddr
                      when (and (stringp k)
                                (not (member k '("id" "super")))
                                v
                                (not (string= v "")))
                      do (push (format "- %s :: %s" k v) lines))
             ;; children
             (dolist (child (plist-get node :subs))
               (emit child (1+ depth))))))
      (dolist (root forest) (emit root 0)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun elot-headings-from-table ()
  "Convert the Org table at point to an ELOT outline.
The table must contain column headers `id' and `super'.
Entities in `id' with optional `rdfs:label' produce
an outline indented under the current headline.

Additional columns are output as description lists entries.
Column headers for these should be annotation
properties (`skos:synonym', etc.) or OWL Manchester Syntax
keywords (`EquivalentTo', `Domain', etc).  Suffixes like `@en'
or `xsd:integer' on a column header will be applied to values."
  (interactive)
  (unless (org-at-table-p)
    (user-error "Point is not inside an Org table"))

  ;; 1. read the table as lisp
  (let* ((mini-table (org-table-to-lisp))
         (headers     (car mini-table)))
    ;; 2. must contain \"id\"
    (unless (member "id" headers)
      (user-error "Table lacks required \"id\" column -- cannot create headings"))

    ;; 3. outline depth = one deeper than containing headline
    (let ((target-level (save-excursion
                          (org-back-to-heading t)
                          (1+ (org-outline-level)))))

      ;; 4. position after table & any TBLFM lines
      (let ((insert-pos
             (save-excursion
               (goto-char (org-table-end))          ; end of last | row
               ;; skip following #+tblfm lines
               (while (and (not (eobp))
                           (progn (beginning-of-line)
                                  (looking-at "^[ \t]*#\\+tblfm:")))
                 (forward-line 1))
               (point))))

        ;; 5. build forest -> org text and insert
        (let* ((forest   (elot--table->forest mini-table))
               (org-text (elot-forest->org forest target-level)))
          (goto-char insert-pos)
          (insert "\n" org-text "\n"))))))

(defvar elot-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The optional binding for `elot-toggle-label-display' is
    ;; installed at mode-enable time (see `elot-mode--apply-toggle-key'
    ;; called from `elot-mode--enable'), not here, because the
    ;; user's customized value of `elot-toggle-labels-key' is not
    ;; necessarily available when this `defvar' runs at load time.
    (define-key map (kbd "C-c C-x r") #'elot-label-lookup)
    ;; S-<f5> previously bound to a Shift-F5 menu; that menu has
    ;; been retired in favour of the ELOT easymenu ("ELOT" in the
    ;; menu bar).
    map)
  "Keymap active when `elot-mode' is enabled.")

(easy-menu-define elot-menu elot-mode-map
  "ELOT Ontology Authoring Menu"
  '("ELOT"
    ["Check for common problems" elot-org-lint
     :active (fboundp 'elot-org-lint)
     :enable (elot--in-elot-buffer-p)]
    ["Export to OWL (Tangle)" elot-tangle-buffer-to-omn
     :enable (elot--in-elot-buffer-p)]
    ["Export to HTML" (lambda () (interactive) (browse-url-of-file (expand-file-name (org-html-export-to-html))))
     :active (fboundp 'org-html-export-to-html)
     :enable (elot--in-elot-buffer-p)]
    ["Import OWL ontology" elot-open-owl
     :active (fboundp 'elot-open-owl)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ["Insert Existing Resource ID" elot-label-lookup :active (fboundp 'elot-label-lookup)]
    ["... (local only)" elot-label-lookup-local
     :active (fboundp 'elot-label-lookup-local)
     :enable (and (bound-and-true-p elot-attriblist-ht)
                  (hash-table-p elot-attriblist-ht)
                  (> (hash-table-count elot-attriblist-ht) 0))]
    ["... (external only)" elot-label-lookup-external
     :active (fboundp 'elot-label-lookup-external)
     :enable (bound-and-true-p elot-active-label-sources)]
    ["Insert Class heading" (lambda () (interactive) (outline-next-heading) (tempo-template-elot-class-skos))
     :active (fboundp 'tempo-template-elot-class-skos)
     :enable (elot--in-elot-buffer-p)]
    ["Insert Property heading" (lambda () (interactive) (outline-next-heading) (tempo-template-elot-property-skos))
     :active (fboundp 'tempo-template-elot-property-skos)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ["Jump to Resource Headline (M-.)" xref-find-definitions
     :enable (elot--in-elot-buffer-p)]
    ["Find References to Resource (M-?)" xref-find-references
     :enable (elot--in-elot-buffer-p)]
    ["Quick-Describe Resource" elot-describe-curie-at-point
     :enable (elot--in-elot-buffer-p)]
    ["Toggle Label-Display" elot-toggle-label-display :active (fboundp 'elot-toggle-label-display)]
    ["Refresh Label-Display Index" elot-label-display-setup
     :active (fboundp 'elot-label-display-setup)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ["Insert Table declaring Resources" tempo-template-elot-table-of-resources
     :active (fboundp 'tempo-template-elot-table-of-resources)
     :enable (elot--in-elot-buffer-p)]
    ["Generate Outline from Table" elot-headings-from-table
     :active (fboundp 'elot-headings-from-table)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ["Insert SPARQL Select Block" tempo-template-elot-block-sparql-select
     :active (fboundp 'tempo-template-elot-block-sparql-select)
     :enable (elot--in-elot-buffer-p)]
    ["Insert SPARQL Construct Block" tempo-template-elot-block-sparql-construct
     :active (fboundp 'tempo-template-elot-block-sparql-construct)
     :enable (elot--in-elot-buffer-p)]
    ["Insert RDFPUML Diagram Block" tempo-template-elot-block-rdfpuml-diagram
     :active (fboundp 'tempo-template-elot-block-rdfpuml-diagram)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ["Insert New Document Header" tempo-template-elot-doc-header
     :active (fboundp 'tempo-template-elot-doc-header)
     :enable (elot--in-elot-buffer-p)]
    ["Insert New Ontology Skeleton" tempo-template-elot-ont-skeleton
     :active (fboundp 'tempo-template-elot-ont-skeleton)
     :enable (elot--in-elot-buffer-p)]
    "---"
    ("Labels & sources"
     ("Source registration"
      ["Register current buffer as label source"
       elot-label-register-current-buffer
       :active (fboundp 'elot-label-register-current-buffer)]
      ["Register label source..."
       elot-label-register-source
       :active (fboundp 'elot-label-register-source)]
      ["Unregister label source..."
       elot-label-unregister-source
       :active (fboundp 'elot-label-unregister-source)
       :enable (and (fboundp 'elot-db-list-sources)
                    (consp (ignore-errors (elot-db-list-sources))))]
      ["Refresh label source..."
       elot-label-refresh-source
       :active (fboundp 'elot-label-refresh-source)
       :enable (and (fboundp 'elot-db-list-sources)
                    (consp (ignore-errors (elot-db-list-sources))))]
      ["Refresh all label sources"
       elot-label-refresh-all-sources
       :active (fboundp 'elot-label-refresh-all-sources)
       :enable (and (fboundp 'elot-db-list-sources)
                    (consp (ignore-errors (elot-db-list-sources))))]
      ["List registered label sources"
       elot-label-list-sources
       :active (fboundp 'elot-label-list-sources)])
     ("Active sources (per buffer / project)"
      ["Activate label source in buffer..."
       elot-label-activate-source
       :active (fboundp 'elot-label-activate-source)]
      ["Deactivate label source in buffer..."
       elot-label-deactivate-source
       :active (fboundp 'elot-label-deactivate-source)
       :enable (bound-and-true-p elot-active-label-sources)]
      ["List/reorder active label sources"
       elot-label-list-active-sources
       :active (fboundp 'elot-label-list-active-sources)]
      ["Raise active source priority..."
       elot-label-move-source-up
       :active (fboundp 'elot-label-move-source-up)
       :enable (bound-and-true-p elot-active-label-sources)]
      ["Lower active source priority..."
       elot-label-move-source-down
       :active (fboundp 'elot-label-move-source-down)
       :enable (bound-and-true-p elot-active-label-sources)])
     ("Display"
      ["Toggle global label display (this buffer)"
       elot-global-label-display-mode
       :active (fboundp 'elot-global-label-display-mode)
       :style toggle
       :selected (bound-and-true-p elot-global-label-display-mode)]
      ["Rebuild label-display matcher"
       elot-global-label-display-setup
       :active (fboundp 'elot-global-label-display-setup)]
      ["Toggle label display"
       elot-toggle-label-display
       :active (fboundp 'elot-toggle-label-display)
       :style toggle
       :selected (and (boundp 'elot-label-display)
                      (eq elot-label-display 'on))]))))

; Also surface the ELOT menu when `elot-global-label-display-mode'
;; is active in a non-ELOT buffer.  We bind the same menu into the
;; global mode's keymap, but mark it `:visible' only when `elot-mode'
;; is NOT active -- that prevents a duplicate ELOT menu when both
;; modes are enabled in the same buffer (elot-mode-map already
;; provides the entry in that case).
;; `with-eval-after-load' guards the load order: if elot-label-display.el
;; loads later, the sibling snippet there handles the other direction.
;; NOTE: `package-lint' warns about `with-eval-after-load' in packages.
;; The warning is informational (configuration code belongs in user
;; init); here it is intentional -- a cross-module load-order bridge
;; between two modules of the same package, not user-config injection.
(with-eval-after-load 'elot-label-display
  (when (and (boundp 'elot-global-label-display-mode-map)
             (boundp 'elot-menu))
    (define-key elot-global-label-display-mode-map
                [menu-bar ELOT]
                `(menu-item "ELOT" ,elot-menu
                            :visible (not (bound-and-true-p elot-mode))))))

(defun elot-mode--add-hooks ()
  "Add buffer-local hooks for an ELOT buffer."
  ;; Pre-tangle: remember source file
  (add-hook 'org-babel-pre-tangle-hook #'elot--remember-org-source nil t)
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images nil t)
  ;; After save: refresh link abbreviations and slurp data
  (add-hook 'after-save-hook #'elot-update-link-abbrev nil t)
  (add-hook 'after-save-hook #'elot-slurp-to-vars nil t)
  ;; xref: register the ELOT backend for M-. / M-?
  (add-hook 'xref-backend-functions #'elot-xref-backend nil t))

(defun elot-mode--remove-hooks ()
  "Remove buffer-local hooks added by `elot-mode--add-hooks'."
  (remove-hook 'org-babel-pre-tangle-hook #'elot--remember-org-source t)
  (remove-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images t)
  (remove-hook 'after-save-hook #'elot-update-link-abbrev t)
  (remove-hook 'after-save-hook #'elot-slurp-to-vars t)
  (remove-hook 'xref-backend-functions #'elot-xref-backend t))

(defvar elot--sparql-advice-installed-p nil
  "Non-nil when ELOT's around-advice on `org-babel-execute:sparql' is active.
Managed by `elot-mode--install-sparql-advice' and
`elot-mode--uninstall-sparql-advice'.")

(defvar elot--sparql-advice-buffer-count 0
  "Number of live buffers in which `elot-mode' is currently enabled.
When this drops to zero, `elot-mode--uninstall-sparql-advice' removes
the global advice on `org-babel-execute:sparql'.")

(defun elot-mode--install-sparql-advice ()
  "Install ELOT's around-advice on `org-babel-execute:sparql'.
Idempotent: safe to call from every `elot-mode--enable'."
  (cl-incf elot--sparql-advice-buffer-count)
  (unless elot--sparql-advice-installed-p
    (when (fboundp 'elot--custom-org-babel-execute-sparql)
      (advice-add 'org-babel-execute:sparql :around
                  #'elot--custom-org-babel-execute-sparql)
      (setq elot--sparql-advice-installed-p t))))

(defun elot-mode--uninstall-sparql-advice ()
  "Remove ELOT's around-advice on `org-babel-execute:sparql' when no
ELOT buffer is left.  Idempotent."
  (when (> elot--sparql-advice-buffer-count 0)
    (cl-decf elot--sparql-advice-buffer-count))
  (when (and elot--sparql-advice-installed-p
             (<= elot--sparql-advice-buffer-count 0))
    (advice-remove 'org-babel-execute:sparql
                   #'elot--custom-org-babel-execute-sparql)
    (setq elot--sparql-advice-installed-p nil
          elot--sparql-advice-buffer-count 0)))

(defvar elot--xref-globals-installed-p nil
  "Non-nil when ELOT's global xref advice and hooks are installed.
Managed by `elot-mode--install-xref-globals' and
`elot-mode--uninstall-xref-globals'.")

(defvar elot--xref-globals-buffer-count 0
  "Reference count of live buffers with `elot-mode' enabled, for the
purpose of managing ELOT's global xref advice and hooks.  When this
drops to zero, `elot-mode--uninstall-xref-globals' removes the
global advice on `xref-find-references' and the entries on
`xref-after-update-hook'.")

(defun elot-mode--install-xref-globals ()
  "Install ELOT's global xref advice and `xref-after-update-hook' entries.
Idempotent: safe to call from every `elot-mode--enable'.  The
backing helpers are defined later in this file, so each install
is guarded with `fboundp' so that loading order does not matter."
  (cl-incf elot--xref-globals-buffer-count)
  (unless elot--xref-globals-installed-p
    (when (fboundp 'elot--capture-slurp)
      (advice-add 'xref-find-references :before #'elot--capture-slurp))
    (when (fboundp 'elot--xref-label-overlay-setup)
      (add-hook 'xref-after-update-hook #'elot--xref-label-overlay-setup))
    (when (fboundp 'elot--xref-buffer-enable-backend)
      (add-hook 'xref-after-update-hook #'elot--xref-buffer-enable-backend))
    (setq elot--xref-globals-installed-p t)))

(defun elot-mode--uninstall-xref-globals ()
  "Remove ELOT's global xref advice and hooks when no ELOT buffer is
left.  Idempotent."
  (when (> elot--xref-globals-buffer-count 0)
    (cl-decf elot--xref-globals-buffer-count))
  (when (and elot--xref-globals-installed-p
             (<= elot--xref-globals-buffer-count 0))
    (advice-remove 'xref-find-references #'elot--capture-slurp)
    (remove-hook 'xref-after-update-hook #'elot--xref-label-overlay-setup)
    (remove-hook 'xref-after-update-hook #'elot--xref-buffer-enable-backend)
    (setq elot--xref-globals-installed-p nil
          elot--xref-globals-buffer-count 0)))

(defvar elot--lob-ingested-p nil
  "Non-nil once ELOT's `elot-lob.org' has been ingested in this Emacs.
Guards `elot-mode--ingest-lob' against redundant re-ingestion when
`elot-mode' is enabled in additional buffers.")

(defun elot-mode--ingest-lob ()
  "Ingest ELOT's `elot-lob.org' into Org's Library of Babel.
No-op after the first successful call in an Emacs session.  Locates
`elot-lob.org' relative to the loaded `elot-mode.el' so it works
both from a Git checkout and from an installed MELPA package."
  (unless elot--lob-ingested-p
    (let* ((mode-lib (locate-library "elot-mode"))
           (lob-file (and mode-lib
                          (expand-file-name
                           "elot-lob.org"
                           (file-name-directory mode-lib)))))
      (when (and lob-file (file-readable-p lob-file))
        (org-babel-lob-ingest lob-file)
        (setq elot--lob-ingested-p t)))))

(defvar elot-mode--applied-toggle-key nil
  "Key currently bound to `elot-toggle-label-display' in `elot-mode-map'.
Tracked so `elot-mode--apply-toggle-key' can rebind cleanly when
the user changes `elot-toggle-labels-key' between activations.")

(defun elot-mode--apply-toggle-key ()
  "Install the user's `elot-toggle-labels-key' binding in `elot-mode-map'.
Reads the current value via `elot--toggle-labels-key' (which also
emits the one-time hint when no value is set), unbinds any
previously-installed key, and binds the new one if any.  Safe to
call from every `elot-mode--enable'."
  (when (boundp 'elot-mode-map)
    ;; Unbind the previously-applied key, if any, so customize-driven
    ;; changes between activations don't leave stale bindings behind.
    (when elot-mode--applied-toggle-key
      (define-key elot-mode-map
                  (kbd elot-mode--applied-toggle-key) nil)
      (setq elot-mode--applied-toggle-key nil))
    (let ((key (and (fboundp 'elot--toggle-labels-key)
                    (elot--toggle-labels-key))))
      (when key
        (define-key elot-mode-map (kbd key)
                    #'elot-toggle-label-display)
        (setq elot-mode--applied-toggle-key key)))))

(defun elot-mode--enable ()
    "Set up ELOT in the current Org buffer."
    ;; Guard: elot-mode is only meaningful inside Org-mode buffers.
    ;; Silently refuse when invoked in a non-Org file so that visiting
    ;; a .ttl (or any non-Org) file does not accidentally spawn a
    ;; second ELOT menu via this mode's keymap.
    (unless (derived-mode-p 'org-mode)
      (elot-mode -1)
      (user-error "elot-mode is only supported in Org-mode buffers"))
    ;; 1. Buffer-local variables
    (elot-mode--set-buffer-locals)

    ;; 2. Syntax table
    (set-syntax-table elot-mode-syntax-table)

    ;; 3. Ingest the Library of Babel (once per Emacs session)
    (elot-mode--ingest-lob)

    ;; 3b. Apply the user's toggle-labels keybinding (if any) into
    ;;     `elot-mode-map' now that customize has had a chance to load.
    (elot-mode--apply-toggle-key)

    ;; 4. Parse the headline hierarchy (populates elot-headline-hierarchy,
    ;;    which is needed by link-abbrev refresh and label-display)
    (elot-update-headline-hierarchy)

    ;; 5. Hooks
    (elot-mode--add-hooks)

    ;; 6. SPARQL advice (global, but reference-counted across ELOT buffers)
    (elot-mode--install-sparql-advice)

    ;; 7. Xref globals (advice on `xref-find-references' and entries on
    ;;    `xref-after-update-hook'; reference-counted across ELOT buffers)
    (elot-mode--install-xref-globals)

    ;; 8. LaTeX export filter for OMN in description lists
    (when (fboundp 'elot-latex-filter-omn-item)
      (add-to-list 'org-export-filter-item-functions
                   'elot-latex-filter-omn-item))

    ;; 9. Export pre-processing hook (linkify, CUSTOM_IDs, prefix resolution)
    (when (fboundp 'elot--prepare-export-buffer)
      (add-hook 'org-export-before-processing-functions
                #'elot--prepare-export-buffer))

    ;; 10. Label-display: set up immediately (with size-gated prompt)
    (elot-mode--maybe-setup-labels)

    ;; 11. Set initial fold visibility
    (org-cycle-set-startup-visibility))

  (defun elot-mode--maybe-setup-labels ()
    "Set up label-display, prompting the user for large files.
If `elot-label-display-size-threshold' is nil, always set up.
If the buffer is larger than the threshold, ask the user first.
In batch mode (`noninteractive'), skip label-display entirely."
    (unless noninteractive
      (let ((threshold elot-label-display-size-threshold))
        (when (or (null threshold)
                  (<= (buffer-size) threshold)
                  (y-or-n-p
                   (format "Buffer is %s KB -- enable ELOT label-display? "
                           (/ (buffer-size) 1024))))
          (elot-label-display-setup)))))

  (defun elot-mode--disable ()
    "Tear down ELOT in the current Org buffer."
    ;; 1. Remove hooks
    (elot-mode--remove-hooks)

    ;; 2. SPARQL advice (uninstall when this is the last ELOT buffer)
    (elot-mode--uninstall-sparql-advice)

    ;; 3. Xref globals (uninstall when this is the last ELOT buffer)
    (elot-mode--uninstall-xref-globals)

    ;; 4. LaTeX export filter
    (setq org-export-filter-item-functions
          (delq 'elot-latex-filter-omn-item
                org-export-filter-item-functions))

    ;; 5. Export pre-processing hook
    (remove-hook 'org-export-before-processing-functions
                 #'elot--prepare-export-buffer)

    ;; 6. Remove label-display overlays
    (when (fboundp 'elot-remove-prop-display)
      (with-silent-modifications
        (elot-remove-prop-display)))

    ;; 7. Restore syntax table
    (set-syntax-table org-mode-syntax-table))

;;;###autoload
(define-minor-mode elot-mode
  "Minor mode for authoring OWL ontologies in Org-mode with ELOT.

When enabled, sets up:
- Tangling hooks for OMN export and ROBOT conversion
- Label-display overlays (toggle with \\[elot-toggle-label-display])
- Syntax table tweaks for CURIE navigation
- Library of Babel ingestion

When disabled, all hooks, timers, and overlays are cleanly removed."
  :lighter " ELOT"
  :keymap elot-mode-map
  (if elot-mode
      (elot-mode--enable)
    (elot-mode--disable)))

(defcustom elot-mode-auto-detect t
  "When non-nil, automatically enable `elot-mode' in ELOT buffers.
Detection looks for `:ELOT-context-type: ontology' in the buffer."
  :type 'boolean
  :group 'elot)

(defun elot-mode--maybe-enable ()
  "Enable `elot-mode' if the current Org buffer looks like an ELOT file."
  (when (and elot-mode-auto-detect
             (derived-mode-p 'org-mode)
             (not (bound-and-true-p elot-mode))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward
                ":ELOT-context-type:.*ontology" nil t)))
    (elot-mode 1)))

(add-hook 'org-mode-hook #'elot-mode--maybe-enable)

(defun elot-xref-backend ()
  "Return the ELOT xref backend identifier."
  'elot)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql elot)))
  "Return a CURIE at point, like :BFO_0000015, or nil if not found."
  (let ((curie-regex "\\(?:\\sw\\|\\s_\\|:\\)+"))
    (save-excursion
      (skip-chars-backward "-_A-Za-z0-9:")
      (when (looking-at curie-regex)
	(match-string-no-properties 0)))))

(cl-defun elot--xref-find-matches (identifier &key find-definition)
  "Return xref matches for IDENTIFIER in all ELOT buffers.
If FIND-DEFINITION is non-nil, restrict matches to headlines;
otherwise return every reference."
  (let ((matches nil))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (and (derived-mode-p 'org-mode)
		   (boundp 'elot-slurp))        ; use whatever predicate marks an ELOT buffer
	  (save-excursion
	    (goto-char (point-min))
	    (let ((pattern (if find-definition
			       (concat "^\\*+ .*\\b" (regexp-quote identifier) "\\b")
			     (concat "\\b" (regexp-quote identifier) "\\b")))
		  (case-fold-search nil))
	      (while (re-search-forward pattern nil t)
		(let ((loc (xref-make-buffer-location
			    buf (line-beginning-position))))
		  (push
		   (xref-make
		    (cond
		     (find-definition
		      (org-get-heading t t t t))  ; headline text only
		     ;; reference context ---------
		     ((not (org-at-heading-p))
		      (let* ((heading (save-excursion
					(or (outline-previous-heading)
					    (goto-char (point-min)))
					(org-get-heading t t t t)))
			     (item (org-element-lineage
				    (org-element-context) '(item) t))
			     (entry-text (if item
					     (buffer-substring-no-properties
					      (org-element-property :begin item)
					      (org-element-property :end   item))
					   (thing-at-point 'line t)))
			     (one-line (replace-regexp-in-string
					"\n\\s-*" " " entry-text)))
			(format "%s\n %s\n" heading (string-trim one-line))))
		     (t
		      (thing-at-point 'line t))) ; fallback
		    loc)
		   matches))))))))
    (nreverse matches)))

(cl-defmethod xref-backend-references  ((_backend (eql elot)) identifier)
  "Return every reference to IDENTIFIER in ELOT buffers."
  (elot--xref-find-matches identifier))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql elot)))
  "Disable identifier completion for ELOT xref backends.

	This prevents Emacs from prompting with completions in xref commands
	like `xref-find-references'."
  nil)

(defvar elot--xref-pending-active-sources nil
  "Transient: snapshot of `elot-active-label-sources' from the caller of xref.
Captured by `elot--capture-slurp' (the :before advice on
`xref-find-references') and consumed by
`elot--xref-label-overlay-setup' to seed the *xref* buffer so that
label display works there too.  Cleared after use.")

(defvar elot--xref-pending-source-buffer nil
  "Transient: the buffer that invoked `xref-find-references'.
Captured alongside `elot--xref-pending-active-sources' so the
calling buffer's file can be added as a DB-backed label source in
the *xref* buffer.  Cleared after use.")

(defun elot--capture-slurp (&rest _args)
  "Stage label-display state for transfer into the *xref* buffer.
Runs as :before advice on `xref-find-references' in the originating
ELOT buffer.  Captures:
  * `elot-slurp' -> `elot-slurp-global' (legacy staging path);
  * `elot-active-label-sources' + the current buffer ->
    `elot--xref-pending-active-sources' /
    `elot--xref-pending-source-buffer', consumed by
    `elot--xref-label-overlay-setup' to enable
    `elot-global-label-display-mode' in the *xref* buffer with the
    caller's sources (and the caller's file) seeded."
  (when (boundp 'elot-slurp)
    (setq elot-slurp-global elot-slurp))
  (setq elot--xref-pending-active-sources
        (and (boundp 'elot-active-label-sources)
             (copy-sequence elot-active-label-sources)))
  (setq elot--xref-pending-source-buffer (current-buffer)))

;; The advice on `xref-find-references' that runs `elot--capture-slurp'
;; before xref is installed by `elot-mode--install-xref-globals' and
;; removed by `elot-mode--uninstall-xref-globals'; see Milestone 3
;; Step 3.5 of ELPA-SUBMISSION-PLAN.org.

(defun elot--xref-label-overlay-setup ()
  "Enable label display in the *xref* buffer using DB-backed sources.
Reads the transient values stashed by `elot--capture-slurp':
  * sets `elot-active-label-sources' buffer-locally in *xref* to the
    caller's list, augmented with an entry naming the caller's file
    so identifiers defined there are looked up;
  * enables `elot-global-label-display-mode' in *xref* (DB-driven
    matcher, works in any major mode);
  * binds the opt-in toggle key (if customized) locally.
Transients are cleared after use."
  (when (equal (buffer-name) "*xref*")
    ;; 1. Carry over active sources from the calling buffer.
    (let* ((sources (copy-sequence elot--xref-pending-active-sources))
           (src-buf elot--xref-pending-source-buffer)
           (src-file (and (buffer-live-p src-buf)
                          (buffer-local-value 'buffer-file-name src-buf))))
      (when (and src-file
                 (not (cl-find-if (lambda (e) (equal (nth 0 e) src-file))
                                  sources)))
        (setq sources (append sources (list (list src-file nil)))))
      (setq-local elot-active-label-sources sources))
    ;; 2. Bind opt-in toggle key (if customized).
    (let ((key (and (fboundp 'elot--toggle-labels-key)
                    (elot--toggle-labels-key))))
      (when key
        (local-set-key (kbd key) #'elot-toggle-label-display)))
    ;; 3. Enable label display via the DB-driven global mode.
    (when (fboundp 'elot-global-label-display-mode)
      (elot-global-label-display-mode 1))
    ;; 4. Clear transients defensively.
    (setq elot--xref-pending-active-sources nil
          elot--xref-pending-source-buffer nil)))

;; Installed/removed via `elot-mode--{install,uninstall}-xref-globals'.

(cl-defmethod xref-backend-definitions ((_backend (eql elot)) identifier)
  "Return Org headlines that *define* IDENTIFIER."
  (elot--xref-find-matches identifier :find-definition t))

;; NOTE: The xref backend is registered/unregistered in
;; elot-mode--add-hooks / elot-mode--remove-hooks, not globally.

(defun elot--xref-buffer-enable-backend ()
  "Enable the ELOT xref backend in the `*xref*` buffer.

This ensures `xref-find-definitions` works on CURIEs inside the xref buffer."
  (when (equal (buffer-name) "*xref*")
    (with-current-buffer (current-buffer)
      (make-local-variable 'xref-backend-functions)
      (add-hook 'xref-backend-functions #'elot-xref-backend nil t))))

;; Installed/removed via `elot-mode--{install,uninstall}-xref-globals'.

(cl-defun elot-describe-curie-at-point (&optional curie)
  "Pop up a *Help* buffer describing CURIE at point (or prompt).
Shows the defining headline and a few reference examples, each as
a clickable button.  Label overlays are rendered inside the help
buffer exactly like they are in the *xref* buffer."
  (interactive
   (list
    (or (cl-letf* ((backend 'elot)
		   (id-fn (symbol-function
			   'xref-backend-identifier-at-point)))
	  (funcall id-fn backend))
	(read-string "ELOT CURIE: "))))
  (unless (and curie (stringp curie) (not (string-empty-p curie)))
    (user-error "No CURIE given"))
  ;; ------------------------------------------------------------------
  ;; 1. Capture caller-side label-display state for transfer into the
  ;;    *ELOT Describe* buffer.  Mirrors the xref path: snapshot
  ;;    `elot-active-label-sources' and the calling buffer's file so the
  ;;    Help buffer can use `elot-global-label-display-mode' (DB-backed)
  ;;    instead of the older buffer-local `elot-slurp' path.
  ;; ------------------------------------------------------------------
  (let* ((caller-sources (and (boundp 'elot-active-label-sources)
                              (copy-sequence elot-active-label-sources)))
         (caller-file    buffer-file-name))
    ;; ----------------------------------------------------------------
    ;; 2. Gather xref data
    ;; ----------------------------------------------------------------
    (let* ((defns   (elot--xref-find-matches curie :find-definition t))
	   (refs    (elot--xref-find-matches curie))
	   (buffer  (get-buffer-create "*ELOT Describe*"))
	   (max-ref 10))
      ;; ----------------------------------------------------------------
      ;; 3. Build the Help buffer
      ;; ----------------------------------------------------------------
      (with-help-window buffer
	(with-current-buffer buffer
	  (help-mode)
	  (setq truncate-lines t)
	  ;; ------------------------------------
	  ;; 3a. Header & definition
	  ;; ------------------------------------
	  (princ (format "%s\n\n" curie))
	  (if defns
	      (progn
		(princ (propertize "Defined in:\n" 'face 'bold))
		(elot--describe--insert-xref-button (car defns) 2))
	    (princ (propertize "No definition found.\n" 'face 'warning)))
	  (princ "\n")
	  ;; ------------------------------------
	  ;; 3b. References (first N)
	  ;; ------------------------------------
	  (princ (propertize "Selected references:\n" 'face 'bold))
	  (if refs
	      (let ((count 0))
		(dolist (xref refs)
		  (when (< count max-ref)
		    (elot--describe--insert-xref-button xref 4)
		    (setq count (1+ count))))
		(when (> (length refs) max-ref)
		  (princ (format "  ...and %d more\n"
				 (- (length refs) max-ref)))))
	    (princ "  (none)\n"))
	  (princ
	   "\n----\n`q' to quit, `RET' to visit location.\n")
	  ;; ------------------------------------
	  ;; 3c. Seed DB-backed label sources, bind opt-in toggle key, and
	  ;;     enable label display via `elot-global-label-display-mode'
	  ;;     (same path as the *xref* buffer).
	  ;; ------------------------------------
	  (let ((sources (copy-sequence caller-sources)))
	    (when (and caller-file
		       (not (cl-find-if (lambda (e) (equal (nth 0 e) caller-file))
					sources)))
	      (setq sources (append sources (list (list caller-file nil)))))
	    (setq-local elot-active-label-sources sources))
	  (let ((key (and (fboundp 'elot--toggle-labels-key)
                          (elot--toggle-labels-key))))
            (when key
              (local-set-key (kbd key) #'elot-toggle-label-display)))
	  (when (fboundp 'elot-global-label-display-mode)
	    (elot-global-label-display-mode 1)))))))

(defun elot--describe--insert-xref-button (xref indent)
  "Insert XREF as an indented bullet with filename and a clickable link."
  (let* ((summary (xref-item-summary xref))
	 (loc     (xref-item-location  xref))
	 (marker  (xref-location-marker loc))
	 (buf     (marker-buffer marker))
	 (file    (or (buffer-file-name buf) (buffer-name buf)))
	 (short   (file-name-nondirectory file))
	 (line    (with-current-buffer buf
		    (line-number-at-pos marker))))
    ;; bullet + file prefix
    (insert (make-string indent ?\s) "- "
	    (propertize (concat short ": ") 'face 'font-lock-keyword-face))
    ;; clickable summary
    (insert-text-button
     summary
     'follow-link t
     'elot-target-buffer buf
     'elot-target-pos    (marker-position marker)
     'action (lambda (btn)
	       (let ((target-buf (button-get btn 'elot-target-buffer))
		     (pos        (button-get btn 'elot-target-pos)))
		 (pop-to-buffer target-buf)
		 (goto-char pos)
		 (xref-pulse-momentarily)))
     'help-echo (format "%s:%d" short line))
    (insert "\n")))

(provide 'elot-mode)
;;; elot-mode.el ends here
