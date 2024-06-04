;;; elot.el --- Emacs Literate Ontology Tool (ELOT)   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.2"))
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
(require 'org-tempo) ; link functions
(require 'htmlize) ; fontify blocks
(require 'omn-mode) ; OMN support
(require 'sparql-mode) ; OMN support
(require 'ob-plantuml) ; PlantUML
(require 'hydra) ; hydra menu
(require 'ht) ; hashtable, for label display

;;;; Usage

;; ... create a new file, use <template inserting function> to insert a template ontology ...

;; [[file:../elot-defs.org::*Setting for post-processing with ROBOT, rdfpuml][Setting for post-processing with ROBOT, rdfpuml:1]]
(defgroup elot 
  nil
  "Customization group for ELOT")
(defcustom elot-robot-jar-path (expand-file-name "~/bin/robot.jar")
  "Path to the robot.jar file."
  :group 'elot
  :version "29.2"
  :type 'string)
(defvar elot-robot-command-str
  (concat "java -jar " elot-robot-jar-path))
(defun elot-robot-command (cmd)
  (if (or (string= elot-robot-jar-path "") (not (file-exists-p elot-robot-jar-path)))
      (error "ROBOT not found. Set elot-robot-jar-path with M-x customize-variable."))
  (shell-command (concat elot-robot-command-str " " cmd)))
(defun elot-robot-omn-to-ttl (omnfile)
  "Call ROBOT to make a Turtle file from `omnfile'."
  (cond
   ((or (string= elot-robot-jar-path "") (not (file-exists-p elot-robot-jar-path)))
    (message "ROBOT not found, not converting to Turtle. Set elot-robot-jar-path with M-x customize-variable."))
   ((not (file-exists-p omnfile))
    (message (concat omnfile " not found, nothing for ROBOT to convert")))
   (t (shell-command
       (concat elot-robot-command-str
               " convert --verbose"
               " --input " omnfile
               " --output " (file-name-sans-extension omnfile) ".ttl")))))
(defun elot-tangled-omn-to-ttl ()
  "After tangling to OMN, call ROBOT to convert to Turtle."
  (let* ((omnfile (buffer-file-name))  ;; will run in the tangled buffer
         (omn-p (string-match-p ".omn$" omnfile)))
    (if omn-p
        (elot-robot-omn-to-ttl omnfile))))
(defcustom elot-default-image-path "./images/"
  "ELOT default output directory for generated images"
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-path (expand-file-name "~/bin/rdf2rml/bin/rdfpuml.pl")
  "Path to the rdfpuml Perl program."
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-options
  "hide empty members
hide circle
skinparam classAttributeIconSize 0"
  "Default options for rdfpuml"
  :group 'elot
  :version "29.2"
  :type 'string)
(defcustom elot-rdfpuml-command-str
  (if (executable-find "rdfpuml") ;; rdfpuml.exe available
    "rdfpuml"  ;; LC_ALL=C should be added, but not available in Windows
    (concat "perl -C -S " elot-rdfpuml-path))
  "Command to execute `rdfpuml'."
  :group 'elot
  :version "29.2"
  :type 'string)
(defun elot-rdfpuml-command (ttl-file)
  (shell-command (concat elot-rdfpuml-command-str " " ttl-file)))
;; Setting for post-processing with ROBOT, rdfpuml:1 ends here

;; [[file:../elot-defs.org::*OMN keywords][OMN keywords:1]]
(defvar elot-omn-property-keywords
'(
    "EquivalentTo"
    "SubClassOf"
    "Characteristics"
    "DisjointWith"
    "Domain"
    "Range"
    "InverseOf"
    "SubPropertyOf"
    "SubPropertyChain"
    "SameAs"
    "DifferentFrom"
    "Types"
    "Facts"
    "HasKey"
    "Import"
    ))
;; OMN keywords:1 ends here

;; [[file:../elot-defs.org::*OMN keywords][OMN keywords:2]]
(defun elot-latex-filter-omn-item (text backend info)
  "Format OMN content in description lists"
  (when (org-export-derived-backend-p backend 'latex)
    (when (seq-some
           (lambda (x)
             (string-match (concat "^\\\\item\\[{" x "}\\]") text))
           elot-omn-property-keywords)
        ;; make the description term texttt
        (setq text (replace-regexp-in-string
                    "\\\\item\\[{\\([a-zA-Z]+\\)}\\]"
                    "\\\\item[\\\\normalfont\\\\ttfamily\\\\small \\1]"
                    text))
        ;; make the list entry content omn inline code unless it's a url
        (if (not (string-match "\\url{.*}$" text))
            (replace-regexp-in-string
             "^\\(.*\\] \\)\\(.*\\)"
             "\\1\\\\lstinline[language=omn]{\\2}"
             text)
          text))))


(add-to-list 'org-export-filter-item-functions
           'elot-latex-filter-omn-item)
;; OMN keywords:2 ends here

;; [[file:../elot-defs.org::*Context identification][Context identification:1]]
(defun elot-context-type ()
  "Retrieve value of property ELOT-context-type for a governing
heading. This will return \"ontology\" if point is under a
heading that declares an ontology."
  (org-entry-get-with-inheritance "ELOT-context-type"))
(defun elot-context-localname ()
  "Retrieve value of property ELOT-context-localname for a governing
heading. This will return the localname of the ontology if point
is under a heading that declares an ontology."
  (org-entry-get-with-inheritance "ELOT-context-localname"))
(defun elot-default-prefix ()
  "Retrieve value of property ELOT-default-prefix for a governing
heading. This will return the default prefix for ontology
resources if point is under a heading that declares an ontology."
  (org-entry-get-with-inheritance "ELOT-default-prefix"))
(defun elot-governing-hierarchy ()
  "Retrieve the ID value of the governing hierarchy, or nil"
  (let ((this-ID
         (org-entry-get-with-inheritance "ID")))
    (and (string-match-p "-hierarchy$" this-ID)
         this-ID)))
;; Context identification:1 ends here

;; [[file:../elot-defs.org::*Looking at][Looking at:1]]
(defun elot-at-ontology-heading ()
  "Return TRUE if point is in a heading that declares ontology"
  (let ((id (or (org-entry-get (point) "ID") "")))
   (string-match "ontology-declaration" id)))
(defun elot-in-class-tree ()
  "Return TRUE if point is a class hierarchy heading"
  (string-match-p "class-hierarchy" (elot-governing-hierarchy)))
(defun elot-in-property-tree ()
  "Return TRUE if point is a property hierarchy heading"
  (string-match-p "property-hierarchy" (elot-governing-hierarchy)))
;; Looking at:1 ends here

;; [[file:../elot-defs.org::defun-desc-lists][defun-desc-lists]]
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
      (if ;; don't include the section that has the target property id itself, except if ontology section
          (or (outline-next-heading)
            (elot-at-ontology-heading))
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
            (nreverse ret))))))
;; defun-desc-lists ends here

;; [[file:../elot-defs.org::defun-puri][defun-puri]]
(defconst puri-re "^\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9-.]*\\)$")

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
         ;; if a language tag @en is present, return unchanged
         (if (string-match "\".*\"@[a-z]+" str)
             (concat " " str)
           ;; escape all quotes with \", note this gives invalid results if some are already escaped
           (concat "  \"" (replace-regexp-in-string "\"" "\\\\\"" str) "\"")))
        (; else, a puri -- wrap in angles
         t (concat "  " (unprefix-uri str org-link-abbrev-alist-local)))))

(defun omn-restriction-string (str)
  "str is wanted as OMN value. Strip any meta-annotations. Otherwise return unchanged."
  (setq str (replace-regexp-in-string " - [^ ]+ ::.*$" "" str))
  str)
;; defun-puri ends here

;; [[file:../elot-defs.org::defun-resource-headings][defun-resource-headings]]
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
  (if (string-match "(\\([-_[:alnum:]]*:[-_[:alnum:]]*\\))" str) ; the resource id is in parentheses
      (match-string 1 str)
    (if (string-match "^\\([-_[:alnum:]]*:[-_[:alnum:]]*\\)" str) ; return string up to whitespace
        (match-string 1 str)
      (if (string-match "(\\([-_[:alnum:]]*:[-_[:alnum:]]* [-_[:alnum:]]*:[-_/.[:alnum:]]*\\))" str) ; two ids in parentheses, for ontology
          (match-string 1 str)
        (error (message "%s%s%s%s%s" "Fail! Heading \"" str "\" in " (org-entry-get-with-inheritance "ID") " is not well-formed") 
               (concat "Malformed_" str))))))
;; defun-resource-headings ends here

;; [[file:../elot-defs.org::defun-resource-declaration][defun-resource-declaration]]
(defun omn-declare (str owl-type)
  "Given a string STR and an OWL type owl-type, write a Manchester Syntax entity declaration. Add rdfs:label annotation. If a parenthesis is given, use that as resource id."
  ;; check whether we have a label and a resource in parentheses
  (let* ((suri (entity-from-header str)))
    (concat owl-type ": " suri)))

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
                                               elot-omn-property-keywords))
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
      ;; if annotations, add to the annotation block that has been started with rdfs:label
      (omn-annotate x)
      (omn-restrict x)
      ))
   l "\n"))

(defun resource-declarations-from-header (header-id owl-type)
  "HEADER-ID is an org location id, OWL-TYPE is Class, etc."
  (save-excursion
    (org-id-goto header-id)
    (let ((entity-l (org-subsection-descriptions)))
      (if (or entity-l (string= owl-type "Ontology"))
          (resource-declarations entity-l owl-type)
        "## (none)"))))
;;(cdr (org-subsection-descriptions))))
;; defun-resource-declaration ends here

;; [[file:../elot-defs.org::*Update link alist from prefix-table][Update link alist from prefix-table:1]]
(defun update-link-abbrev ()
  (if (save-excursion (goto-char (point-min))
                      (re-search-forward "^#[+]name: prefix-table$" nil t))
      (setq-local org-link-abbrev-alist-local
                  (mapcar (lambda (x) 
                            (cons (replace-regexp-in-string ":" "" (car x)) (cadr x)))
          (cl-remove 'hline (org-babel-ref-resolve "prefix-table")))
                  )))
;; Update link alist from prefix-table:1 ends here

;; [[file:../elot-defs.org::*Make prefix blocks for omn, sparql, ttl][Make prefix blocks for omn, sparql, ttl:1]]
(defun elot-prefix-block-from-alist (prefixes format)
  "`prefixes' is an alist of prefixes, from an org-mode table or 
the standard `org-link-abbrev-alist' or `org-link-abbrev-alist-local'. 
`format' is a symbol, either `'omn', `'sparql', or `'ttl'.
Return a string declaring prefixes."
  (let ((format-str
         (cond
          ((eq format 'omn) "Prefix: %-5s <%s>")
          ((eq format 'ttl) "@prefix %-5s <%s> .")
          ((eq format 'sparql) "PREFIX %-5s <%s>"))))
    (mapconcat (lambda (row) 
                 (let ((prefix-str
                        (if (string-match-p ":$" (car row))
                            (car row) (concat (car row) ":")))
                       (uri-str
                        (if (listp (cdr row))
                            (cadr row) ;; comes from org table
                          (cdr row))))
                       (format format-str prefix-str uri-str)))
               (if (equal (car prefixes) '("prefix" . "uri"))
                   (cdr prefixes)
                 prefixes)
                 "\n")))
;; Make prefix blocks for omn, sparql, ttl:1 ends here

;; [[file:../elot-defs.org::*Execute sparql using ROBOT][Execute sparql using ROBOT:1]]
(defun elot-robot-execute-query (query inputfile format)
  "Execute sparql query `query' with ROBOT on ontology file
`inputfile'. `format' is `'csv' for tabular results, or `'ttl'
for RDF results in Turtle."
  (let* ((query-file
          (concat (org-babel-temp-directory) "/"
                  (file-name-base inputfile)
                  ".sparql"))
         (result-file
          (concat (file-name-sans-extension inputfile) (symbol-name format)))
         )
    (with-temp-file query-file (insert query))
    (elot-robot-command
     (concat "query --input " inputfile
             " --format " (symbol-name format)
             " --query " query-file
             " " result-file))
    (insert-file-contents result-file)))
;; Execute sparql using ROBOT:1 ends here

;; [[file:../elot-defs.org::*Execute sparql using ROBOT][Execute sparql using ROBOT:2]]
(defun org-babel-execute:sparql (body params)
  "Execute a block containing a SPARQL query with org-babel.
This function is called by `org-babel-execute-src-block'.
The function has been patched for ELOT to allow query with ROBOT."
  (message "Executing a SPARQL query block with ELOT version of org-babel-execute:sparql.")
  (let* ((url (cdr (assoc :url params)))
         (format (cdr (assoc :format params)))
         (query (org-babel-expand-body:sparql body params))
         (org-babel-sparql--current-curies 
          (append org-link-abbrev-alist-local org-link-abbrev-alist))
         (elot-prefixed-query
          (concat (elot-prefix-block-from-alist org-link-abbrev-alist-local 'sparql)
                  "\n" query))
         (format-symbol
          (if (string-match-p "\\(turtle\\|ttl\\)" format) 'ttl 'csv)))
    (with-temp-buffer
      (if (string-match-p "^http" url)  ;; querying an endpoint, or a file?
          (sparql-execute-query query url format t) ;; add test, does the file exist at all
        (elot-robot-execute-query elot-prefixed-query url format-symbol))
      (org-babel-result-cond
          (cdr (assoc :result-params params))
        (buffer-string)
        (if (string-equal "text/csv" format)
            (org-babel-sparql-convert-to-table)
          (buffer-string))))))
;; Execute sparql using ROBOT:2 ends here

;; [[file:../elot-defs.org::defun-class-patterns][defun-class-patterns]]
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

;; [[file:../elot-defs.org::defun-resource-taxonomy][defun-resource-taxonomy]]
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
    (if (org-goto-first-child)
        (let ((hierarchy-l (org-list-siblings)))
          (resource-taxonomy-from-l hierarchy-l owl-type owl-relation))
      (concat "## no " owl-type "taxonomy"))))
;; defun-resource-taxonomy ends here

;; [[file:../elot-defs.org::defun-latex-export][defun-latex-export]]
(defun ontology-resource-section (level numbered-p)
  (if numbered-p
    (cond 
      ((= 1 level) "\\chapter{%s}")
      ((= 2 level) "\\section{%s}")
      ((= 3 level) "\\subsection{%s}")
      ((= 4 level) "\\subsubsection{%s}")
      ((= 5 level) "\\subsubsubsection{%s}")
      ((= 6 level) "\\paragraph{%s}")
      (t "\\subparagraph{%s}"))
    (cond ;; Koma-script commands, see https://tex.stackexchange.com/questions/193767/how-to-use-unnumbered-chapters-with-koma-script/193799#193799
     ((= 1 level) "\\addchap{%s}")
     ((= 2 level) "\\addsec{%s}")
     ((= 3 level) "\\subsection*{%s}")
     (t "\\subsubsection*{%s}"))
    ))
;; defun-latex-export ends here

;; [[file:../elot-defs.org::defun-get-heading-nocookie][defun-get-heading-nocookie]]
(defun org-get-heading-nocookie (&optional no-tags no-todo no-priority no-comment)
  (replace-regexp-in-string " \\[[[:digit:]/%]+\\]$" ""
                            (org-get-heading no-tags no-todo no-priority no-comment)))
;; defun-get-heading-nocookie ends here

;; [[file:../elot-defs.org::defun-get-description-entry][defun-get-description-entry]]
(defun org-get-description-entry (tag)
  (save-excursion
    (if (search-forward-regexp tag nil t)
        (let* ((element (org-element-at-point))
               (beg (org-element-property :contents-begin element))
               (end (org-element-property :contents-end element))
               (entry-text (buffer-substring-no-properties beg end)))
           (replace-regexp-in-string "\n\s*" " " entry-text)))))
;; defun-get-description-entry ends here

;; [[file:../elot-defs.org::defun-ELOT-latex-derived-backend][defun-ELOT-latex-derived-backend]]
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

;; [[file:../elot-defs.org::*Passthrough execute for ttl blocks][Passthrough execute for ttl blocks:1]]
(defun org-babel-execute:passthrough (body params) body)
(unless (fboundp 'org-babel-execute:ttl)                
  (defalias 'org-babel-execute:ttl 'org-babel-execute:passthrough))
;; Passthrough execute for ttl blocks:1 ends here

;; [[file:../elot-defs.org::*Execute rdfpuml on Turtle content][Execute rdfpuml on Turtle content:1]]
(defun elot-rdfpuml-execute (ttl &optional prefixes config add-options epilogue)
  "Run rdfpuml on Turtle RDF content and return PlantUML code. 
`ttl' is a Turtle string, `prefixes' optional prefix block, 
`config' optional Turtle for rdfpuml configuration, 
`add-options' string of PlantUML options added to rdfpuml defaults,
`epilogue' extra PlantUML clauses"
  (let* ((options-str
         (if add-options
             (concat "[] puml:options \"\"\""
                     elot-rdfpuml-options "\n"
                     add-options
                     "\n\"\"\".\n")))
        (input-ttl-file (org-babel-temp-file "rdfpuml-" ".ttl"))
        (output-puml-file (concat (file-name-sans-extension input-ttl-file) ".puml")))
    (with-temp-file input-ttl-file
      (insert (mapconcat 'identity
                         (list prefixes ttl config options-str) "\n")))
    ;; apparently prefixes.ttl is needed to reside in current dir, will overwrite
    (if prefixes (with-temp-file "prefixes.ttl"
                   (insert prefixes "\n")))
    (elot-rdfpuml-command input-ttl-file)
    (with-temp-file output-puml-file
      (insert-file-contents output-puml-file)
      (if epilogue (replace-string "@enduml"
                                   (concat epilogue "\n" "@enduml"))))
    output-puml-file))
;; Execute rdfpuml on Turtle content:1 ends here

;; [[file:../elot-defs.org::*Execute rdfpuml on Turtle content][Execute rdfpuml on Turtle content:2]]
(defun elot-plantuml-execute (puml-file output-name format)
  "With PlantUML, read `puml-file' and output `output-name'.`format'
to ELOT default image (sub)directory. Return output file name."
  (if (or (string= org-plantuml-jar-path "") (not (file-exists-p org-plantuml-jar-path)))
    (error "PlantUML not found. Set org-plantuml-jar-path with M-x customize-variable."))
  (let ((tmp-output-file (concat (file-name-sans-extension puml-file) "." format))
  (output-file (concat elot-default-image-path output-name "." format)))
    (message (concat puml-file " --> " output-file))
    (make-directory elot-default-image-path :always)
    (shell-command 
     (concat "java -jar " org-plantuml-jar-path " -t" format " " puml-file))
    (copy-file tmp-output-file output-file :allow-overwrite)
    output-file))
;; Execute rdfpuml on Turtle content:2 ends here

;; [[file:../elot-defs.org::*ELOT document header][ELOT document header:1]]
(tempo-define-template "elot-doc-header"
 '("# -*- eval: (load-library \"elot-defaults\") -*-" > n
	"#+title: " (p "Document title: " doctitle) > n
	"#+subtitle: An OWL ontology" > n
	"#+author: " (p "Author name: " authname) > n
	"#+date: WIP (version of " (format-time-string "%Y-%m-%d %H:%M") ")" > n
  "#+call: theme-readtheorg()" n n
	(progn (load-library "elot-defaults") (message "Loaded ELOT") "")
	)
 "<odh"
 "ELOT document header"
 'org-tempo-tags)
;; ELOT document header:1 ends here

;; [[file:../elot-defs.org::*ELOT ontology skeleton][ELOT ontology skeleton:1]]
(tempo-define-template
 "elot-ont-skeleton"
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
     ":OMN:" > n
     "#+begin_src omn :exports none" > n
     "  ##" > n
     "  ## This is the " (s ontlocalname) " ontology" > n
     "  ## This document is in OWL 2 Manchester Syntax, see https://www.w3.org/TR/owl2-manchester-syntax/" > n
     "  ##" > n n
     "  ## Prefixes" > n
     "  <<omn-prefixes()>>" > n  n
     "  ## Ontology declaration" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-ontology-declaration\", owl-type=\"Ontology\", owl-relation=\"\")>>" > n 
     "" > n
     "  ## Data type declarations" > n
     "  Datatype: xsd:dateTime" > n
     "  Datatype: xsd:date" > n
     "  Datatype: xsd:boolean" > n
     "" > n
     "  ## Class declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-class-hierarchy\", owl-type=\"Class\")>>" > n
     "" > n
     "  ## Object property declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-object-property-hierarchy\", owl-type=\"ObjectProperty\")>>" > n
     "" > n
     "  ## Data property declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-data-property-hierarchy\", owl-type=\"DataProperty\")>>" > n
     "" > n
     "  ## Annotation property declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-annotation-property-hierarchy\", owl-type=\"AnnotationProperty\")>>" > n
     "" > n
     "  ## Individual declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-individuals\", owl-type=\"Individual\")>>" > n
     "" > n
     "  ## Resource taxonomies" > n
     "  <<resource-taxonomy(hierarchy=\"" (s ontlocalname) "-class-hierarchy\", owl-type=\"Class\", owl-relation=\"SubClassOf\")>>" > n
     "  <<resource-taxonomy(hierarchy=\"" (s ontlocalname) "-object-property-hierarchy\", owl-type=\"ObjectProperty\", owl-relation=\"SubPropertyOf\")>>" > n
     "  <<resource-taxonomy(hierarchy=\"" (s ontlocalname) "-data-property-hierarchy\", owl-type=\"DataProperty\", owl-relation=\"SubPropertyOf\")>>" > n
     "  <<resource-taxonomy(hierarchy=\"" (s ontlocalname) "-annotation-property-hierarchy\", owl-type=\"AnnotationProperty\", owl-relation=\"SubPropertyOf\")>>" > n
     "#+end_src" > n
     ":END:" > n
"** Prefixes
The ontology document in OWL employs the namespace prefixes of table [[prefix-table]].

#+name: prefix-table
#+attr_latex: :align lp{.8\\textwidth} :font \small
#+caption: OWL ontology prefixes
| prefix    | uri                                                                            |
|-----------+--------------------------------------------------------------------------------|
| owl:      | http://www.w3.org/2002/07/owl#                                                 |
| rdf:      | http://www.w3.org/1999/02/22-rdf-syntax-ns#                                    |
| xml:      | http://www.w3.org/XML/1998/namespace                                           |
| xsd:      | http://www.w3.org/2001/XMLSchema#                                              |
| rdfs:     | http://www.w3.org/2000/01/rdf-schema#                                          |
| skos:     | http://www.w3.org/2004/02/skos/core#                                           |
| pav:      | http://purl.org/pav/                                                           |
| foaf:     | http://xmlns.com/foaf/0.1/                                                     |
| dc:       | http://purl.org/dc/elements/1.1/                                               |
| dcterms:  | http://purl.org/dc/terms/                                                      |
| prov:     | http://www.w3.org/ns/prov#                                                     |
| iof-av:   | https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary/ |" > n
"| " (s resprefix)  
":       | " (p "Resource namespace in full (\"http ...\") " resns) "                                                            |" > n
"| " (p "Namespace prefix for the ontology itself (without the \":\") " ontprefix) 
":       | " (p "Ontology namespace in full (\"http ...\") " ontns) "                                                            |" >  n
"*** Source blocks for prefixes                                     :noexport:
:PROPERTIES:
:header-args:omn: :tangle no
:END:
#+name: sparql-prefixes
#+begin_src emacs-lisp :var prefixes=prefix-table :exports none
  (elot-prefix-block-from-alist prefixes 'sparql)
#+end_src
#+name: omn-prefixes
#+begin_src emacs-lisp :var prefixes=prefix-table :exports none
  (elot-prefix-block-from-alist prefixes 'omn)
#+end_src
#+name: ttl-prefixes
#+begin_src emacs-lisp :var prefixes=prefix-table :exports none
  (elot-prefix-block-from-alist prefixes 'ttl)
#+end_src
"
"
** " (s ontlocalname) " ontology (" (s ontprefix) ":" (s ontlocalname) " " (s ontprefix) ":" (s ontlocalname) "/0.0)
:PROPERTIES:
:ID:       " (s ontlocalname) "-ontology-declaration
:custom_id: " (s ontlocalname) "-ontology-declaration
:resourcedefs: yes
:END:
 # - Import :: https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary/
 - owl:versionInfo :: 0.0 start of " (s ontlocalname) "
 - dcterms:title :: \"" (s ontlocalname) " ontology\"@en
 - pav:lastUpdateOn :: {{{modification-time(\"%Y-%m-%dT%H:%M:%SZ\",t)}}}^^xsd:dateTime
 - dcterms:license :: [[https://creativecommons.org/licenses/by-sa/4.0/]]
 - dcterms:creator :: {{{author}}}
 - dcterms:modified ::  {{{modification-time(\"%Y-%m-%d\",t)}}}^^xsd:date
 - dcterms:publisher :: https://example.org/thepublisher
 - dc:rights :: Copyright info here
 - dcterms:description :: The " (s ontlocalname) " ontology is ...
 - rdfs:comment :: The " (s ontlocalname) " ontology is ...
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
*** pav:lastUpdateOn
 - rdfs:isDefinedBy :: http://purl.org/pav/
*** skos:example
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** skos:prefLabel
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** skos:altLabel
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
*** iof-av:isPrimitive
 - rdfs:isDefinedBy :: https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary
*** skos:definition
 - rdfs:isDefinedBy :: http://www.w3.org/2004/02/skos/core
**** iof-av:naturalLanguageDefinition
 - rdfs:isDefinedBy :: https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary
**** iof-av:primitiveRationale
 - rdfs:isDefinedBy :: https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary
** Individuals
:PROPERTIES:
:ID:       " (s ontlocalname) "-individuals
:custom_id: " (s ontlocalname) "-individuals
:resourcedefs: yes
:END:
"
(progn (update-link-abbrev) 
       (save-buffer) (org-macro-initialize-templates)
       (org-cycle-set-startup-visibility)
       (goto-char (point-min))
       (search-forward "dcterms:description :: ") (outline-show-entry) "")
)
 "<ods"
 "ELOT ontology sections skeleton"
 'org-tempo-tags)
;; ELOT ontology skeleton:1 ends here

;; [[file:../elot-defs.org::*OWL primitive/non-primitive class, with IOF default annotations][OWL primitive/non-primitive class, with IOF default annotations:1]]
(tempo-define-template "elot-class-iof-primitive"
 '(
   (org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Class label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ") [1/4]" > n
   " - [ ] iof-av:naturalLanguageDefinition :: " > n
   " - [X] iof-av:isPrimitive :: true" > n
   " - [ ] iof-av:primitiveRationale :: " > n
   " - [ ] skos:example :: " > 
 )
 "<ocp"
 "ELOT primitive class with IOF-AV annotations"
 'org-tempo-tags)

(tempo-define-template "elot-class-iof-defined"
 '((org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Class label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ") [1/4]" > n
   " - [ ] iof-av:semiFormalNaturalLanguageDefinition :: " > n
   " - [X] iof-av:isPrimitive :: false" > n
   " - [ ] skos:example :: " > 
 )
 "<ocd"
 "ELOT primitive class with IOF-AV annotations"
 'org-tempo-tags)

(tempo-define-template "elot-property-iof"
 '((org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Property label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ") [1/4]" > n
   " - [ ] iof-av:naturalLanguageDefinition :: " > n
   " - [ ] skos:example :: " > 
 )
 "<op"
 "ELOT primitive class with IOF-AV annotations"
 'org-tempo-tags)
;; OWL primitive/non-primitive class, with IOF default annotations:1 ends here

;; [[file:../elot-defs.org::*Code blocks][Code blocks:1]]
(tempo-define-template "elot-block-robot-metrics"
 '(
   (org-open-line 1) p
   "#+call: robot-metrics(omnfile=\"" (elot-context-localname) ".omn\") :eval never-export" > 
   (progn (message "Execute blocks with C-c C-c") "")
 )
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
   (progn (message "Execute blocks with C-c C-c") "")
 )
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
   (progn (message "Execute blocks with C-c C-c") "")
 )
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
   (progn (message "Execute blocks with C-c C-c") "")
 )
 "<obm"
 "ELOT ontology metrics from ROBOT"
 'org-tempo-tags)
;; Code blocks:1 ends here

;; [[file:../elot-defs.org::*Hydra interface F4][Hydra interface F4:1]]
(defhydra hydra-elot (:color blue :hint nil)
  "
 --- ELOT helpdesk --- press F5 to toggle labels ---

 Insert                    Code block             Document         ^^^^^^Output                  
-----------------------------------------------------------------------------------------------
 [_r_] resource id        <_obm_ metrics             <_odh_ header      [_t_] tangle ontology    
<_ocp_ primitive class    <_obs_ sparql select       <_ods_ ontology    [_h_] export HTML        
<_ocd_ defined class      <_obc_ sparql construct                                             
 <_op_ property           <_obd_ rdfpuml diagram                                              
"
  ("r" (elot-label-lookup))
  ("ocp" (progn (outline-next-heading) (tempo-template-elot-class-iof-primitive)))
  ("ocd" (progn (outline-next-heading) (tempo-template-elot-class-iof-defined)))
  ("op" (progn (outline-next-heading) (tempo-template-elot-property-iof)))
  ("t" (org-babel-tangle))
  ("h" (browse-url-of-file (expand-file-name (org-html-export-to-html))))
  ("obm" (tempo-template-elot-block-robot-metrics))
  ("obs" (tempo-template-elot-block-sparql-select))
  ("obc" (tempo-template-elot-block-sparql-construct))
  ("obd" (tempo-template-elot-block-rdfpuml-diagram))
  ("odh" (tempo-template-elot-doc-header))
  ("ods" (tempo-template-elot-ont-skeleton))
  )

(define-key org-mode-map (kbd "<f4>") 'hydra-elot/body)
;; Hydra interface F4:1 ends here

;; [[file:../elot-defs.org::*Read tsv into org table][Read tsv into org table:1]]
(defun elot-tsv-to-table (filename)
  (let* ((lines (with-temp-buffer
                 (insert-file-contents filename)
                 (split-string (buffer-string) "\n")))
         (header (split-string (car lines) "\t"))
         (body (mapcar
                (lambda (line) (split-string line "\t"))
                (butlast (cdr lines)))))  ;; check this is ok
    (cons header (cons 'hline body))))
;; Read tsv into org table:1 ends here

;; [[file:../elot-defs.org::*ROBOT metrics][ROBOT metrics:1]]
(tempo-define-template "robot-metrics"
 '("#+call: robot-metrics(omnfile=\""
   (p "Ontology filename to read for metrics: ") "\")"
   (progn (org-ctrl-c-ctrl-c) "")
   )
   "<om"
   "ROBOT metrics"
   'org-tempo-tags)
;; ROBOT metrics:1 ends here

;; [[file:../elot-defs.org::*End with "provides"][End with "provides":1]]
(provide 'elot)
;; End with "provides":1 ends here
