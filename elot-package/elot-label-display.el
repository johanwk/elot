;;; elot-label-display.el --- Emacs Literate Ontology Tool (ELOT): Label display   -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0

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

;; This file allows for displaying rdfs:label strings instead of
;; resource identifiers while editing an ontology with ELOT.

;; By default for ELOT, switching between display of identifiers and
;; labels is assigned to the F5 key.

;; By default, labels are updated when your file is saved to disk.
;; You can also update labels with M-x `elot-label-display-setup'.

;;; Code:

;; No external dependencies -- ht/dash removed; using built-in gethash.
(require 'elot-tangle)

(defvar org-link-abbrev-alist-local)

(defun elot-org-link-search (&rest strings)
  "Search for an :ID: heading in current buffer.
The concatenation of STRINGS is searched.  If found, move point there
and return position.  If not found, return nil and leave point unchanged."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (re-search-forward (concat ":ID:\\s-*"
                                          (apply #'concat strings)
                                          "\\s-*$")
                                  nil :noerror))))
    (when pos
      (goto-char pos))
    pos))

(defvar-local elot-label-display 'off
  "Value says `off' or `on' to showing labels for RDF resources.")

(defun elot--strip-lang-tag (s)
  "Strip quotes and language/datatype tags from string S like \"abc\"@en."
  (if (and (stringp s) (string-prefix-p "\"" s))
      (replace-regexp-in-string "^\"\\([^\"]+\\)\".*" "\\1" s)
    s))
(defun elot-codelist-id-label (idstring)
  "Given curie IDSTRING, return label if found.
Two-tier lookup: local `elot-codelist-ht' first, then the ELOT label
DB (`elot-db-get-label-any') if available and not found locally."
  (or (and (hash-table-p elot-codelist-ht)
           (elot--strip-lang-tag (gethash idstring elot-codelist-ht)))
      (and (fboundp 'elot-db-get-label-any)
           (fboundp 'sqlite-open)
           (ignore-errors (elot-db-get-label-any idstring)))))
(defun elot-attriblist-label-value (idstring prop)
  "Given label IDSTRING and PROP, return value if found.
Two-tier: local `elot-attriblist-ht' first, then `elot-db-get-attr'
if available."
  (or (and (hash-table-p elot-attriblist-ht)
           (plist-get (gethash idstring elot-attriblist-ht) prop 'equal))
      (and (fboundp 'elot-db-get-attr)
           (fboundp 'sqlite-open)
           (ignore-errors (elot-db-get-attr idstring prop)))))

(defvar elot-codelist-fontify-regexp
  "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9.-]*\\)\\>"
  "A regular expression used to match identifiers, for use with label-display.")

(defun elot-update-codelist-fontify-regexp ()
  "Populate `elot-codelist-fontify-regexp'.

• First try to build a tight union-of-identifiers regexp with
  `regexp-opt' and *eagerly* compile it once.
• If compilation signals “regular expression too big” (or any other
  error), fall back to a generic CURIE pattern:

  \"\\\\<\\\\([-a-z_A-Z0-9]*\\\\):\\\\([a-z_A-Z0-9.-]*\\\\)\\\\>\"

This guarantees that the value stored is always usable during
font-lock and export."
  (let ((fallback "\\<\\([-a-z_A-Z0-9]*\\):\\([a-z_A-Z0-9.-]*\\)\\>"))
    (condition-case err
        (progn
          (unless (listp elot-slurp)
            (error "List of resources `elot-slurp' is missing"))

          ;; Build the precise regexp …
          (let* ((ids (flatten-tree (mapcar #'car elot-slurp)))
                 (precise (regexp-opt ids)))

            ;; … and *force* Emacs to compile it once.  Any size error
            ;; happens right here.
            (string-match-p precise "")

            ;; If we got here, it’s safe to use.
            (setq elot-codelist-fontify-regexp precise)))

      (error
       (message "elot: %s – using generic CURIE regexp." (error-message-string err))
       (setq elot-codelist-fontify-regexp fallback)))))

(defvar elot-fontify-keyword nil
  "Variable holding font-lock pattern.")

(defface elot-label-face
  '((t :inherit italic))
  "Default face used to fontify labels in ELOT overlays."
  :group 'elot)

(defcustom elot-label-display-face 'elot-label-face
  "Customizable face used for ELOT label fontification.
You can choose any face (e.g. `italic', `shadow', `underline')"
  :group 'elot
  :version "29.2"
  :type 'face)

(defun elot-update-fontify-keyword ()
  "Update `elot-fontify-keyword' from collected identifier-label pairs."
  (setq elot-fontify-keyword
        `((,elot-codelist-fontify-regexp
           (0
            (unless (memq (get-char-property (match-beginning 0) 'face)
                          org-level-faces)
              ;; bind once per match — match-string and both ht lookups
              (let* ((id    (match-string 0))
                     (label (elot-codelist-id-label id))
                     (rtype (elot-attriblist-label-value label "rdf:type")))
                (put-text-property (match-beginning 0) (match-end 0)
                                   'help-echo
                                   (concat id "  " label "  (" rtype ")"))
                (when (eq elot-label-display 'on)
                  (put-text-property (match-beginning 0) (match-end 0)
                                     'elot-label-display label)
                  (put-text-property (match-beginning 0) (match-end 0)
                                     'face elot-label-display-face)))))))))

(defun elot-add-label-fontification ()
 "Add label fontification to the font-lock list of keywords, then fontify.
The list of keywords is in `elot-fontify-keyword'."
 (progn
   (with-silent-modifications ;; don't mark as edited
     (font-lock-add-keywords
      nil  ; current buffer
      elot-fontify-keyword 'append))
   (font-lock-flush)))

(defun elot-remove-prop-display ()
  "Remove fontification added by `elot-label-display'."
  (remove-text-properties (point-min) (point-max) '(elot-label-display nil)))

(defun elot-label-attribs-query (&optional filter limit)
  "SPARQL query to retrieve (id, label, list of relationships).
Query resources, optionally with FILTER and LIMIT merged into the query."
  (concat
   (elot-prefix-block-from-alist org-link-abbrev-alist-local 'sparql)
   "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX owl:  <http://www.w3.org/2002/07/owl#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX iof-av: <https://spec.industrialontologies.org/ontology/core/meta/AnnotationVocabulary/>
select distinct ?id ?label ?plist
{ ?id rdfs:label ?label
  filter(lang(?label) = \"\" || lang(?label) = \"en\")    # language should be a user option
  { select ?id
    (concat( group_concat(distinct concat(str(?p), \";;\", str(?o)); separator=\";;\") ) as ?plist)
    where { ?id rdfs:label ?label .
            optional {
              values ?p { rdf:type rdfs:label
                          iof-av:naturalLanguageDefinition dcterms:description skos:definition rdfs:comment }
              ?id ?p ?o .
              filter(!(isBlank(?o))) }
            FILTER isIRI(?id)
            "
   (if filter (concat filter "\n"))
   " } group by ?id ?label }
}"
  (if limit (concat "\nlimit "
                    (if (stringp limit) limit (number-to-string limit) )))))

(defun elot-retrieve-prefixes (uri)
  "Query with SPARQL and return the prefixes.
Prefixes in the query result are returned as a list of (uri, prefix) pairs.
URI is a SPARQL endpoint URL or ontology filename."
  (let ((empty-construct-qry "construct where {?x ?y ?z} limit 0")
        (format ""))
    (with-temp-buffer
      ;; reusing from ELOT customized org-babel-execute:sparql
      (if (string-match-p "^http" uri)  ;; querying an endpoint, or a file?
          (sparql-execute-query empty-construct-qry uri format t)
        (elot-robot-execute-query empty-construct-qry uri 'ttl))
      (mapcar
       (lambda (x)
         (string-match "^\\([^ ]*:\\).*<\\([^>]+\\)>" x)
         (cons (match-string 2 x) (match-string 1 x)))
           (cl-remove ""
                  (split-string
                      (buffer-string)
                      "@prefix +")
                  :test #'equal)))))

(defun elot-replace-strings (str pairs)
  "PAIRS is a list of pairs of strings to replace in string STR."
  (seq-reduce
   (lambda (s pair)
     (string-replace (car pair) (cdr pair) s))
   pairs
   str))

(defun elot-retrieve-labels-plist (url out-file &optional filter limit)
  "Query URL with SPARQL for labels and attributes, optionally FILTER and LIMIT.
Output to OUT-FILE as an elisp list."
  (let ((labels-qry (elot-label-attribs-query filter limit))
        (format "application/sparql-results+json"))
    (with-temp-buffer
      ;; reusing from ELOT customized org-babel-execute:sparql
      (if (string-match-p "^http" url)  ;; querying an endpoint, or a file?
          (sparql-execute-query labels-qry url format t)
        (error "ROBOT ontology-file query not implemented yet for elot labels query"))
        ;; (elot-robot-execute-query labels-qry url 'json)) ; can't output json format
      (let* ((prefixes (elot-retrieve-prefixes url))
             (data-puri (elot-replace-strings (buffer-string) prefixes))
             (bindings (cdr (cadadr (json-read-from-string data-puri)))))
        (with-temp-file (expand-file-name out-file)
          (insert (pp-to-string
           (mapcar (lambda (x)
            (list
             (alist-get 'value (alist-get 'id x))
             (alist-get 'value (alist-get 'label x))
             (string-split
              (alist-get 'value (alist-get 'plist x))
              ";;" t)))
          bindings))))))))

;;(elot-retrieve-labels-plist "http://localhost:3030/bfo-core/query" "~/tmp/bfotest.el")
;;(elot-retrieve-labels-plist "https://www.qudt.org/fuseki/qudt/sparql" "~/tmp/qudttest.el")

(defun elot-read-slurp-global (&rest file-l)
  "FILE-L is a list of files holding elisp lists for label-display."
  (let ((out))
    (cl-loop for l in file-l do
             (setq out
              (append out
               (with-temp-buffer
                 (insert-file-contents (expand-file-name l))
                 (read (buffer-string))))))
    (setq elot-slurp-global out)))

(defun elot-label-display-setup ()
  "Read identifier-label pairs and initialise label-display."
  (interactive)
  (progn
    (elot-update-headline-hierarchy)
    (elot-slurp-to-vars)
    (elot-update-codelist-fontify-regexp)
    (elot-update-fontify-keyword)
    (unless (rassoc '(elot-label-display) char-property-alias-alist)
      (push '(display elot-label-display)
            char-property-alias-alist))
    (elot-add-label-fontification)
    (make-local-variable 'elot-label-display)
    (setq elot-label-display 'on)
    ;; use minibuffer to display info about identifier at point.
    ;; set, then activate.
    (setq help-at-pt-display-when-idle t)
    (help-at-pt-set-timer)))

(defun elot-toggle-label-display ()
  "Toggle between showing identifier or rdfs:label, using `elot-label-display'."
  (interactive)
  (with-silent-modifications
    (if (local-variable-p 'elot-label-display)
        (if (eq elot-label-display 'on)
            (progn (elot-remove-prop-display)
                   (setq elot-label-display 'off)
                   (message "ELOT label-display turned off"))
          (progn (font-lock-flush)
                 (setq elot-label-display 'on)
                 (message "ELOT label-display turned on")))
      ;; not active yet, add fontification
      (elot-add-label-fontification))))

(defvar elot-label-lookup-tmp-attriblist-ht nil
  "Temporary storage for attribute list during label lookup.")

(defun elot-label-lookup-annotations (label)
  "Helper function for `elot-label-lookup' provides preview string for LABEL."
  (let* ((attrib-plist (gethash label elot-label-lookup-tmp-attriblist-ht))
         (rdf-type (plist-get attrib-plist "rdf:type" 'string=))
         (prefix (car (split-string (plist-get attrib-plist "puri" 'string=) ":")))
         (definition (string-replace "\n" " " (string-limit
                      (or (plist-get attrib-plist "iof-av:naturalLanguageDefinition" 'string=)
                          (plist-get attrib-plist "skos:definition" 'string=)
                          (plist-get attrib-plist "dcterms:description" 'string=)
                          (plist-get attrib-plist "rdfs:comment" 'string=)
                          "")
                      120))))
    (concat
     ;; pad annotations to col 35
     (make-string (max (- 35 (length label)) 0) 32)
     "  "
     prefix
     (make-string (max (- 10 (length prefix)) 0) 32)
     rdf-type
     (make-string (max (- 24 (length rdf-type)) 0) 32)
     definition)))

(defun elot-label-lookup ()
  "Interactive lookup of resource identifier, with completion."
  (interactive)
  (let ((completion-extra-properties
         (append completion-extra-properties
                 '(:annotation-function elot-label-lookup-annotations))))
    ;; Store the attriblist globally so annotation function can access it
    (setq elot-label-lookup-tmp-attriblist-ht elot-attriblist-ht)
    (let ((selected-label
           (completing-read
            "Label: " elot-attriblist-ht)))
      (when selected-label
        (insert " ")     ;; Insert a space to avoid getting stuck under the text property
        (backward-char 1)
        (insert (elot-attriblist-label-value selected-label "puri"))
        (forward-char 1)))))



;;;; -------------------------------------------------------------------
;;;; Step 1.7: Activation for non-ELOT buffers
;;;; -------------------------------------------------------------------
;;
;; `elot-global-label-display-mode' is a general-purpose buffer-local
;; minor mode that decorates identifiers in *any* buffer (Turtle,
;; SPARQL, CSV, Python, Markdown, shell scripts, etc.) with labels
;; drawn from the active label sources recorded in the ELOT DB.
;;
;; It has no dependency on:
;;   - `elot-slurp' being populated in the buffer
;;   - prefix tables / CURIE syntax
;;   - the buffer's major mode
;;
;; The font-lock matcher is built from the DB: `regexp-opt' is
;; applied to the union of `entities.id' strings across the active
;; sources (`elot-db-all-active-ids'), producing a literal-token
;; matcher.  This is what lets UC3 buffers (e.g. a Python file
;; containing `EMP-12345') get labelled without any language-aware
;; tokenising.
;;
;; Scope limit (v1): matching is literal tokens only.  No capture
;; groups, no per-source regex templates, no partial-string
;; substitution inside larger tokens.  See the Decisions Log in
;; ELOT-DB-PLAN.org.

(declare-function elot-db-all-active-ids "elot-db" (&optional active-sources))
(declare-function elot-db-init "elot-db" (&optional path))

(defvar-local elot-global--fontify-regexp nil
  "Buffer-local regexp used by `elot-global-label-display-mode'.
Built from `elot-db-all-active-ids' at activation time.")

(defvar-local elot-global--keywords nil
  "Buffer-local font-lock keywords installed by the global mode.")

(defun elot-global--build-regexp (ids)
  "Return a DB-driven font-lock regexp matching literal IDS.
IDS is a list of strings (entity identifiers).  The returned
regexp uses `regexp-opt' with `symbols' boundaries so matches are
anchored at token boundaries rather than inside longer
identifiers.  Returns nil when IDS is empty."
  (when ids
    (regexp-opt ids 'symbols)))

(defun elot-global--build-keywords (regexp)
  "Return a font-lock keyword form decorating REGEXP with labels.
The matcher looks the matched id up via `elot-db-get-label-any'
and, on a hit, installs the `elot-label-display' text property
(aliased to `display' via `char-property-alias-alist').  The
property is only applied when the buffer-local toggle
`elot-label-display' is `on', so \[elot-toggle-label-display]
can hide labels without disabling the mode."
  `((,regexp
     (0 (let* ((id (match-string 0))
               (label (and (fboundp 'elot-db-get-label-any)
                           (ignore-errors (elot-db-get-label-any id)))))
          (when label
            (put-text-property (match-beginning 0) (match-end 0)
                               'help-echo (concat id "  " label))
            (when (eq elot-label-display 'on)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'elot-label-display label)
              (put-text-property (match-beginning 0) (match-end 0)
                                 'face (if (boundp 'elot-label-display-face)
                                           elot-label-display-face
                                         'italic)))))))))

(defun elot-global--install ()
  "Build regexp + keywords from the DB and install them in this buffer."
  (when (and (fboundp 'elot-db-init)
             (or (not (boundp 'elot-db)) (null elot-db)))
    (ignore-errors (elot-db-init)))
  ;; Ensure the `display' alias is registered so that setting the
  ;; `elot-label-display' text property actually swaps the glyph.
  (unless (rassoc '(elot-label-display) char-property-alias-alist)
    (push '(display elot-label-display) char-property-alias-alist))
  (let* ((ids (and (fboundp 'elot-db-all-active-ids)
                   (ignore-errors (elot-db-all-active-ids))))
         (regexp (elot-global--build-regexp ids)))
    (setq elot-global--fontify-regexp regexp)
    (setq elot-global--keywords
          (and regexp (elot-global--build-keywords regexp)))
    (when elot-global--keywords
      (with-silent-modifications
        (font-lock-add-keywords nil elot-global--keywords 'append))
      (font-lock-flush))))

(defun elot-global--uninstall ()
  "Remove decorations and font-lock keywords installed by the mode."
  (when elot-global--keywords
    (with-silent-modifications
      (font-lock-remove-keywords nil elot-global--keywords)
      (remove-text-properties (point-min) (point-max)
                              '(elot-label-display nil help-echo nil))))
  (setq elot-global--keywords nil)
  (setq elot-global--fontify-regexp nil)
  (font-lock-flush))

;;;###autoload
(define-minor-mode elot-global-label-display-mode
  "Display labels from ELOT active label sources in any buffer.

When enabled, identifiers present in the active sources
(`elot-active-label-sources', usually set via `.dir-locals.el' or
`M-x elot-label-activate-source') are decorated in place with
their labels.  Works in any major mode: the font-lock matcher is
built from the DB, not from a language grammar.

Toggle with \\[elot-toggle-label-display] (F5) once enabled.  Use
\\[elot-global-label-display-setup] for a one-shot refresh after
registering or activating new sources."
  :lighter " ELOT-L"
  (if elot-global-label-display-mode
      (progn
        (elot-global--install)
        (make-local-variable 'elot-label-display)
        (setq elot-label-display 'on)
        (local-set-key (kbd "<f5>") #'elot-toggle-label-display))
    (elot-global--uninstall)
    (setq elot-label-display 'off)))

;;;###autoload
(defun elot-global-label-display-setup ()
  "One-shot: (re)build the DB-driven font-lock regexp in this buffer.
Use after registering or activating new label sources to refresh
decorations without toggling the minor mode off and on."
  (interactive)
  (elot-global--uninstall)
  (elot-global--install)
  (unless elot-global-label-display-mode
    (elot-global-label-display-mode 1)))

(provide 'elot-label-display)
;;; elot-label-display.el ends here
