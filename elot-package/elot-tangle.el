;;; elot-tangle.el --- ELOT tangling and OMN generation  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse an ELOT Org buffer's headline hierarchy into an AST and
;; generate OWL Manchester Syntax (OMN) files from it.  This module
;; is required by `elot-mode'.

;;; Code:

(require 'cl-lib)

(defvar elot-omn-property-keywords
  '(
    "EquivalentTo"
    "SubClassOf"
    "Characteristics"
    "DisjointWith"
    "DisjointUnionOf"
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
    "Import"))
(defvar elot-omn-misc-keywords
  '("DisjointClasses"
    "EquivalentClasses"
    "DisjointProperties"
    "EquivalentProperties"
    "SameIndividual"
    "DifferentIndividuals"
    "Rule"))
(defvar elot-omn-all-keywords
  (append elot-omn-property-keywords elot-omn-misc-keywords)
  "List of all Manchester syntax keywords, both property and misc keywords.")

(defvar elot-owl-builtin-resources
  '("owl:Thing" "owl:Nothing" "xsd:string" "xsd:boolean" "xsd:decimal" "xsd:integer"
    "xsd:float" "xsd:double" "xsd:dateTime" "xsd:time" "xsd:date" "xsd:gYear"
    "xsd:gMonth" "xsd:gDay" "xsd:gYearMonth" "xsd:gMonthDay" "xsd:hexBinary"
    "xsd:base64Binary" "xsd:anyURI" "xsd:normalizedString" "xsd:token" "xsd:language"
    "xsd:Name" "xsd:NCName" "xsd:NMTOKEN" "rdf:PlainLiteral")
  "List of built-in OWL and XSD resources that are always considered known.")

(defconst elot-puri-re 
  "^\\([a-zA-Z][-a-zA-Z0-9_.]*\\|\\):\\([-[:word:]_./]*\\)$")

(defun elot-context-type ()
  "Retrieve value of property ELOT-context-type for a governing heading.
This will return \"ontology\" if point is under a heading that
declares an ontology."
  (org-entry-get-with-inheritance "ELOT-context-type"))
(defun elot-context-localname ()
  "Retrieve value of property ELOT-context-localname for a governing heading.
This will return the localname of the ontology
if point is under a heading that declares an ontology."
  (org-entry-get-with-inheritance "ELOT-context-localname"))
(defun elot-default-prefix ()
  "Retrieve value of property ELOT-default-prefix for a governing heading.
This will return the default prefix for ontology resources
if point is under a heading that declares an ontology."
  (org-entry-get-with-inheritance "ELOT-default-prefix"))
(defun elot-governing-hierarchy ()
  "Return the governing hierarchy ID if inside a hierarchy section, or nil."
  (let ((this-ID (org-entry-get-with-inheritance "ID")))
    (when (and this-ID
               (string-match-p "-hierarchy$" this-ID))
      this-ID)))

(defun elot-at-ontology-heading ()
  "Return TRUE if point is in a heading that declares ontology."
  (let ((id (or (org-entry-get (point) "ID") "")))
   (string-match "ontology-declaration" id)))
(defun elot-in-class-tree ()
  "Return TRUE if point is a class hierarchy heading."
  (string-match-p "class-hierarchy" (elot-governing-hierarchy)))
(defun elot-in-property-tree ()
  "Return TRUE if point is a property hierarchy heading."
  (string-match-p "property-hierarchy" (elot-governing-hierarchy)))

(defun elot-unprefix-uri (puri abbrev-alist &optional noerror)
  "Replace prefix in PURI with full form from ABBREV-ALIST, if there's a match."
  (if (eq abbrev-alist nil) puri
    (if (string-match elot-puri-re puri)
        (let* ((this-prefix (match-string-no-properties 1 puri))
               (this-localname (match-string-no-properties 2 puri))
               (this-ns (cdr (assoc this-prefix abbrev-alist))))
          (if this-ns
              (concat "<" this-ns this-localname ">")
            (if noerror
                nil
              ;;(error "Fail! Prefix \"%s\" is not defined" this-prefix)
              ;; tentatively just let the raw value through
              puri)))
          puri)))

(defun elot-annotation-string-or-uri (str)
  "Expand STR to be used as an annotation value in Manchester Syntax.
Expand uri, or return number, or wrap in quotes."
  ;; maybe there's macros in the string, expand them
  (if (string-match "{{{.+}}}" str)
      (let ((omt org-macro-templates))
        (with-temp-buffer (org-mode)
                          (insert str) (org-macro-replace-all omt)
                          (setq str (buffer-string)))))
  (cond
   ;; a number -- return the string
   ((string-match "^[[:digit:]]+[.]?[[:digit:]]*$" str)
    (concat "  " str))
   ;; a bare URI, which org-mode wraps in double brackets -- wrap in angles
   ((string-match "^[[][[]\\(http[^ ]*\\)[]][]]$" str)
    (concat "  <" (match-string 1 str) ">"))
   ;; a bare URI, but no double brackets -- wrap in angles
   ((string-match "^\\(http[^ ]*\\)$" str)
    (concat "  <" (match-string 1 str) ">"))
   ;; a bare URI, in angles
   ((string-match "^\\(<http[^ ]*>\\)$" str)
    (concat "  " (match-string 1 str)))
   ;; a bare URN, in angles
   ((string-match "^\\(<urn:[^>]+>\\)$" str)
    (concat "  " (match-string 1 str)))
   ;; a URN without angles, explicitly treat as xsd:string
   ((string-match "^\\(urn:uuid[^ ]+\\)$" str)
    (concat "  \"" (match-string 1 str) "\"^^xsd:string"))
   ;; true -- make it an explicit boolean
   ((string-match "^true$" str) " \"true\"^^xsd:boolean")
   ;; false -- make it an explicit boolean
   ((string-match "^false$" str) " \"false\"^^xsd:boolean")
   ;; string with datatype -- return unchanged
   ((string-match "^\".*\"\\^\\^[-_[:alnum:]]*:[-_[:alnum:]]+$" str)
    (concat "  " str))
   ;; not a puri -- normal string, wrap in quotes
   ((equal str (elot-unprefix-uri str org-link-abbrev-alist-local))
    (if (string-match "\"\\(.*\n\\)*.*\"@[a-z]+" str)
        (concat " " str)
      (concat "  \"" (replace-regexp-in-string "\"" "\\\\\"" str) "\"")))
   ;; else, a puri -- wrap in angles
   (t (concat "  " (elot-unprefix-uri str org-link-abbrev-alist-local :noerror)))))

(defun elot-org-elt-exists (x elt)
  "Return a list of elements of type ELT extracted from X.
Uses `org-element-map` to collect matching elements.
The function is used to check whether the list contains ELT."
  (org-element-map x elt #'identity))

(defun elot-org-elt-item-tag-str (x)
  "For an item X in an `org-element-map', return the item tag."
  (if (org-element-property :tag x)
      (substring-no-properties (org-element-interpret-data (org-element-property :tag x)))))

(defun elot-meta-annotation-tag-p (tag)
  "Return non-nil if TAG is a recognizable URI."
  (and tag
       (stringp tag)
       (let ((u (elot-unprefix-uri tag org-link-abbrev-alist-local t)))
         (and (stringp u)
              (string-match-p "^<" u)))))

(defun elot-org-elt-item-pars-str (x)
  "For an item X in an `org-element-map', return the paragraphs as one string.
Stops at the first nested description list item that has a recognizable URI tag,
so meta-annotations are excluded from the literal text."
  (let ((result nil))
    (catch 'stop
      (dolist (child (org-element-contents x))
        (let ((type (car child)))
          (cond
           ((eq type 'paragraph)
            (push (substring-no-properties (org-element-interpret-data child)) result))
           ((eq type 'plain-list)
            (let ((sub-result nil))
              (dolist (subitem (org-element-contents child))
                (let ((tag (elot-org-elt-item-tag-str subitem)))
                  (if (elot-meta-annotation-tag-p tag)
                      (progn
                        (when sub-result
                          (push (string-join (nreverse sub-result) "") result))
                        (throw 'stop t))
                    (push (substring-no-properties (org-element-interpret-data subitem)) sub-result))))
              (when sub-result
                (push (concat (string-join (nreverse sub-result) "")
                              (make-string (or (org-element-property :post-blank child) 0) ?\n))
                      result))))
           (t
            (push (substring-no-properties (org-element-interpret-data child)) result))))))
    (string-trim-right (string-join (nreverse result) ""))))

(defun elot-org-elt-item-str (x)
  "For X in an `org-element-map', return pair of strings (tag, paragraph content)."
  (list (elot-org-elt-item-tag-str x) (elot-org-elt-item-pars-str x)))

(defun elot-entity-from-header (str &optional noerror)
  "Given a heading text STR, return the identifier it declares.

The returned value is either
  - a CURIE (e.g. \"ex:Apple\"), or
  - a full URI wrapped in \"<>\" (e.g. \"<http://example.org/Apple>\"), or
  - a composite string like \"ex:Ont <http://…/0.9>\" for ontology/version
    pairs.

If the heading contains *no* recognisable identifier and NOERROR is
non-nil, return NIL.  Otherwise raise an error."
  (let* ((curie-regex "\\(?:[a-zA-Z][-a-zA-Z0-9_.]*\\|\\):\\(?:[-[:word:]_./]*\\)")
         (full-uri-regex "http[s]?://[-[:alnum:]._~:/?#\\@!$&'()*+,;=%]*"))
    (cond
     ;; single URI, beginning of line
     ((string-match (format "^<?\\(%s\\)>?" full-uri-regex) str)
      (format "<%s>" (match-string 1 str)))
     ;; single URI in parentheses
     ((string-match (format "(<?\\(%s\\)>?)" full-uri-regex) str)
      (format "<%s>" (match-string 1 str)))
     ;; CURIE, beginning of line
     ((string-match (format "^\\(%s\\)" curie-regex) str)
      (match-string 1 str))
     ;; CURIE in parentheses
     ((string-match (format "(\\(%s\\))" curie-regex) str)
      (match-string 1 str))
     ;; two URIs in parentheses (ontology and ontology version)
     ((string-match (format "(<?\\(%s\\)>? <?\\(%s\\)>?)" full-uri-regex full-uri-regex) str)
      (let ((uri1 (match-string 1 str))
            (uri2 (match-string 2 str)))
        (format "<%s> <%s>" uri1 uri2)))
     ;; CURIE, then URI in parentheses (ontology and ontology version)
     ((string-match (format "(\\(%s\\) <?\\(%s\\)>?)" curie-regex full-uri-regex) str)
      (format "%s <%s>" (match-string 1 str) (match-string 2 str)))
     ;; two CURIEs in parentheses (ontology and ontology version)
     ((string-match (format "(\\(%s\\) \\(%s\\))" curie-regex curie-regex) str)
      (format "%s %s" (match-string 1 str) (match-string 2 str)))
     ;; URN identifier: return as-is if the string is a URN, e.g. <urn:isbn:0943396611>
     ((string-match "^<urn:[^>]+>$" str) str)
     ;; URN in parentheses
     ((string-match "(\\(<urn:[^>]+>\\))" str)
      (match-string 1 str))
     (t
      (if noerror
          nil
        (error "Fail! Heading \"%s\" in %s is not well-formed"
               str
               (org-entry-get-with-inheritance "ID")))))))

(defcustom elot-robot-jar-path (expand-file-name "~/bin/robot.jar")
  "Path to the robot.jar file."
  :group 'elot
  :version "29.2"
  :type 'string)
(defvar elot-robot-command-str
  (concat "java -jar " elot-robot-jar-path))
(defcustom elot-exporter-jar-path (expand-file-name "~/bin/elot-exporter.jar")
  "Path to the elot-exporter.jar file."
  :group 'elot
  :version "29.2"
  :type 'string)
(defvar elot-exporter-command-str
  (concat "java -jar " elot-exporter-jar-path))

(defun elot-robot-command (cmd)
  "Execute ROBOT command CMD using `shell-command'.
  Check whether `elot-robot-jar-path` is set and points to an existing file.
  It not set, return an error."
  (if (or (string= elot-robot-jar-path "") (not (file-exists-p elot-robot-jar-path)))
      (error "ROBOT not found.  Set elot-robot-jar-path with M-x customize-variable"))
  (shell-command (concat elot-robot-command-str " " cmd)))

;; Helper function for synchronous batch execution
(defun elot-robot-omn-to-ttl--batch (omnfile output-file command-args)
  "Perform synchronous ROBOT conversion for batch mode.
Handles process execution via `call-process`, output parsing on error,
and calls `kill-emacs` on failure.
OMNFILE, OUTPUT-FILE are file paths. COMMAND-ARGS is the full
list of arguments for the process, starting with \"java\"."
  (let* ((output-buffer (generate-new-buffer "*ROBOT Output (Batch)*"))
         (process-connection-type nil) ; Important for batch stability
         (exit-code nil))
    (message "[elot-robot-omn-to-ttl Batch] Executing synchronously: %s" (mapconcat #'shell-quote-argument command-args " "))
    (unwind-protect ; Ensure buffer cleanup
        (progn
          ;; Execute: program is first element, rest are args
          (setq exit-code (apply #'call-process (car command-args) nil output-buffer t (cdr command-args)))
          (message "[elot-robot-omn-to-ttl Batch] ROBOT process finished with exit code: %d" exit-code)

          (if (= exit-code 0)
              ;; Success Case (Batch)
              (message "ROBOT: Conversion of %s successful: %s" omnfile output-file)

            ;; Failure Case (Batch)
            (progn
              (message "ROBOT: Conversion failed (exit code %d)." exit-code)
              ;; Try to extract specific error, suppress full output
              (with-current-buffer output-buffer
                (goto-char (point-min))
                (if (and (re-search-forward ; Look for specific parser error
                          "^Parser: org\\.semanticweb\\.owlapi\\.manchestersyntax\\.parser\\.ManchesterOWLSyntaxOntologyParser" nil t)
                         (re-search-forward "Encountered"))
                    ;; Found specific error - extract and print ONLY that
                    (let* ((start (line-beginning-position))
                           (end (or (and (re-search-forward
                                          "org\\.semanticweb\\.owlapi\\.manchestersyntax\\.parser\\.ManchesterOWLSyntaxOntologyParser" nil t)
                                         (line-beginning-position))
                                    (point-max)))
                           (error-text (buffer-substring-no-properties start end)))
                      (message "ROBOT parse error detected:\n%s" error-text))
                  ;; Didn't find specific error - print generic failure message
                  (message "ROBOT failed. Full output suppressed. No specific parse error found.")))
              ;; Exit Emacs directly with non-zero status using kill-emacs
              (kill-emacs 1))) ; <--- Signal failure without Elisp error
          )
      ;; Cleanup (called by unwind-protect)
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer)))))

;; Helper function for asynchronous interactive execution
(defun elot-robot-omn-to-ttl--interactive (omnfile output-file command-args)
  "Perform asynchronous ROBOT conversion for interactive mode.
Handles process execution via `make-process` and sets up a sentinel
for feedback and error handling (including potential jumping).
OMNFILE, OUTPUT-FILE are file paths. COMMAND-ARGS is the full
list of arguments for the process, starting with \"java\"."
  (let* ((buffer-name "*ROBOT Errors (Interactive)*")
         (buffer (get-buffer-create buffer-name)))
    (message "[elot-robot-omn-to-ttl Interactive] Starting asynchronous process.")
    (with-current-buffer buffer (erase-buffer))
    (make-process
     :name "robot-convert-interactive"
     :buffer buffer
     :command command-args ; Pass the full list including "java"
     :stderr buffer
     :noquery t
     :sentinel
     ;; Sentinel logic - suitable for interactive use
     (lambda (proc _event)
       (when (not (process-live-p proc))
         (if (= (process-exit-status proc) 0)
             (message "ROBOT: Conversion successful: %s" output-file)
           ;; --- Failure Case (Interactive) ---
           (with-current-buffer buffer
             (goto-char (point-min))
             (if (and (re-search-forward
                       "^Parser: org\\.semanticweb\\.owlapi\\.manchestersyntax\\.parser\\.ManchesterOWLSyntaxOntologyParser" nil t)
                      (re-search-forward "Encountered"))
                 ;; Found specific error - show it and try to jump
                 (let* ((start (line-beginning-position))
                        (end (or (and (re-search-forward
                                       "org\\.semanticweb\\.owlapi\\.manchestersyntax\\.parser\\.ManchesterOWLSyntaxOntologyParser" nil t)
                                      (line-beginning-position))
                                 (point-max)))
                        (error-text (buffer-substring-no-properties start end))
                        ;; Attempt parsing location, ignore errors if it fails
                        (loc (ignore-errors (elot--parse-robot-error-location error-text))))
                   (message "ROBOT parse error:\n%s" error-text)
                   (when loc ; Only jump if location parsing worked
                     (elot--jump-to-omn-error omnfile (car loc) (cadr loc))
                     (elot--jump-to-org-heading-for-identifier omnfile (car loc))))
               ;; Didn't find specific error - show generic message & buffer name
               (message "ROBOT failed, but no parse error could be extracted. See buffer %s." buffer-name))
             ;; Optional: Display the error buffer for the user interactively
             ;; (display-buffer buffer)
             )))))))

;; Main dispatcher function
(defun elot-robot-omn-to-ttl (omnfile)
  "Convert OMNFILE (Manchester Syntax) to Turtle using ROBOT.
Dispatches to synchronous batch or asynchronous interactive helpers.
Checks for `elot-robot-jar-path`."

  ;; --- Common Setup ---
  (message "[elot-robot-omn-to-ttl] Starting conversion for: %s (Mode: %s)"
           omnfile (if noninteractive "Batch" "Interactive"))
  (unless (and (boundp 'elot-robot-jar-path) elot-robot-jar-path (file-exists-p elot-robot-jar-path))
    (error "elot-robot-jar-path is not set or invalid: %s" elot-robot-jar-path))

  (let* ((output-file (concat (file-name-sans-extension omnfile) ".ttl"))
         ;; Base command arguments list (suitable for both helpers)
         (command-args (list "java" "-jar" elot-robot-jar-path
                             "convert" "-vvv" ; Keep verbose ROBOT output for parsing
                             "--input" omnfile
                             "--output" output-file)))

    (message "[elot-robot-omn-to-ttl] Target ttlfile: %s" output-file)

    ;; --- Dispatch based on mode ---
    (if noninteractive
        (elot-robot-omn-to-ttl--batch omnfile output-file command-args)
      (elot-robot-omn-to-ttl--interactive omnfile output-file command-args))))

(defun elot--parse-robot-error-location (text)
  "Extract (line column) from ROBOT error TEXT.  Return list of integers or nil."
  (when (string-match "Line \\([0-9]+\\) column \\([0-9]+\\)" text)
    (list (string-to-number (match-string 1 text))
          (string-to-number (match-string 2 text)))))

(defun elot--jump-to-omn-error (omnfile line col)
  "Open OMNFILE and move point to LINE and COL."
  (let ((buf (find-file-other-window omnfile)))
    (with-current-buffer buf
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (1- col))
      ;;(pulse-momentary-highlight-one-line (point))
      )))

(defun elot--jump-to-org-heading-for-identifier (omnfile line)
  "From OMNFILE and error LINE, search upward for a declaration.
  Jump to the Org-mode heading defining the identifier found."
  (let ((identifier nil))
    (save-excursion
      (with-current-buffer (find-file-noselect omnfile)
        (goto-char (point-min))
        (forward-line (1- line))
        (end-of-line)
        (when (re-search-backward "^[^ \t]" nil t)
          (let ((line-text (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))))
            (when (string-match "^\\([-A-Za-z]+\\):[ \t]+\\(.+\\)" line-text)
              (setq identifier (match-string 2 line-text)))))))
    (when (and identifier elot-last-org-source (file-exists-p elot-last-org-source))
      (let ((buf (find-file-other-window elot-last-org-source)))
        (with-current-buffer buf
          (goto-char (point-min))
          (if (re-search-forward
               (format "^\\(?:\\*+ .*\\b%s\\b\\|.*::.*%s\\)"
                       (regexp-quote identifier)
                       (regexp-quote identifier))
               nil t)
              (progn
                (beginning-of-line)
                ;;(pulse-momentary-highlight-one-line (point))
                ;;(message "Parse error traced to heading: %s" (match-string 0))
                )
            (message "Could not find Org heading for: %s" identifier)))))))

(defvar elot-last-org-source nil
  "Path to the last Org-mode file that generated an OMN file.")

(defun elot--remember-org-source ()
  "Remember the current Org file for use after tangling."
  (setq elot-last-org-source (buffer-file-name)))

(defun elot-tangled-omn-to-ttl ()
  "After tangling to OMN, call ROBOT to convert to Turtle."
  (let* ((omnfile (buffer-file-name))  ;; will run in the tangled buffer
         (omn-p (string-match-p ".omn$" omnfile)))
    (if omn-p
        (elot-robot-omn-to-ttl omnfile))))

(defun elot-parse-headline-hierarchy (tree)
  "Iteratively parse headlines from org-element TREE into a nested AST.
Creates a dummy root at level 0 to handle multiple top-level ontologies."
  (let* ((dummy-root (list :level 0 :title "ROOT" :descriptions nil :children nil))
         (stack (list dummy-root)))
    
    (org-element-map tree 'headline
      (lambda (hl)
        (let ((level (org-element-property :level hl)))
          ;; Pop the stack until the top node's level is strictly less than current level
          (while (>= (plist-get (car stack) :level) level)
            (pop stack))
          
          (let* ((title-raw (org-element-property :title hl))
                 (title (substring-no-properties 
                         (if (stringp title-raw)
                             title-raw
                           (org-element-interpret-data title-raw))))
                 ;; Extract the required properties:
                 (id           (org-element-property :ID hl))
                 (context-type (org-element-property :ELOT-CONTEXT-TYPE hl))
                 (context-localname (org-element-property :ELOT-CONTEXT-LOCALNAME hl))
                 (default-prefix (org-element-property :ELOT-DEFAULT-PREFIX hl))
                 (resourcedefs (org-element-property :RESOURCEDEFS hl))
                 (prefixdefs (org-element-property :PREFIXDEFS hl))
                 (header-args-omn (org-element-property :HEADER-ARGS:OMN hl))
                 (tangle-target-omn (when (and header-args-omn (string-match ":tangle[ \t]+\\([^ \t\n]+\\)" header-args-omn))
                                      (match-string 1 header-args-omn)))
                 ;; 1. Extract the position (for markers)
                 (begin-pos (org-element-property :begin hl))
                 ;; Extract URI and Label mirroring `elot-entities-with-plist'
                 ;; some headings are marked not to be included in OMN
                 (tags (org-element-property :tags hl))
                 (nodeclare (when (member "nodeclare" tags) "yes"))
                 (commented (org-element-property :commentedp hl))
                 ;; uri
                 (uri
                  (unless (or nodeclare commented)
                    (elot-entity-from-header title t))) ; t = noerror for wrapper headings
                 (label (if (string-match "\\(.+\\) (.*)" title)
                            (match-string 1 title) 
                          uri)) ; fallback to uri if no label matches
                 ;; Find ancestor with resourcedefs "yes" to determine rdf:type
                 (ancestor-resourcedefs (cl-find-if (lambda (n) (equal (plist-get n :resourcedefs) "yes")) stack))
                 (ancestor-id (and ancestor-resourcedefs (plist-get ancestor-resourcedefs :id)))
                 (rdf-type (if ancestor-id
                               (cond
                                ((string-suffix-p "-datatypes" ancestor-id) "rdfs:Datatype")
                                ((string-suffix-p "-class-hierarchy" ancestor-id) "owl:Class")
                                ((string-suffix-p "-object-property-hierarchy" ancestor-id) "owl:ObjectProperty")
                                ((string-suffix-p "-data-property-hierarchy" ancestor-id) "owl:DatatypeProperty")
                                ((string-suffix-p "-annotation-property-hierarchy" ancestor-id) "owl:AnnotationProperty")
                                ((string-suffix-p "-individuals" ancestor-id) "owl:NamedIndividual"))
                             (when (string-suffix-p "-ontology-declaration" id) "owl:Ontology")))
                 ;; Extract descriptions specific to this headline
                 (extracted-desc (elot--extract-headline-descriptions hl))
                 (desc (let ((d extracted-desc))
                         ;; Automatically add rdfs:label if it differs from the URI
                         (when (and uri label (not (equal label uri))
                                    (not (assoc "rdfs:label" d)))
                           (push (list "rdfs:label" label) d))
                         (if (and uri rdf-type)
                             (cons (list "rdf:type" rdf-type) d)
                           d)))
                 ;; Conditionally append relevant properties
                 (node (append
                        (list :level level
                              :title title
                              :marker (copy-marker begin-pos)
                              :tags tags)
                        (when tangle-target-omn
                          (list :tangle-target-omn tangle-target-omn))
                        (when (equal resourcedefs "yes")
                          (list :id id
                                :resourcedefs resourcedefs))
                        (when (equal context-type "ontology")
                          (list :elot-context-type context-type
                                :elot-context-localname context-localname
                                :elot-default-prefix default-prefix))
                        (when (or uri (not (or (equal resourcedefs "yes") (equal context-type "ontology"))))
                          (list :uri uri
                                :label label))
                        (when (equal prefixdefs "yes")
                          (append (list :prefixdefs prefixdefs)
                                  (let ((prefixes-alist (elot-get-prefixes-alist hl id)))
                                    (when prefixes-alist
                                      (list :prefixes prefixes-alist)))))
                        (list :descriptions desc
                              :children nil))))
            
            ;; Attach current node as child of the top node
            (let ((parent (car stack)))
              (plist-put parent :children (cons node (plist-get parent :children))))
            
            ;; Push current node onto stack
            (push node stack))))
      ;; NO-RECURSION is nil, so it maps depth-first over all headlines
      nil nil nil)

    ;; After parsing, reverse the children lists to restore document order
    (let ((root (car (last stack))))
      (elot--reverse-children root)
      root)))

(defun elot--reverse-children (node)
  "Recursively reverse the children of NODE in-place."
  (let ((children (plist-get node :children)))
    (when children
      (plist-put node :children (nreverse children))
      (dolist (child (plist-get node :children))
        (elot--reverse-children child)))))

(defun elot--extract-headline-descriptions (hl)
  "Extract description list items immediately under headline HL.
Does not recurse into child headlines, matching the output format of
`elot-org-subsection-descriptions` for meta-annotations with sublists."
  (org-element-map (org-element-contents hl) 'item
    (lambda (y)
      (when (org-element-property :tag y)
        (append
         (elot-org-elt-item-str y)
         (when (elot-org-elt-exists (org-element-contents y) 'item)
           (org-element-map (org-element-contents y) 'item
             (lambda (z)
               (when (elot-meta-annotation-tag-p (elot-org-elt-item-tag-str z))
                 (elot-org-elt-item-str z)))
             nil nil 'item)))))
    ;; Pass '(headline item) to prevent double-processing nested items
    nil nil '(headline item)))


(defun elot-get-prefixes-alist (headline-ast table-name)
  "Find TABLE-NAME under HEADLINE-AST and return its rows as an alist.
If TABLE-NAME is nil, return the first table found."
  (catch 'found
    (org-element-map headline-ast 'table
      (lambda (table)
        ;; Match the table name or fallback if table-name wasn't provided
        (when (or (null table-name)
                  (string= (org-element-property :name table) table-name))
          (save-excursion
            ;; :post-affiliated places point directly on the | row, bypassing #+name: etc.
            (goto-char (org-element-property :post-affiliated table))
            (let ((prefixes
                   (delq nil
                         (mapcar (lambda (row)
                                   (when (listp row)
                                     (cons (substring-no-properties (car row)) (substring-no-properties (cadr row)))))
                                 (org-table-to-lisp)))))
              (dolist (p prefixes)
                (let* ((key-raw (car p))
                       (val     (cdr p))
                       (key     (replace-regexp-in-string ":" "" key-raw)))
                  (unless (or (equal key-raw "prefix")
                              (assoc key org-link-abbrev-alist-local))
                    (push (cons key val) org-link-abbrev-alist-local))))
              (throw 'found prefixes))))))))

(defun elot-update-link-abbrev ()
  "Refresh `org-link-abbrev-alist-local' from `elot-headline-hierarchy'.
Scans the hierarchy for nodes with `:prefixdefs \"yes\"' and adds their
`:prefixes' to `org-link-abbrev-alist-local'. The first appearance wins."
  (setq-local org-link-abbrev-alist-local nil)
  (let ((stack (and elot-headline-hierarchy (list elot-headline-hierarchy)))
        (seen-prefixes (make-hash-table :test 'equal))
        (new-abbrevs nil))
    (while stack
      (let* ((node     (pop stack))
             (children (plist-get node :children)))
        (when (equal (plist-get node :prefixdefs) "yes")
          (dolist (p (plist-get node :prefixes))
            (let* ((key-raw (car p))
                   (val     (cdr p))
                   (key     (replace-regexp-in-string ":" "" key-raw)))
              (unless (or (equal key-raw "prefix")
                          (gethash key seen-prefixes))
                (puthash key t seen-prefixes)
                (push (cons key val) new-abbrevs)))))
        (setq stack (append children stack))))
    (setq-local org-link-abbrev-alist-local (nreverse new-abbrevs))))

(defun elot-sanity-check-prefixes ()
  "Ensure level 1 'Prefixes' headings have the required property drawer.
Silently adds :prefixdefs: yes if missing."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\* Prefixes" nil t)
      (let ((hl (org-element-at-point)))
        (when (and (eq (org-element-type hl) 'headline)
                   ;; assume we are at outline level 2
                   (= (org-element-property :level hl) 2)
                   (not (org-element-property :PREFIXDEFS hl)))
          (org-set-property "prefixdefs" "yes"))))))

(defvar elot-resource-section-alist
  '(("Datatypes"             . "-datatypes")
    ("Classes"               . "-class-hierarchy")
    ("Object properties"     . "-object-property-hierarchy")
    ("Data properties"       . "-data-property-hierarchy")
    ("Annotation properties" . "-annotation-property-hierarchy")
    ("Individuals"           . "-individuals"))
  "Alist mapping well-known heading names to their ID suffixes.")

(defun elot-sanity-check-resource-sections ()
  "Ensure well-known resource section headings have required property drawers.
For each heading named \"Classes\", \"Object properties\", etc., silently
adds :ID: and :resourcedefs: yes if missing.  The ID is constructed from
the nearest ontology context's localname (or top-level ID) plus the
conventional suffix.  Only the first match per name is patched."
  (save-excursion
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (entry elot-resource-section-alist)
        (let ((name (car entry))
              (suffix (cdr entry)))
          (goto-char (point-min))
          ;; Match level-2 headings with this name (case-insensitive)
          (while (re-search-forward
                  (format "^\\*\\*[ \t]+%s[ \t]*$" (regexp-quote name))
                  nil t)
            (unless (gethash name seen)
              (let ((hl (org-element-at-point)))
                (when (and (eq (org-element-type hl) 'headline)
                           (= (org-element-property :level hl) 2))
                  ;; Find the ontology context prefix for the ID
                  (let* ((parent-id
                          (or (org-entry-get-with-inheritance "ELOT-context-localname")
                              (org-entry-get-with-inheritance "ID")))
                         (expected-id (concat parent-id suffix)))
                    ;; Add :resourcedefs: yes if missing
                    (unless (string-equal
                             (org-element-property :RESOURCEDEFS hl) "yes")
                      (org-set-property "resourcedefs" "yes"))
                    ;; Add :ID: if missing
                    (unless (org-element-property :ID hl)
                      (org-set-property "ID" expected-id))
                    (puthash name t seen)))))))))))

(defvar-local elot-headline-hierarchy nil
  "Stores the parsed headline hierarchy for the current Elot buffer.")

(defun elot-update-headline-hierarchy ()
  "Update `elot-headline-hierarchy' from the current org buffer.
This uses `elot-parse-headline-hierarchy' on the parsed buffer elements."
  (interactive)
  (elot-sanity-check-prefixes)
  (elot-sanity-check-resource-sections)
  (let ((ast (org-element-parse-buffer)))
    (setq elot-headline-hierarchy
          (elot-parse-headline-hierarchy ast)))
  (elot-update-link-abbrev))

(defun elot-build-slurp (&optional hierarchy)
  "Build `elot-slurp` entries iteratively from HIERARCHY.
Defaults to `elot-headline-hierarchy`.
Returns a list of lists: (URI label (plist of attributes))."
  (let ((stack (list (or hierarchy (bound-and-true-p elot-headline-hierarchy))))
        (result nil))
    (while stack
      (let* ((node     (pop stack))
             (uri      (plist-get node :uri))
             (label    (plist-get node :label))
             (desc     (plist-get node :descriptions))
             (children (plist-get node :children)))
        
        ;; 1. Process the current node
        (when (and uri (stringp uri))
          (let* ((flat-desc (flatten-tree desc))
                 (display-label (or label uri))
                 (rdf-type nil)
                 (other-props nil)
                 (tail flat-desc))
            
            ;; Parse descriptions
            (while tail
              (let ((k (car tail))
                    (v (cadr tail)))
                (cond
                 ((equal k "rdf:type")   (setq rdf-type v))
                 ((equal k "rdfs:label") nil) ; Omit
                 (t
                  (push k other-props)
                  (push v other-props)))
                (setq tail (cddr tail))))
            
            ;; Build attrs
            (let ((attrs (list "rdfs:label" display-label)))
              (when rdf-type
                (setq attrs (nconc attrs (list "rdf:type" rdf-type))))
              (setq attrs (nconc attrs (nreverse other-props)))
              
              ;; O(1) accumulation
              (push (list uri display-label attrs) result))))
        
        ;; 2. Push children to stack (keeps depth-first pre-order)
        (when children
          (setq stack (append children stack)))))
          
    ;; Reverse once at the end for O(N) performance
    (nreverse result)))

(defvar-local elot-slurp nil
  "List of resources declared in an ELOT buffer.
Each member is a list of curie, label, and plist of attributes.")
(defvar elot-slurp-global nil
  "List of resources retrieved from SPARQL endpoints.")
(defvar-local elot-codelist-ht nil
  "Hashtable holding pairs of curie and label for ELOT label-display.")
(defvar-local elot-attriblist-ht nil
  "Hashtable holding pairs of curie and attribute plist for ELOT label-display.")

(defun elot--ht-from-plist (plist)
  "Build a hash-table from flat PLIST (key1 val1 key2 val2 ...).
Keys are compared with `equal'.  Pure built-in, no external deps."
  (let ((ht (make-hash-table :test 'equal)))
    (while plist
      (puthash (pop plist) (pop plist) ht))
    ht))

(defun elot--ht-from-alist (alist)
  "Build a hash-table from ALIST ((key . val) ...).
Keys are compared with `equal'.  Pure built-in, no external deps."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (puthash (car pair) (cdr pair) ht))
    ht))

(defun elot-slurp-to-vars ()
  "Read resources declared in ELOT buffer into local variables.
The variables are ELOT-SLURP (plist) and ELOT-CODELIST-HT,
ELOT-ATTRIBLIST-HT (hashtable).  Outside ELOT buffers, use ELOT-SLURP-GLOBAL."
  (let ((slurp (elot-build-slurp)))
    (setq elot-slurp (or slurp elot-slurp-global))
    (setq elot-codelist-ht
          (elot--ht-from-plist (elot-codelist-from-slurp
                                ;; only fontify what's locally declared
                                elot-slurp)))
    (setq elot-attriblist-ht
          (elot--ht-from-alist (elot-attriblist-from-slurp
                                ;; lookup includes the global list
                                (append slurp elot-slurp-global))))))

(defun elot-codelist-from-slurp (slurp)
  "Return a plist of the first two entries of each member of SLURP.
SLURP is a list of lists made with `elot-slurp-entities'.
The result is a plist of pairs of identifiers and labels to display."
  (mapcan (lambda (row) (list (car row) (cadr row))) slurp))

(defun elot-attriblist-from-slurp (slurp)
  "Return an alist mapping labels to plists of predicate--value pairs.
SLURP is a list of lists made with `elot-slurp-entities'.
The identifier (puri) of the resource is added to the plist with key \"puri\"."
  (let (result)
    (dolist (row slurp (nreverse result))
      ;; (nth 1 row) is the label. 
      ;; The rest becomes the cdr of the alist entry (the property list).
      (push (cons (nth 1 row)
                  (cons "puri" (cons (nth 0 row) (nth 2 row))))
            result))))

(defun elot-omn-prefix-block (ontology-node)
  "Generate an OMN prefix block string from an ONTOLOGY-NODE.
Searches for a descendant node with `:prefixdefs \"yes\"' and formats
its `:prefixes' alist for OWL Manchester Syntax."
  (let ((prefix-node nil))
    ;; Recursively search for the node containing the prefix definitions
    (cl-labels ((search-node (node)
                  (if (equal (plist-get node :prefixdefs) "yes")
                      (setq prefix-node node)
                    (mapc #'search-node (plist-get node :children)))))
      (search-node ontology-node))
    
    (when prefix-node
      (let* ((prefixes (plist-get prefix-node :prefixes))
             ;; Ignore the table header ("prefix" . "uri") if present
             (clean-prefixes (if (equal (car prefixes) '("prefix" . "uri"))
                                 (cdr prefixes)
                               prefixes)))
        (concat "## Prefixes\n"
                (mapconcat (lambda (row)
                             (let ((prefix (car row))
                                   (uri (cdr row)))
                               ;; Ensure prefix always ends with a colon
                               (format "Prefix: %-5s <%s>"
                                       (if (string-suffix-p ":" prefix) prefix (concat prefix ":"))
                                       uri)))
                           clean-prefixes
                           "\n"))))))

(defun elot-omn-format-annotations (l indent-level)
  "Format list of annotations L with INDENT-LEVEL spaces.
Recursively handles meta-annotations without writing `Annotations:`."
  (let ((ind (make-string indent-level ?\s)))
    (mapconcat (lambda (y)
                 (let ((key (car y))
                       (val (cadr y))
                       (meta (cddr y)))
                   (concat
                    (if meta
                        (concat ind "Annotations: \n"
                                (elot-omn-format-annotations meta (+ indent-level 4))
                                "\n")
                      "")
                    ind key " " (string-trim-left (elot-annotation-string-or-uri val)))))
               l
               ",\n")))

(defun elot-omn-format-restrictions (l indent-level)
  "Format list of restrictions L with INDENT-LEVEL spaces.
Includes the property keyword (e.g., `SubClassOf:`)."
  (let ((ind (make-string indent-level ?\s)))
    (mapconcat (lambda (y)
                 (let* ((key (car y))
                        (val (cadr y))
                        (meta (cddr y))
                        (formatted-val (if (member key elot-omn-all-keywords)
                                           val
                                         (string-trim-left (elot-annotation-string-or-uri val)))))
                   (if meta
                       (concat ind key ": \n"
                               (make-string (+ indent-level 4) ?\s) "Annotations: \n"
                               (elot-omn-format-annotations meta (+ indent-level 8))
                               "\n"
                               (make-string (+ indent-level 4) ?\s) formatted-val)
                     (concat ind key ": " formatted-val))))
               l
               "\n")))

(defun elot-omn-resource-frame (node parent-uri)
  "Generate a resource frame string for NODE.
Uses PARENT-URI to automatically emit taxonomy axioms.
Returns nil if NODE does not define a resource or is tagged :nodeclare:."
  (let* ((uri (plist-get node :uri))
         (desc (plist-get node :descriptions))
         (type (cadr (assoc "rdf:type" desc)))
         (tags (plist-get node :tags))
         (annotations nil)
         (restrictions nil))
    (when (and uri (stringp uri) (not (member "nodeclare" tags)))
      ;; 1. Partition descriptions into restrictions and annotations
      (dolist (y desc)
        (let ((k (car y)))
          (cond
           ((member k elot-omn-property-keywords)
            (push y restrictions))
           ((member k elot-omn-misc-keywords) nil) ;; Ignored here, handled by misc-frames
           ((equal k "rdf:type") nil)
           (t
            (push y annotations)))))
      
      ;; 2. Add inferred taxonomy
      (when parent-uri
        (cond
         ((equal type "owl:Class")
          (push (list "SubClassOf" parent-uri) restrictions))
         ((and type (string-match-p "Property$" type))
          (push (list "SubPropertyOf" parent-uri) restrictions))))
      
      ;; 3. Build the strictly-formatted OMN frame string
      (let* ((omn-type (cond
                        ((equal type "owl:Class") "Class")
                        ((equal type "owl:ObjectProperty") "ObjectProperty")
                        ((equal type "owl:DatatypeProperty") "DataProperty")
                        ((equal type "owl:AnnotationProperty") "AnnotationProperty")
                        ((equal type "owl:NamedIndividual") "Individual")
                        ((equal type "owl:Ontology") "Ontology")
                        ((equal type "rdfs:Datatype") "Datatype")
                        (type (replace-regexp-in-string "^.*:" "" type))
                        (t "Class")))
             (frame (list (format "%s: %s" omn-type uri))))
        
        (when annotations
          (push (concat "    Annotations: \n" 
                        (elot-omn-format-annotations (nreverse annotations) 8)) 
                frame))
        
        (when restrictions
          (push (elot-omn-format-restrictions (nreverse restrictions) 4) frame))
        
        ;; Join frame internals with a single newline (no blank lines inside)
        (mapconcat 'identity (nreverse frame) "\n")))))

(defun elot-omn-misc-frames (node)
  "Generate a list of formatted misc frame strings from NODE's descriptions.
These represent top-level standalone axioms like DisjointClasses."
  (let ((desc (plist-get node :descriptions))
        (misc-frames nil))
    (dolist (y desc)
      (when (member (car y) elot-omn-misc-keywords)
        ;; elot-omn-format-restrictions with indent=0 works perfectly for top-level misc frames
        (push (elot-omn-format-restrictions (list y) 0) misc-frames)))
    (nreverse misc-frames)))

(defun elot-omn-resource-declarations (nodes &optional parent-uri)
  "Recursively traverse AST NODES to generate OMN syntax frames.
Nodes are property lists from `elot-headline-hierarchy`.
Uses PARENT-URI to automatically emit taxonomy axioms."
  (let ((frames nil))
    (dolist (node nodes)
      (let ((uri (plist-get node :uri))
            (children (plist-get node :children)))
        
        ;; 1. Try to generate a resource frame
        (let ((res-frame (elot-omn-resource-frame node parent-uri)))
          (when res-frame
            (push res-frame frames)))
            
        ;; 2. Try to generate misc frames (independent of resource frame)
        (let ((m-frames (elot-omn-misc-frames node)))
          (dolist (m m-frames)
            (push m frames)))
        
        ;; 3. Recurse into children
        (when children
          (let ((child-frames (elot-omn-resource-declarations children (if (and uri (stringp uri)) uri parent-uri))))
            (when (not (string-empty-p child-frames))
              (push child-frames frames))))))
    
    ;; Join adjacent frames with a single blank line
    (mapconcat 'identity (nreverse frames) "\n\n")))

(defun elot-get-ontology-node-omn (node)
    "Return the OMN content for the given ONTOLOGY-NODE."
    (let* ((prefix-block (elot-omn-prefix-block node))
           (resources (elot-omn-resource-declarations (list node))))
      (concat (or prefix-block "")
              (if (and prefix-block (not (string-empty-p resources))) "\n\n" "")
              resources)))

(defun elot-tangle-buffer-to-omn ()
  "Update hierarchy and export OMN for all ontologies to their tangle targets."
  (interactive)
  (elot-update-headline-hierarchy)
  (let ((ontology-nodes (plist-get elot-headline-hierarchy :children))
        (omn-tangle-blocks (org-babel-tangle-collect-blocks "omn"))
        (abbrev-save org-link-abbrev-alist-local)
        (omt-save org-macro-templates))
    ;; First tangle omn blocks. This writes the tangled contents to disk.
    (org-babel-tangle t t "omn")
    (dolist (node ontology-nodes)
      (when-let ((target (plist-get node :tangle-target-omn)))
        (let ((target-full (expand-file-name target)))
          (with-temp-file target-full
            ;; restore abbrev and macro definitions
            (setq-local org-link-abbrev-alist-local abbrev-save)
            (setq-local org-macro-templates omt-save)
            ;; 1. Insert the ontology node string (ontology from headings, main output)
            (insert (elot-get-ontology-node-omn node))
            ;; 2. If standard tangled blocks exist for this file...
            (when (assoc-string target-full omn-tangle-blocks)
              (message "insert result of tangle from: %s" target-full)
              ;; Ensure we are on a new line before appending
              (unless (bolp) (insert "\n\n#\n# Tangle blocks\n#\n"))
              ;; Slurp the contents that `org-babel-tangle` just wrote to disk
              (when (file-exists-p target-full)
                (let ((start (point)))
                  (insert-file-contents target-full)
                  ;; insert-file-contents leaves point at `start`, so we move to the end
                  (goto-char (point-max))))))
          ;; If the .omn file is already open in a buffer, silently revert it
          ;; so Emacs won't prompt "file changed on disk; really edit the buffer?"
          (let ((buf (find-buffer-visiting target-full)))
            (when buf
              (with-current-buffer buf
                (revert-buffer t t t))))
          (message "Tangled OMN outline to %s" target)
          (elot-robot-omn-to-ttl target-full))))))

(provide 'elot-tangle)
;;; elot-tangle.el ends here
