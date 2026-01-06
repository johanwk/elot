;;; elot.el --- Emacs Literate Ontology Tool (ELOT)   -*- lexical-binding: t; no-native-compile: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 1.0.6
;; Package-Requires: ((emacs "29.1") (htmlize "1.58") (ht "2.3") (omn-mode "1.3") (hydra "0.15.0") (sparql-mode "4.0.2"))
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

;; This package is for authoring OWL ontologies using org-mode.

;; To start an ontology from scratch using ELOT, open an Org file and
;; use predefined "tempo" templates.

;;  - insert `<odh' and hit <Tab> to add a header for the document
;;  - insert `<ods' and hit <Tab> to insert headers for the ontology,
;;    classes, properties, and individuals.

;; Shift-<F5> will open a "hydra" menu with more templates and
;; functions for exporting to an ontology file or HTML.

;; Please consult the package Github site for more information:
;;        <https://github.com/johanwk/elot>

;;; Code:

;; [[file:../elot-defs.org::src-require][src-require]]
(require 'ob-lob) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions
(require 'org-tempo) ; document templates
(require 'htmlize) ; fontify blocks
(require 'omn-mode) ; OWL Manchester Syntax (OMN) support
(require 'sparql-mode) ; SPARQL
(require 'ob-sparql) ; SPARQL in org-babel
(require 'ob-plantuml) ; PlantUML in org-babel
(require 'hydra) ; hydra menu
(require 'ht) ; hashtable, for label display
(require 'url) ; for opening online ontologies
(require 'url-http) ; for opening online ontologies
(require 'xref) ; jump around
(require 'button) ; for text‑buttons
(require 'help-mode) ; nice keymap & look
;; src-require ends here

;; [[file:../elot-defs.org::src-settings-externals][src-settings-externals]]
(defgroup elot
  nil
  "Customization group for Emacs Literate Ontology Tool (ELOT)."
  :prefix "elot-"
  :group 'elot)
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
(defvar elot-last-org-source nil
  "Path to the last Org-mode file that generated an OMN file.")
(defun elot--remember-org-source ()
  "Remember the current Org file for use after tangling."
  (setq elot-last-org-source (buffer-file-name)))
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
     (lambda (proc event)
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
(defun elot-tangled-omn-to-ttl ()
  "After tangling to OMN, call ROBOT to convert to Turtle."
  (let* ((omnfile (buffer-file-name))  ;; will run in the tangled buffer
         (omn-p (string-match-p ".omn$" omnfile)))
    (if omn-p
        (elot-robot-omn-to-ttl omnfile))))
(defcustom elot-default-image-path "./images/"
  "ELOT default output directory for generated images."
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
  "Default options for rdfpuml."
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
  "Command to execute rdfpuml to generate diagram from TTL-FILE."
  (shell-command (concat elot-rdfpuml-command-str " " ttl-file)))
;; src-settings-externals ends here

;; [[file:../elot-defs.org::*Open existing OWL files or online ontologies][Open existing OWL files or online ontologies:1]]
(defun elot-open-owl (owl-source)
  "Open an OWL ontology from OWL-SOURCE by converting with `elot-exporter'.
OWL-SOURCE can be a local file or a URL.  If a URL is provided, the
function requests the ontology using content negotiation, preferring
Turtle, RDF/XML, N3, JSON-LD, OWL Functional Syntax, or Manchester
Syntax.  The output is captured into a buffer named after the converted
file, with `.org' as the extension."
  (interactive "sEnter OWL file path or URL: ")
  (let* ((is-url (string-match-p "\\`https?://" owl-source))
         (local-file (if is-url
                         (let ((temp-file (make-temp-file "elot-ontology-" nil ".owl")))
                           (elot-download-ontology owl-source temp-file)
                           temp-file)
                       owl-source))
         (output-buffer-name (concat (file-name-sans-extension (file-name-nondirectory local-file)) ".org"))
         (output-buffer (get-buffer-create output-buffer-name))
         (command (concat elot-exporter-command-str " " (shell-quote-argument local-file))))
    (with-current-buffer output-buffer
      (erase-buffer)
      (shell-command command output-buffer)
      (org-mode))
    (pop-to-buffer output-buffer)
    (when is-url
      (delete-file local-file))))  ;; Clean up temp file after conversion

(defun elot-download-ontology (url dest-file)
  "Download an ontology from URL with content negotiation, save it to DEST-FILE.
Requests the ontology in the best available format: Turtle, RDF/XML, N3,
JSON-LD, OWL Functional Syntax, or Manchester Syntax."
  (let ((url-request-extra-headers
         '(("Accept" . "text/turtle, application/rdf+xml, text/n3, application/ld+json, text/owl-functional, text/owl-manchester; q=0.9"))))
    (url-copy-file url dest-file t)))
;; Open existing OWL files or online ontologies:1 ends here

;; [[file:../elot-defs.org::src-owl-builtins][src-owl-builtins]]
(defvar elot-owl-builtin-resources
  '("owl:Thing" "owl:Nothing" "xsd:string" "xsd:boolean" "xsd:decimal" "xsd:integer"
    "xsd:float" "xsd:double" "xsd:dateTime" "xsd:time" "xsd:date" "xsd:gYear"
    "xsd:gMonth" "xsd:gDay" "xsd:gYearMonth" "xsd:gMonthDay" "xsd:hexBinary"
    "xsd:base64Binary" "xsd:anyURI" "xsd:normalizedString" "xsd:token" "xsd:language"
    "xsd:Name" "xsd:NCName" "xsd:NMTOKEN" "rdf:PlainLiteral")
  "List of built-in OWL and XSD resources that are always considered known.")
;; src-owl-builtins ends here

;; [[file:../elot-defs.org::src-omn-keywords][src-omn-keywords]]
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
;; src-omn-keywords ends here

;; [[file:../elot-defs.org::src-omn-latex-tt][src-omn-latex-tt]]
(defun elot-latex-filter-omn-item (text backend info)
  "Format OWL Manchester Syntax content TEXT in description lists.
Target output type BACKEND
The context INFO is ignored."
  (progn
    (always info) ;; ignore this argument
    (when (org-export-derived-backend-p backend 'latex)
      (when (seq-some
             (lambda (x)
               (string-match (concat "^\\\\item\\[{" x "}\\]") text))
             elot-omn-property-keywords)
        ;; make the description term texttt
        (setq text (replace-regexp-in-string
                    "\\\\item\\[{\\([-a-zA-Z]+\\)}\\]"
                    "\\\\item[\\\\normalfont\\\\ttfamily\\\\small \\1]"
                    text))
        ;; make the list entry content omn inline code unless it's a url
        (if (not (string-match "\\\\url{.*}$" text))
            (replace-regexp-in-string
             "^\\(.*\\] \\)\\(.*\\)"
             "\\1\\\\lstinline[language=omn]{\\2}"
             text)
          text)))))


(add-to-list 'org-export-filter-item-functions
             'elot-latex-filter-omn-item)
;; src-omn-latex-tt ends here

;; [[file:../elot-defs.org::src-context-info][src-context-info]]
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
;; src-context-info ends here

;; [[file:../elot-defs.org::src-looking-at][src-looking-at]]
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
;; src-looking-at ends here

;; [[file:../elot-defs.org::src-desc-lists][src-desc-lists]]
(defun elot-org-elt-exists (x elt)
  "Return a list of elements of type ELT extracted from X.
Uses `org-element-map` to collect matching elements.
The function is used to check whether the list contains ELT."
  (org-element-map x elt #'identity))
(defun elot-org-elt-item-tag-str (x)
  "For an item X in an `org-element-map', return the item tag."
  (if (org-element-property :tag x)
      (substring-no-properties (org-element-interpret-data (org-element-property :tag x)))))
(defun elot-org-elt-item-pars-str (x)
  "For an item X in an `org-element-map', return the paragraphs as one string."
  (string-join
   (split-string
    (string-trim (apply #'concat
                     (org-element-map x '(paragraph plain-list)
                       (lambda (y) (substring-no-properties
                                    (org-element-interpret-data y)))
                       nil nil 'plain-list)))
    "[ \t]*\n[ \t]*" nil)
   ":newline: "))
(defun elot-org-elt-item-str (x)
  "For X in an `org-element-map', return pair of strings (tag, paragraph content)."
  (list (elot-org-elt-item-tag-str x) (elot-org-elt-item-pars-str x)))
(defun elot-org-descriptions-in-section-helper ()
  "Return all description list items as pairs in a list.
This function is called from `elot-org-descriptions-in-section' after
narrowing to a description list under a heading."
  (org-element-map (org-element-parse-buffer) 'item
    (lambda (y) (if (org-element-property :tag y)
                    (append (elot-org-elt-item-str y)
                            (if (elot-org-elt-exists (cdr y) 'item)
                                (org-element-map (cdr y) 'item
                                  (lambda (z) (if (org-element-property :tag z)
                                                  (elot-org-elt-item-str z))) nil nil 'item)))))
    nil nil 'item))

(defun elot-org-descriptions-in-section ()
  "Return any description list items in current section as a list of strings."
  (interactive)
  ;; narrow our area of interest to the current section, before any subsection
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
            ;; return all paragraphs--description items as pairs in a list
            (elot-org-descriptions-in-section-helper)))))))

(defun elot-org-subsection-descriptions ()
  "Return a plist mapping subsection headlines to description lists.
This function collects headlines in the current subtree and associates
each with a plist of description-list items and values.  Sections with
the tag `nodeclare' or with headings starting with `COMMENT' are excluded.
The function does not include the section that has the target property ID,
unless it is an ontology section."
  (save-restriction
    (save-excursion
      (unless (org-at-heading-p) (org-previous-visible-heading 1)) ; ensure we are at a heading
      (org-narrow-to-subtree)
      (if ;; don't include the section that has the target property id itself, except if ontology section
          (or (outline-next-heading)
            (elot-at-ontology-heading))
          (let (ret)
            (while (let ((heading (substring-no-properties (org-get-heading nil t)))
                         (descriptions (elot-org-descriptions-in-section)))
                     (unless (or (string-match-p "^COMMENT" heading)
                                 (member "nodeclare" (org-get-tags (point) t)))
                       (setq ret
                             (cons
                              (if descriptions
                                  (list heading descriptions)
                                (list heading))
                              ret)))
                     (outline-next-heading)))
            (nreverse ret))))))
;; src-desc-lists ends here

;; [[file:../elot-defs.org::src-puri-expand][src-puri-expand]]
(defconst elot-puri-re 
  "^\\([-[:word:]_./]*\\):\\([-[:word:]_./]*\\)$")
  ;; "^\\([-_./[:alnum:]]*\\):\\([-_/.[:alnum:]]*\\)$")

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
  ;; maybe this entry contains string representation of meta-annotations, remove them
  (setq str (replace-regexp-in-string "\\(:newline:\\)? - [^ ]+ ::.*$" "" str))
  ;; maybe there's macros in the string, expand them
  (if (string-match "{{{.+}}}" str)
      (let ((omt org-macro-templates))
        (with-temp-buffer (org-mode)
                          (insert str) (org-macro-replace-all omt)
                          (setq str (buffer-string)))))
  (cond (; a number -- return the string
         (string-match "^[[:digit:]]+[.]?[[:digit:]]*$" str)
         (concat "  " str))
        (; a bare URI, which org-mode wraps in double brackets -- wrap in angles
         (string-match "^[[][[]\\(http[^ ]*\\)[]][]]$" str)
         (concat "  <" (match-string 1 str) ">"))
        (; a bare URI, but no double brackets -- wrap in angles
         (string-match "^\\(http[^ ]*\\)$" str)
         (concat "  <" (match-string 1 str) ">"))
        (; a bare URI, in angles
         (string-match "^\\(<http[^ ]*>\\)$" str)
         (concat "  " (match-string 1 str)))
        (; a bare URN, in angles
         (string-match "^\\(<urn:[^>]+>\\)$" str)
         (concat "  " (match-string 1 str)))
        (; a URN without angles, explicitly treat as xsd:string
         (string-match "^\\(urn:uuid[^ ]+\\)$" str)
         (concat "  \"" (match-string 1 str) "\"^^xsd:string"))
        (; true -- make it an explicit boolean
         (string-match "^true$" str) " \"true\"^^xsd:boolean")
        (; false -- make it an explicit boolean
         (string-match "^false$" str) " \"false\"^^xsd:boolean")
        (; string with datatype -- return unchanged
         (string-match "^\".*\"\\^\\^[-_[:alnum:]]*:[-_[:alnum:]]+$" str)
         (concat "  " str))
        (; not a puri -- normal string, wrap in quotes
         (equal str (elot-unprefix-uri str org-link-abbrev-alist-local))
         ;; if a language tag @en is present, return unchanged
         (replace-regexp-in-string ":newline:" ""
                                   (replace-regexp-in-string
                                    ":newline: " "\n"
                                    (if (string-match "\"\\(.*\n\\)*.*\"@[a-z]+" str)
                                        (concat " " str)
                                      ;; escape all quotes with \", note this gives invalid results if some are already escaped
                                      (concat "  \"" (replace-regexp-in-string "\"" "\\\\\"" str) "\"")))))
        (; else, a puri -- wrap in angles
         t (concat "  " (elot-unprefix-uri str org-link-abbrev-alist-local :noerror)))))

(defun elot-omn-restriction-string (str)
 "STR is wanted as an OMN value.  Strip any meta-annotations, or return unchanged."
 (setq str (replace-regexp-in-string " - [^ ]+ ::.*$" "" str))
 (replace-regexp-in-string ":newline:" "\n" str))
;; src-puri-expand ends here

;; [[file:../elot-defs.org::src-heading-to-list][src-heading-to-list]]
; http://stackoverflow.com/questions/17179911/emacs-org-mode-tree-to-list
(defun elot-org-list-siblings ()
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
                         (append (save-excursion
                                         (when (org-goto-first-child)
                                           (elot-org-list-siblings))) ret)
                       (cons (append (list
                                        ; the nil t arguments for tags yes, todos no, todos no, priorities no
                                        (substring-no-properties (org-get-heading nil t t t)))
                                       (save-excursion
                                         (when (org-goto-first-child)
                                           (elot-org-list-siblings))))
                               ret))))
             (org-goto-sibling)))
    (nreverse ret)))

(defun elot-entity-from-header (str &optional noerror)
  "Given a heading text STR, return the identifier it declares.

The returned value is either
  - a CURIE (e.g. \"ex:Apple\"), or
  - a full URI wrapped in \"<>\" (e.g. \"<http://example.org/Apple>\"), or
  - a composite string like \"ex:Ont <http://…/0.9>\" for ontology/version
    pairs.

If the heading contains *no* recognisable identifier and NOERROR is
non-nil, return NIL.  Otherwise raise an error."
  (let* ((curie-regex "[-_./[:alnum:]]*:[-_/.[:alnum:]]*") ;; review and update
         (full-uri-regex "http[s]?://[-[:alnum:]._~:/?#\\@!$&'()*+,;=%]*"))
    (cond
     ;; single URI, beginning of line
     ((string-match (format "^<?\\(%s\\)>?" full-uri-regex) str)
      (format "<%s>" (match-string 1 str)))
     ;; single URI in parentheses
     ((string-match (format "(<?\\(%s\\)>?)" full-uri-regex) str)
      (format "<%s>" (match-string 1 str)))
     ;; CURIE, then URI in parentheses (ontology and ontology version)
     ((string-match (format "(\\(%s\\) <?\\(%s\\)>?)" curie-regex full-uri-regex) str)
      (format "%s <%s>" (match-string 1 str) (match-string 2 str)))
     ;; two URIs in parentheses (ontology and ontology version)
     ((string-match (format "(<?\\(%s\\)>? <?\\(%s\\)>?)" full-uri-regex full-uri-regex) str)
      (let ((uri1 (match-string 1 str))
            (uri2 (match-string 2 str)))
        (format "<%s> <%s>" uri1 uri2)))
     ;; CURIE, beginning of line
     ((string-match (format "^\\(%s\\)" curie-regex) str)
      (match-string 1 str))
     ;; CURIE in parentheses
     ((string-match (format "(\\(%s\\))" curie-regex) str)
      (match-string 1 str))
     ;; URN identifier: return as-is if the string is a URN, e.g. <urn:isbn:0943396611>
     ((string-match "^<urn:[^>]+>$" str) str)
     ;; URN in parentheses
     ((string-match "(\\(<urn:[^>]+>\\))" str)
      (match-string 1 str))
     ;; two CURIEs in parentheses (ontology and ontology version)
     ((string-match (format "(\\(%s\\) \\(%s\\))" curie-regex curie-regex) str)
      (format "%s %s" (match-string 1 str) (match-string 2 str)))
     (t
      (if noerror
          nil
        (error "Fail! Heading \"%s\" in %s is not well-formed"
               str
               (org-entry-get-with-inheritance "ID")))))))
;; src-heading-to-list ends here

;; [[file:../elot-defs.org::src-resource-declare][src-resource-declare]]
(defun elot-omn-declare (str owl-type)
  "Declare entity from header content STR as an OWL-TYPE, in Manchester Syntax.
Add rdfs:label annotation.  If the identifier is inside parentheses, use
that as resource id."
  ;; check whether we have a label and a resource in parentheses
  (let* ((suri (elot-entity-from-header str)))
    (concat owl-type ": " suri)))

(defun elot-annotation-entries (l &optional sep)
  "Return a list of puri--string pairs, with optional meta-annotations.
L is a list of puri--string pairs, each perhaps with a trailing list of
similar, meta-annotation pairs.  SEP is a number used to build a string
of spaces for line indentation.  Ensures consistent spacing in formatted
output."
  (let ((indent (make-string (if sep (* 2 sep) 6) ?\ ))
        ;; l-uri-entries is the description list after purging any
        ;; items that have a prefix that isn't included as a LINK
        ;; entry, which goes into org-link-abbrev-alist-local. Note
        ;; that expanded URIs in brackets <...> are let through.
        (l-uri-entries
         (cl-remove-if (lambda (x) (string-equal (car x)
                                                 (elot-unprefix-uri (car x) org-link-abbrev-alist-local)))
                       l)))
    (if (atom l) "\n"
      (concat "\n" indent "Annotations: "
              (mapconcat (lambda (y)
                           (concat
                            (if (consp (caddr y)) ; we have meta-annotations
                                (concat (elot-annotation-entries (cddr y) 4) "\n " indent))
                            (car y)
                            (elot-annotation-string-or-uri (cadr y))))
                         l-uri-entries
                         (concat ",\n " indent))))))

(defun elot-restriction-entries (l)
  "Write Manchester Syntax restrictions.  L is a list of puri--string pairs.
Add annotations on the restriction axioms if present.
Special treatment for `Import' on an ontology resource."
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
                                (concat (elot-annotation-entries (cddr y) 4) "\n " indent))
                            (if (string-equal (car y) "Import") ; ontology import special case
                                (elot-annotation-string-or-uri (cadr y))
                              (elot-omn-restriction-string (cadr y)))))
                         l-omn-entries
                         (concat "\n" indent))))))

(defun elot-omn-annotate (l)
  "Add annotations to the first element of L, which is an org heading string.
This is a helper function for `elot-resource-declarations'."
  (let* ((str (car l))
         (suri (elot-entity-from-header str))
         (prefix (if (string-match "\\(.*\\):\\(.*\\)" suri)
                    (match-string 1 suri) ""))
         (localname (match-string 2 suri))
         (label (if (string-match "\\(.+\\) (.*)" str)
                    (match-string 1 str) nil))
         (resource-annotations
          (if label 
              (cons (list "rdfs:label" label) (cadr l)) 
            (cadr l))))
    (elot-annotation-entries resource-annotations)))

(defun elot-omn-restrict (l)
  "Retrieve restriction axioms from the second element of L.
This is a helper function for `elot-resource-declarations'."
  (elot-restriction-entries (cadr l)))

(defun elot-resource-declarations (l owl-type)
  "For list L of identifiers with annotations, declare to be of OWL-TYPE."
  (mapconcat
   (lambda (x)
     (concat
      (elot-omn-declare (car x) owl-type)
      ;; if annotations, add to the annotation block that has been started with rdfs:label
      (elot-omn-annotate x)
      (elot-omn-restrict x)))
   l "\n"))

(defun elot-format-misc-axiom-annotations (keyword input)
  "Format a string with annotations inline using ELOT-style annotation block."
  (let* ((main-part (car (split-string input " - "))) ; get the part before any annotations
         (annotations (cdr (split-string input " - ")))
         (pairs (mapcar (lambda (ann)
                          (when (string-match "\\`\\(.+\\)::\\(.*\\)\\'" (string-trim ann))
                            (cons (string-trim (match-string 1 ann))
                                  (string-trim (match-string 2 ann)))))
                        annotations))
         (valid-pairs (delq nil pairs)))
    (concat
     (when valid-pairs
       (if (string= keyword "Rule")
           (message "# WARNING: Rule annotations are not supported in Manchester Syntax\n    ")
         (concat " Annotations: "
                 (mapconcat (lambda (pair)
                              (format "%s \"%s\"" (car pair) (cdr pair)))
                            valid-pairs
                            ",\n       ")
                 "\n  ")))
         main-part)))

(defun elot-misc-axioms ()
  "Output OMN axioms for `elot-omn-misc-keywords' in buffer.
These are axioms not tied to a single resource.
If no axioms are found, return nil."
  (save-restriction
    (org-narrow-to-subtree)
    (let ((misc-axioms
           (mapconcat
            (lambda (l)
              (concat (car l) ": "
                      (elot-format-misc-axiom-annotations
                       (car l)
                       (replace-regexp-in-string ":newline:" "" (cadr l)))))
            (org-element-map (org-element-parse-buffer) 'item
              (lambda (item)
                (let* ((pair (elot-org-elt-item-str item))
                       (tag (car pair)))
                  (if (member tag elot-omn-misc-keywords)
                      pair)))
              nil nil)
            "\n")))
      (unless (string-empty-p misc-axioms)
        misc-axioms))))

(defun elot-resource-declarations-from-header (header-id owl-type)
  "Output OMN declarations for Class, Property, or Individual Org trees.
This function is called from the `org-babel' block in file
`elot-lob.org' named `resource-declarations'.

This function does not output subclass or subproperty axioms, as these
are handled by function `elot-resource-taxonomy-from-header'.

HEADER-ID is an org location id, OWL-TYPE is `Class', `ObjectProperty',
`DataProperty', `AnnotationProperty', `Individual', or `Datatype'.

The org location id, embedded in the `PROPERTIES' drawer for each OWL
resource type, is `<ontology>-class-hierarchy' for the Class outline,
and accordingly for `object-property', `data-property', and
`annotation-property'; for individuals, `<ontology>-individuals'."
  (save-excursion
    (elot-org-link-search header-id)
    (let ((entity-l (elot-org-subsection-descriptions))
          (misc-axioms (elot-misc-axioms)))
      (if (or entity-l misc-axioms (string= owl-type "Ontology"))
          (string-join
           (list
            (elot-resource-declarations entity-l owl-type)
            (if misc-axioms
                (concat "\n\n#### Miscellaneous axioms under " owl-type " declarations\n"))
            misc-axioms))
        "## (none)"))))
;; src-resource-declare ends here

;; [[file:../elot-defs.org::src-prefix-links][src-prefix-links]]
(defun elot-update-link-abbrev ()
  "Refresh `org-link-abbrev-alist-local' from current buffer prefixes table."
  (if (save-excursion (goto-char (point-min))
                      (re-search-forward "^#[+]name: prefix-table$" nil t))
      (setq-local org-link-abbrev-alist-local
                  (mapcar (lambda (x)
                            (cons (replace-regexp-in-string ":" "" (car x)) (cadr x)))
          (cl-remove 'hline (org-babel-ref-resolve "prefix-table"))))))
;; src-prefix-links ends here

;; [[file:../elot-defs.org::src-prefix-blocks][src-prefix-blocks]]
(defun elot-prefix-block-from-alist (prefixes format)
  "Return a prefix block from PREFIXES for use with filetype FORMAT.
PREFIXES is an alist of prefixes, from an Org table or
the standard ORG-LINK-ABBREV-ALIST or ORG-LINK-ABBREV-ALIST-LOCAL.
FORMAT is a symbol, either `omn', `sparql', or `ttl'."
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
;; src-prefix-blocks ends here

;; [[file:../elot-defs.org::src-robot-query][src-robot-query]]
(defun elot-robot-execute-query (query inputfile format)
  "Execute SPARQL query QUERY with ROBOT on ontology file INPUTFILE.
Result FORMAT is tabular `csv', or Turtle RDF `ttl'."
  (let* ((query-file
          (concat (org-babel-temp-directory) "/"
                  (file-name-base inputfile)
                  ".sparql"))
         (result-file
          (concat (file-name-sans-extension inputfile) (symbol-name format))))
    (with-temp-file query-file (insert query))
    (elot-robot-command
     (concat "query --input " inputfile
             " --format " (symbol-name format)
             " --query " query-file
             " " result-file))
    (insert-file-contents result-file)))
;; src-robot-query ends here

;; [[file:../elot-defs.org::src-sparql-exec-patch][src-sparql-exec-patch]]
(defun elot--is-elot-buffer ()
  "Check if the current buffer is an ELOT buffer."
  (bound-and-true-p elot-buffer-p))

(defun elot--custom-org-babel-execute-sparql (orig-fun &rest args)
  "ELOT-specific SPARQL execution with support for ROBOT.
This function is used to provide `advice' around
`org-babel-execute:sparql'.  ORIG-FUN and ARGS serve to invoke the
unchanged function, defined in `ob-sparql.el', when not called from an
ELOT buffer."
  (if (elot--is-elot-buffer)
      (progn
        (message "Executing a SPARQL query block with ELOT version of org-babel-execute:sparql.")
        (let* ((body (nth 0 args))
               (params (nth 1 args))
               (url (cdr (assoc :url params)))
               (format (cdr (assoc :format params)))
               (query (org-babel-expand-body:sparql body params))
               (org-babel-sparql--current-curies
                (append org-link-abbrev-alist-local org-link-abbrev-alist))
               (elot-prefixed-query
                (concat (elot-prefix-block-from-alist org-link-abbrev-alist-local 'sparql) "\n" query))
               (format-symbol (if (string-match-p "\\(turtle\\|ttl\\)" format) 'ttl 'csv)))
          (with-temp-buffer
            (if (string-match-p "^http" url)
                (sparql-execute-query query url format t) ;; Query an endpoint
              (elot-robot-execute-query elot-prefixed-query url format-symbol)) ;; Query local file
            (org-babel-result-cond
                (cdr (assoc :result-params params))
              (buffer-string)
              (if (string-equal "text/csv" format)
                  (org-babel-sparql-convert-to-table)
                (buffer-string))))))
    ;; Default behavior for non-ELOT buffers
    (apply orig-fun args)))

(advice-add 'org-babel-execute:sparql :around #'elot--custom-org-babel-execute-sparql)
;; src-sparql-exec-patch ends here

;; [[file:../elot-defs.org::src-write-class][src-write-class]]
(defun elot-class-oneof-from-header (l)
  "L a list of class resources like ((super (((sub) (sub) ... (sub))))).
This is a helper function for `elot-resource-taxonomy-from-l'."
  (let ((owl-type "Class") (owl-subclause "SubClassOf"))
    (concat "\n" owl-type ": " (elot-entity-from-header (car l))
            "\n    " owl-subclause ": "
            (mapconcat (lambda (x)
                         (elot-entity-from-header (car x)))
                       (cdr l) " or "))))

(defun elot-class-disjoint-from-header (l)
  "L a list of class resources like ((super (((sub) (sub) ... (sub))))).
This is a helper function for `elot-resource-taxonomy-from-l'."
    (concat "\nDisjointClasses: "
            "\n    "
            (mapconcat (lambda (x)
                         (elot-entity-from-header (car x)))
                       (cdr l) ", ")))
;; src-write-class ends here

;; [[file:../elot-defs.org::src-write-taxonomy][src-write-taxonomy]]
(defun elot-org-tags-in-string (str)
  "Return list of any tags from Org heading contents STR."
  (if (string-match ".*\\W+:\\(.*\\):" str)
      (split-string (match-string 1 str) ":")))

(defun elot-resource-taxonomy-from-l (l owl-type owl-subclause)
  "Helper function for `elot-resource-taxonomy-from-header'.
Recursively go through the list L, outputting subtype axioms for OWL
entity type OWL-TYPE and subrelation OWL-SUBCLAUSE.

Process any `oneof' and `disjont' Org tags on each header, calling
`elot-class-oneof-from-header' or `elot-class-disjoint-from-header'."
  (if (listp (car l))
      (mapconcat (lambda (x) (elot-resource-taxonomy-from-l x owl-type owl-subclause)) l "")
    (if (and (stringp (car l)) (stringp (caadr l)))
        (concat
          ;simple subclass clauses
          (mapconcat (lambda (x)
                      (concat "\n" owl-type ": "
                              (elot-entity-from-header (car x))
                              "\n    " owl-subclause ": "
                              (elot-entity-from-header (car l))))
                    (cdr l) "")
          ;one-of pattern
          (if (member "oneof" (elot-org-tags-in-string (car l))) (elot-class-oneof-from-header l))
          ;disjoint pattern
          (if (member "disjoint" (elot-org-tags-in-string (car l))) (elot-class-disjoint-from-header l))
          (elot-resource-taxonomy-from-l (cdr l) owl-type owl-subclause)))))

(defun elot-resource-taxonomy-from-header (header-id owl-type owl-relation)
  "Output OMN subtype axioms for Class or Property Org trees.
This function is called from the `org-babel' block in file
`elot-lob.org' named `resource-taxonomy'.

HEADER-ID is an org location id, OWL-TYPE is `Class', `ObjectProperty',
`DataProperty', `AnnotationProperty', or `Individual'.  OWL-RELATION is
`SubClassOf' or `SubPropertyOf'.

The org location id, embedded in the `PROPERTIES' drawer for each OWL
resource type, is `<ontology>-class-hierarchy' for the Class outline,
and accordingly for `object-property', `data-property', and
`annotation-property'."
  (save-excursion
    (elot-org-link-search header-id)
    (if (org-goto-first-child)
        (let ((hierarchy-l (elot-org-list-siblings)))
          (elot-resource-taxonomy-from-l hierarchy-l owl-type owl-relation))
      (concat "## no " owl-type "taxonomy"))))
;; src-write-taxonomy ends here

;; [[file:../elot-defs.org::src-elot-xref][src-elot-xref]]
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

(defun elot--capture-slurp (&rest _args)
  "Copy the current buffer's `elot-slurp' into `elot-slurp-global'.

	This is used before xref is invoked so that label overlays can be shown
	in the `*xref*' buffer based on the current ELOT context."
  (when (boundp 'elot-slurp)
    (setq elot-slurp-global elot-slurp)))

(advice-add 'xref-find-references :before #'elot--capture-slurp)

(defun elot--xref-label-overlay-setup ()
  "Setup label overlays in the xref buffer using `elot-slurp-global'."
  (when (and (equal (buffer-name) "*xref*")
	     (fboundp 'elot-label-display-setup))
    (elot-label-display-setup)))

(add-hook 'xref-after-update-hook #'elot--xref-label-overlay-setup)

(cl-defmethod xref-backend-definitions ((_backend (eql elot)) identifier)
  "Return Org headlines that *define* IDENTIFIER."
  (elot--xref-find-matches identifier :find-definition t))

(add-hook 'xref-backend-functions #'elot-xref-backend)

(defun elot--xref-buffer-enable-backend ()
  "Enable the ELOT xref backend in the `*xref*` buffer.

This ensures `xref-find-definitions` works on CURIEs inside the xref buffer."
  (when (equal (buffer-name) "*xref*")
    (with-current-buffer (current-buffer)
      (make-local-variable 'xref-backend-functions)
      (add-hook 'xref-backend-functions #'elot-xref-backend nil t))))

(add-hook 'xref-after-update-hook #'elot--xref-buffer-enable-backend)


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
  ;; 1. Capture current elot-slurp so we can reuse it in Help buffer
  ;; ------------------------------------------------------------------
  (let ((elot-slurp-global (when (boundp 'elot-slurp) elot-slurp)))
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
	  ;; make elot-slurp visible in this buffer for overlay code
	  (setq-local elot-slurp elot-slurp-global)
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
		  (princ (format "  …and %d more\n"
				 (- (length refs) max-ref)))))
	    (princ "  (none)\n"))
	  (princ
	   "\n----\n`q' to quit, `RET' to visit location.\n")
	  ;; ------------------------------------
	  ;; 3c. Paint label overlays right now
	  ;; ------------------------------------
	  (when (fboundp 'elot-label-display-setup)
	    (elot-label-display-setup)))))))

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
    (insert (make-string indent ?\s) "• "
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
;; src-elot-xref ends here

;; [[file:../elot-defs.org::src-resolve-prefixes-on-export][src-resolve-prefixes-on-export]]
(defun elot--resolve-prefixes-in-description-list ()
  "Resolve RDF-style prefixes in description list values.
For lines matching ` - <term> :: <prefix>:` at the end,
replace <prefix> with the result of `elot-unprefix-uri`."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern "^\\s-*[-+]\\s-+\\(?:.*?\\)::\\s-*\\([[:word:]_./-]*:\\)\\s-*$"))
      (while (re-search-forward pattern nil t)
        (let* ((start (match-beginning 1))
               (end   (match-end 1))
               (prefix (buffer-substring-no-properties start end))
               (resolved (elot-unprefix-uri prefix org-link-abbrev-alist-local :noerror)))
          (when (stringp resolved)
            (delete-region start end)
            (goto-char start)
            (insert resolved)))))))
;; src-resolve-prefixes-on-export ends here

;; [[file:../elot-defs.org::src-linkify-codelist-items-in-buffer][src-linkify-codelist-items-in-buffer]]
(defun elot--linkify-codelist-items-in-buffer ()
  "Search for `elot-codelist-fontify-regexp` in the current buffer
and replace matches with Org-mode links, unless the match is
within a code block, example block, fixed-width area, or IN a headline's text.
The link description is obtained using `(elot-codelist-id-label MATCH)`."
  (save-excursion
    (goto-char (point-min))
    (while-let ((matched-text
                 (and (re-search-forward elot-codelist-fontify-regexp nil t)
                      (match-string-no-properties 0))))
      (let ((start (match-beginning 0))
            (end (match-end 0)))
        ;; Corrected check:
        (if (let* ((element-at-start ;; Get element at the beginning of the match
                    (save-excursion
                      (goto-char start)
                      (org-element-context))))
              (or
               (eq (org-element-type element-at-start) 'headline)
               (org-element-lineage element-at-start
                                    '(src-block example-block fixed-width)
                                    t))) ; 't' checks element-at-start itself too
            nil ; In forbidden context, do nothing and skip to next match
          (let* ((label (elot-codelist-id-label matched-text))
                 (link-string (if label (format "[[#%s][%s]]" matched-text label))))
            (if label 
                (progn
                  (delete-region start end)
                  (goto-char start)
                  (insert link-string)))))))))
;; src-linkify-codelist-items-in-buffer ends here

;; [[file:../elot-defs.org::src-stable-links-export][src-stable-links-export]]
(defun elot--prepare-export-buffer (backend)
  "Prepare the export clone for Elot:

  - Give each resource-declaring headline a CUSTOM_ID (if missing).
  - Replace every visible CURIE with an internal link to that ID,
    except when the CURIE is inside a src/example/fixed-width block."
  (when (org-export-derived-backend-p backend 'latex)
    (cl-return-from elot--prepare-export-buffer))
  ;; ------------------------------------------------------------
  ;; 1  Turn uses of defined resources into links
  ;; ------------------------------------------------------------
  (org-fold-show-all)
  (elot-label-display-setup)
  ;;(font-lock-fontify-buffer)
  (elot--linkify-codelist-items-in-buffer)
  ;; ------------------------------------------------------------
  ;; 2  Ensure CUSTOM_ID drawers
  ;; ------------------------------------------------------------
  (let (pending)                                   ; (marker . id) pairs
    (org-with-wide-buffer
     (org-element-map (org-element-parse-buffer 'headline) 'headline
       (lambda (hl)
         (let* ((title (org-element-property :raw-value hl))
                (id    (elot-entity-from-header title t)))
           (when id
             (let ((m (copy-marker (org-element-property :begin hl))))
               (unless (org-entry-get m "CUSTOM_ID")
                 (push (cons m id) pending)))))))
     ;; Insert from bottom to top so earlier insertions don’t shift markers
     (dolist (cell (nreverse pending))
       (org-with-point-at (car cell)
         (org-entry-put (car cell) "CUSTOM_ID" (cdr cell))))))
  ;; ------------------------------------------------------------
  ;; 3  Resolve prefixes in description list values
  ;; ------------------------------------------------------------
  (elot--resolve-prefixes-in-description-list))

(add-hook 'org-export-before-processing-functions #'elot--prepare-export-buffer)
;; src-stable-links-export ends here

;; [[file:../elot-defs.org::src-latex-section-export][src-latex-section-export]]
(defun elot-ontology-resource-section (level numbered-p)
  "Return LaTeX environment by subsection depth LEVEL.
If NUMBERED-P is `true', create a numbered section."
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
     (t "\\subsubsection*{%s}"))))
;; src-latex-section-export ends here

;; [[file:../elot-defs.org::src-get-heading-nocookie][src-get-heading-nocookie]]
(defun elot-org-get-heading-nocookie (&optional no-tags no-todo no-priority no-comment)
  "Call `org-get-heading' but strip out any task progress cookie, like `[3/4]'.
If provided, optional arguments NO-TAGS, NO-TODO, NO-PRIORITY, and NO-COMMENT
are passed on to `org-get-heading'."
  (replace-regexp-in-string " \\[[[:digit:]/%]+\\]$" ""
                            (org-get-heading no-tags no-todo no-priority no-comment)))
;; src-get-heading-nocookie ends here

;; [[file:../elot-defs.org::src-org-find-description-value][src-org-find-description-value]]
(defun elot-org-find-description-value (term-regex &optional value-regex)
  "Find value for TERM-REGEX from `elot-org-descriptions-in-section`.
If multiple matches, prefer the first where the value matches VALUE-REGEX.
Return the matched value string, or nil if not found.

VALUE-REGEX is optional; defaults to match anything."
  (setq value-regex (or value-regex ""))
  (let* ((descriptions (elot-org-descriptions-in-section))
         (matches (seq-filter (lambda (pair)
                                (string-match-p term-regex (car pair)))
                              descriptions))
         (result
          (or (seq-some (lambda (pair)
                          (let ((value (cadr pair)))
                            (when (and value
                                       (stringp value)
                                       (string-match-p value-regex value))
                              value)))
                        matches)
              (when matches
                (cadr (car matches))))))
    (if (stringp result)
        (replace-regexp-in-string ":newline:" "" result)
      nil)))
;; src-org-find-description-value ends here

;; [[file:../elot-defs.org::src-get-description-entry :tangle no][src-get-description-entry :tangle no]]
(defun elot-org-get-description-entry (tag)
  "Search forward for TAG and return text of Org element found.
Remove string decorations.  Newlines are replaced by spaces in the result."
  (save-excursion
    (if (search-forward-regexp tag nil t)
        (let* ((element (org-element-at-point))
               (beg (org-element-property :contents-begin element))
               (end (org-element-property :contents-end element))
               (entry-text (buffer-substring-no-properties beg end)))
          (replace-regexp-in-string "\n\s*" " " entry-text)))))
;; src-get-description-entry :tangle no ends here

;; [[file:../elot-defs.org::src-latex-export-replacenames][src-latex-export-replacenames]]
(org-export-define-derived-backend 'ELOT-latex 'latex
  :translate-alist '((item . elot-my-item-translator)))
(defvar elot-item-process nil
  "Toggle during LaTeX export, to turn replacement of list items on or off.
Used in `elot-my-item-translator'.")

(defun elot-my-item-translator (item c info)
  "Translator for LaTeX export, replace RDF identifiers with simpler labels.
This makes for more readable output in description lists.  A list serves
to map selected annotation properties to shorter labels.  For example,
`iof-av:explanatoryNote' will be replaced by `explanatory note'.

ITEM is an entry in a description list.  C is the contents of the item.
INFO is a plist holding contextual information.  See the documentation
for `org-latex-item'.

Translation is turned on when the magic value `item-translate-start' is
found in a description list, and off when `item-translate-stop' is found.

This function is a workaround.  It relies on magic strings because
positions in the buffer are unpredictable while the export is being
conducted."
  (let* ((item-tag-maybe (car (org-element-property :tag item)))
         (item-tag-stringp (stringp item-tag-maybe))
         (item-tag (if item-tag-stringp (substring-no-properties item-tag-maybe) item-tag-maybe)))
    (if (and item-tag-stringp (string= item-tag "item-translate-start")) (setq elot-item-process t))
    (if (and item-tag-stringp (string= item-tag "item-translate-stop")) (setq elot-item-process nil))
    (when (and elot-item-process item-tag-stringp)
      (progn
                                        ;(message (substring-no-properties item-tag))
        (setf (plist-get (cadr item) :checkbox) nil)  ; set checkbox here
        (let ((tag-mapped (assoc item-tag
                 (quote
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
                   ("iof-av:synonym" . "synonym"))))))
          (if tag-mapped
              (setf (plist-get (cadr item) :tag) (cdr tag-mapped))))))
    (unless (and item-tag-stringp
                 (or (string= item-tag "item-translate-start") (string= item-tag "item-translate-stop")))
      (org-latex-item item c info))))
;; src-latex-export-replacenames ends here

;; [[file:../elot-defs.org::src-elot-template][src-elot-template]]
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

   (t value)))                                        ; no decoration → untouched

(defun elot--strip-decoration (header)
  "Return HEADER without trailing @lang or ^^dtype part."
  (cond ((string-match "@[[:alnum:]-]+\\'" header)
         (substring header 0 (match-beginning 0)))
        ((string-match "\\^\\^.+\\'" header)
         (substring header 0 (match-beginning 0)))
        (t header)))

(defun elot--table->forest (mini-table)
  "Convert MINI-TABLE (Org-babel list) to a forest of plists.

- Header suffixes “@lang” / “^^datatype” decorate the cell value
  but are **removed** from the stored key.
- Warns if a row’s SUPER value never appears as an ID.
- Skips `hline` markers that `org-table-to-lisp` inserts."
  (let* ((headers (car mini-table))
         (rows    (cdr mini-table))
         (id-col  (cl-position "id"    headers :test #'string=))
         (sup-col (cl-position "super" headers :test #'string=))
         (id->obj (make-hash-table :test 'equal))
         triples)

    ;; ── pass 1: build node plists and register them ───────────────
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
                     (list :subs nil)))
               )
          (push (list id sup pl) triples)
          (puthash id pl id->obj))))

    (setq triples (nreverse triples))

    ;; ── pass 2: link children to parents, warn on missing parents ─
    (dolist (tr triples)
      (cl-destructuring-bind (id sup child) tr
        (unless (string= sup "")
          (let ((parent (gethash sup id->obj)))
            (if parent
                (setf (plist-get parent :subs)
                      (append (plist-get parent :subs) (list child)))
              (message "elot--table->forest: WARNING – parent id \"%s\" referenced by \"%s\" not found"
                       sup id)
              (setf (nth 1 tr) ""))))))         ; promote to root

    ;; ── pass 3: collect root nodes in original order ──────────────
    (let (forest)
      (dolist (tr triples)
        (cl-destructuring-bind (_id sup pl) tr
          (when (string= sup "") (push pl forest))))
      (nreverse forest))))

(defun elot--prop (plist key)
  "Return KEY’s value in PLIST, comparing keys with `string=`."
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
or `xsd:integer' on a column header will be applied to values.
"
  (interactive)
  (unless (org-at-table-p)
    (user-error "Point is not inside an Org table"))

  ;; 1. read the table as lisp
  (let* ((mini-table (org-table-to-lisp))
         (headers     (car mini-table)))
    ;; 2. must contain \"id\"
    (unless (member "id" headers)
      (user-error "Table lacks required \"id\" column – cannot create headings"))

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

        ;; 5. build forest → org text and insert
        (let* ((forest   (elot--table->forest mini-table))
               (org-text (elot-forest->org forest target-level)))
          (goto-char insert-pos)
          (insert "\n" org-text "\n"))))))
;; src-elot-template ends here

;; [[file:../elot-defs.org::src-babel-passthrough][src-babel-passthrough]]
(defun elot-org-babel-execute-passthrough (body params)
  "Return BODY unchanged when executing an Org Babel block.

This function is used to define a passthrough execution behavior
for Org Babel blocks with the language `ttl'.  It ensures that
the contents of a `#+begin_src ttl' block are returned as-is,
without any processing or transformation.

This is useful for passing Turtle (TTL) content to other source
blocks without modification.

PARAMS is ignored."
  (progn
    (always params)  ;; ignore argument
    body))

(unless (fboundp #'org-babel-execute:ttl)
  (defalias #'org-babel-execute:ttl #'elot-org-babel-execute-passthrough))
;; src-babel-passthrough ends here

;; [[file:../elot-defs.org::src-rdfpuml-execute][src-rdfpuml-execute]]
(defun elot-rdfpuml-execute (ttl &optional prefixes config add-options epilogue)
  "Run rdfpuml on Turtle RDF content and return PlantUML code.
TTL is a Turtle string, PREFIXES optional prefix block,
CONFIG optional Turtle for rdfpuml configuration,
ADD-OPTIONS a string of PlantUML options added to rdfpuml defaults,
EPILOGUE extra PlantUML clauses."
  (let* ((options-str
         (if add-options
             (concat "[] puml:options \"\"\""
                     elot-rdfpuml-options "\n"
                     add-options
                     "\n\"\"\".\n")))
        (input-ttl-file (org-babel-temp-file "rdfpuml-" ".ttl"))
        (output-puml-file (concat (file-name-sans-extension input-ttl-file) ".puml")))
    (with-temp-file input-ttl-file
      (insert (mapconcat #'identity
                         (list prefixes ttl config options-str) "\n")))
    ;; apparently prefixes.ttl is needed to reside in current dir, will overwrite
    (if prefixes (with-temp-file "prefixes.ttl"
                   (insert prefixes "\n")))
    (elot-rdfpuml-command input-ttl-file)
    (with-temp-file output-puml-file
      (insert-file-contents output-puml-file)
      (when epilogue
        (save-excursion
          (goto-char (point-min))
          (while (search-forward "@enduml" nil t)
            (replace-match (concat epilogue "\n@enduml") t t)))))
    output-puml-file))
;; src-rdfpuml-execute ends here

;; [[file:../elot-defs.org::src-plantuml-execute][src-plantuml-execute]]
(defun elot-plantuml-execute (puml-file output-name format)
  "With PlantUML, read PUML-FILE and write image file to OUTPUT-NAME.FORMAT.
The file is stored in the ELOT default image directory.
Return output file name."
  (if (or (string= org-plantuml-jar-path "") (not (file-exists-p org-plantuml-jar-path)))
    (error "PlantUML not found.  Set org-plantuml-jar-path with M-x customize-variable"))
  (let ((tmp-output-file (concat (file-name-sans-extension puml-file) "." format))
  (output-file (concat elot-default-image-path output-name "." format)))
    (message (concat puml-file " --> " output-file))
    (make-directory elot-default-image-path :always)
    (shell-command
     (concat "java -jar " org-plantuml-jar-path " -t" format " " puml-file))
    (copy-file tmp-output-file output-file :allow-overwrite)
    output-file))
;; src-plantuml-execute ends here

;; [[file:../elot-defs.org::src-tempo-docheader][src-tempo-docheader]]
(tempo-define-template "elot-doc-header"
                       '("# -*- eval: (load-library \"elot-defaults\") -*-" > n
                       "#+title: " (p "Document title: " doctitle) > n
                       "#+subtitle: An OWL ontology" > n
                       "#+author: " (p "Author name: " authname) > n
                       "#+date: WIP (version of " (format-time-string "%Y-%m-%d %H:%M") ")" > n
                       "#+call: theme-elot()" n n
                       (progn (load-library "elot-defaults") (message "Loaded ELOT") ""))
                       "<odh"
                       "ELOT document header"
                       'org-tempo-tags)
;; src-tempo-docheader ends here

;; [[file:../elot-defs.org::src-tempo-ontology][src-tempo-ontology]]
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
     "  ## Datatype declarations" > n
     "  <<resource-declarations(hierarchy=\"" (s ontlocalname) "-datatypes\", owl-type=\"Datatype\")>>" > n
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
;; src-tempo-ontology ends here

;; [[file:../elot-defs.org::src-tempo-resource][src-tempo-resource]]
(tempo-define-template "elot-class-iof-primitive"
 '(
   (org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Class label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ") [1/4]" > n
   " - [ ] iof-av:naturalLanguageDefinition :: " > n
   " - [X] iof-av:isPrimitive :: true" > n
   " - [ ] iof-av:primitiveRationale :: " > n
   " - [ ] skos:example :: " > )
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
   " - [ ] skos:example :: " > )
 "<ocd"
 "ELOT primitive class with IOF-AV annotations"
 'org-tempo-tags)

(tempo-define-template "elot-property-iof"
 '((org-open-line 1)
   (make-string (max 3 (org-current-level)) ?*) " "
   (p "Property label: ") " ("
   (elot-default-prefix) ":" (p "localname: ") ") [1/4]" > n
   " - [ ] iof-av:naturalLanguageDefinition :: " > n
   " - [ ] skos:example :: " > )
 "<op"
 "ELOT primitive class with IOF-AV annotations"
 'org-tempo-tags)
;; src-tempo-resource ends here

;; [[file:../elot-defs.org::src-tempo-codeblock][src-tempo-codeblock]]
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
;; src-tempo-codeblock ends here

;; [[file:../elot-defs.org::src-table-of-resources][src-table-of-resources]]
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
;; src-table-of-resources ends here

;; [[file:../elot-defs.org::src-easy-menu][src-easy-menu]]
(easy-menu-define elot-menu org-mode-map
  "ELOT Ontology Authoring Menu"
  '("ELOT"
    ["Check for common problens" elot-org-lint t]
    ["Export to OWL" org-babel-tangle t]
    ["Export to HTML" (lambda () (interactive) (browse-url-of-file (expand-file-name (org-html-export-to-html)))) t]
    ["Import OWL ontology" elot-open-owl t]
    "---"
    ["Insert Existing Resource ID" elot-label-lookup t]
    ["Insert Primitive Class template" (lambda () (interactive) (outline-next-heading) (tempo-template-elot-class-iof-primitive)) t]
    ["Insert Defined Class template" (lambda () (interactive) (outline-next-heading) (tempo-template-elot-class-iof-defined)) t]
    ["Insert Property template" (lambda () (interactive) (outline-next-heading) (tempo-template-elot-property-iof)) t]
    "---"
    ["Jump to Resource Headline" xref-find-definitions t]
    ["Find References to Resource" xref-find-references t]
    ["Quick-Describe Resource" elot-describe-curie-at-point t]
    ["Toggle Label-Display" elot-toggle-label-display t]
    ["Refresh Label-Display Index" elot-label-display-setup t]
    "---"
    ["Insert Table of Resources" tempo-template-elot-table-of-resources t]
    ["Generate Outline from Table of Resources " elot-headings-from-table t]
    "---"
    ["Insert SPARQL Select Block" tempo-template-elot-block-sparql-select t]
    ["Insert SPARQL Construct Block" tempo-template-elot-block-sparql-construct t]
    ["Insert RDFPUML Diagram Block" tempo-template-elot-block-rdfpuml-diagram t]
    "---"
    ["Insert New Ontology Document Header" tempo-template-elot-doc-header t]
    ["Insert New Ontology Skeleton" tempo-template-elot-ont-skeleton t]
    ))
;; src-easy-menu ends here

;; [[file:../elot-defs.org::src-tempo-fwd-declare][src-tempo-fwd-declare]]
(declare-function tempo-template-elot-block-robot-metrics "tempo")
(declare-function tempo-template-elot-block-sparql-select "tempo")
(declare-function tempo-template-elot-block-sparql-construct "tempo")
(declare-function tempo-template-elot-block-rdfpuml-diagram "tempo")
(declare-function tempo-template-elot-doc-header "tempo")
(declare-function tempo-template-elot-ont-skeleton "tempo")
;; src-tempo-fwd-declare ends here

;; [[file:../elot-defs.org::src-hydra-menu][src-hydra-menu]]
(defhydra elot-hydra (:color blue :hint nil)
  "
 --- ELOT helpdesk --- press F5 to toggle labels ---

 Output:  [_t_] ontology    [_h_] HTML

 Insert                    Code block             Document
--------------------------------------------------------------
 [_r_] resource id        <_obm_ metrics             <_odh_ header
<_ocp_ primitive class    <_obs_ sparql select       <_ods_ ontology
<_ocd_ defined class      <_obc_ sparql construct    <_otr_ resource table
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
  ("otr" (tempo-template-elot-table-of-resources)))
;; src-hydra-menu ends here

;; [[file:../elot-defs.org::src-hydra-keybinding][src-hydra-keybinding]]
(defcustom elot-key-open-hydra (kbd "S-<f5>")
  "Keybinding to open the ELOT hydra."
  :type 'key-sequence
  :group 'elot)

(defcustom elot-key-toggle-labels (kbd "<f5>")
  "Keybinding to toggle label display in ELOT buffers."
  :type 'key-sequence
  :group 'elot)

(defun elot-setup-org-keybindings ()
  (local-set-key elot-key-open-hydra #'elot-hydra/body)
  (local-set-key elot-key-toggle-labels #'elot-toggle-label-display))
;; src-hydra-keybinding ends here

;; [[file:../elot-defs.org::src-tsv-table][src-tsv-table]]
(defun elot-tsv-to-table (filename)
  "Read tab separated values file FILENAME and insert an Org table at point."
  (let* ((lines (with-temp-buffer
                 (insert-file-contents filename)
                 (split-string (buffer-string) "\n")))
         (header (split-string (car lines) "\t"))
         (body (mapcar
                (lambda (line) (split-string line "\t"))
                (butlast (cdr lines)))))  ;; check this is ok
    (cons header (cons 'hline body))))
;; src-tsv-table ends here

;; [[file:../elot-defs.org::src-robot-metrics][src-robot-metrics]]
(tempo-define-template "robot-metrics"
 '("#+call: robot-metrics(omnfile=\""
   (p "Ontology filename to read for metrics: ") "\")"
   (progn (org-ctrl-c-ctrl-c) ""))
   "<om"
   "ROBOT metrics"
   'org-tempo-tags)
;; src-robot-metrics ends here

(provide 'elot)
;;; elot.el ends here
