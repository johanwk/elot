;;; elot-gptel.el --- ELOT ontology-authoring tools for gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
;; Keywords: languages tools org ontology ai

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

;; This file exposes ELOT's ontology-authoring capabilities to large
;; language models via the gptel tool protocol.  The aim is a uniquely
;; powerful ontology authoring environment in which the human author
;; and an LLM share the same checks, queries, and reasoners that ELOT
;; already provides interactively.
;;
;; The file is loaded on demand.  Without an active gptel installation
;; nothing is registered and ELOT behaves exactly as before.
;;
;; Design principles:
;;
;;   1. Thin wrappers around existing ELOT Elisp -- this file does
;;      only argument marshalling and result formatting.
;;   2. Read-mostly by default; anything that mutates state is gated
;;      behind `elot-gptel-allow-side-effects' (default nil) and
;;      registered with `:confirm t'.
;;   3. Project-scoped paths -- file arguments are resolved relative
;;      to `project-current' (or `default-directory') and refused if
;;      they escape the project root.
;;   4. gptel is a soft dependency; ROBOT (where used) is invoked via
;;      a separate process layer (Milestone 2).
;;
;; Entry point:
;;
;;     M-x elot-gptel-register-tools

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function gptel-make-tool "gptel")
(declare-function gptel-tool-name "gptel")
(declare-function project-current "project")
(declare-function project-root "project")
(declare-function elot-org-lint "elot-lint")
(declare-function elot-update-headline-hierarchy "elot-tangle")
;; M9.3.F8: O(1) staleness markers.
(declare-function elot-headline-hierarchy-mark-stale "elot-tangle" ())
(declare-function elot-headline-hierarchy-ensure-fresh "elot-tangle" ())
(declare-function elot-get-ontology-node-omn "elot-tangle")
(declare-function elot-omn-collect-uri-line-map "elot-tangle")
(declare-function elot-omn-line-map-from-string "elot-tangle")
(declare-function elot-omn-write-sidecar-map "elot-tangle")
(declare-function elot-omn-lookup-org-line "elot-tangle")
(declare-function elot-robot-available-p "elot-robot")
(declare-function elot-robot-run "elot-robot")
(declare-function elot-robot-call-with-workspace "elot-robot")
(declare-function elot-robot-classify-result "elot-robot")
(declare-function elot-db-get-label-any "elot-db")
(declare-function elot-db-execute-readonly "elot-db")
(declare-function elot-db-init "elot-db")
(declare-function elot-db-contract-uri "elot-db" (uri &optional active-sources))
(declare-function elot-id-mint "elot-id")
(declare-function elot-id-verify "elot-id")
(declare-function elot-id-parse-spec "elot-id")
(declare-function elot-id-scheme-by-name "elot-id")
(declare-function elot-id-scheme-name "elot-id")
(declare-function elot-id-scheme-p "elot-id")
(declare-function elot-id-scheme-for-buffer "elot-id")
(declare-function elot-entity-from-header "elot-tangle")
(defvar elot-id-schemes)
(defvar elot-id-default-scheme)
(defvar elot-active-label-sources)

(defvar elot-db)
(defvar gptel-tools)
(defvar elot-flymake-checkers)
(defvar org-lint--checkers)
(defvar elot-headline-hierarchy)
(defvar elot-gptel--sparql-rdf-extensions)

;;; ---------------------------------------------------------------------------
;;; Customization
;;; ---------------------------------------------------------------------------

(defgroup elot-gptel nil
  "ELOT ontology-authoring tools exposed to gptel."
  :prefix "elot-gptel-"
  :group 'elot)

(defcustom elot-gptel-allow-side-effects nil
  "When non-nil, allow ELOT gptel tools that mutate state to run.
Side-effecting tools include those that write files, modify Org
buffers, or perform network I/O.  When nil (the default), such
tools refuse to act and return an explanatory error.

This is independent of gptel's per-tool `:confirm' flag; both
gates must be open for a side-effecting operation to proceed.

See also `elot-gptel-confirm-mutations', which controls whether
the per-call gptel confirmation prompt is suppressed once this
flag is on (the `once-armed' default).  When using
`once-armed', call \\[elot-gptel-register-tools] again after
toggling this flag so the change takes effect; the ELOT menu's
`Allow LLM side effects' toggle does this automatically when
ELOT tools are already registered."
  :type 'boolean
  :group 'elot-gptel)

(defcustom elot-gptel-confirm-mutations 'once-armed
  "Policy controlling per-call confirmation for mutating ELOT gptel tools.
A mutating tool is one whose spec carries `:confirm t' -- the
insert family (`elot_insert_sibling_resource',
`elot_insert_child_resource', `elot_insert_resource_tree')
currently does so by default; rename and move do not (recorded
in the M9.3 Decisions Log).

This defcustom is consulted by `elot-gptel--register-one' when
the tool table is assembled.  Three values are recognised:

  `always'      Honour the spec's `:confirm t' verbatim.  The
                user sees a per-call confirmation prompt for
                every mutating tool, /in addition/ to the
                session-wide `elot-gptel-allow-side-effects'
                gate.

  `once-armed'  (default) Suppress `:confirm t' once
                `elot-gptel-allow-side-effects' is non-nil --
                the user has already armed the session, one
                gate is enough.  When the flag is nil the
                spec's `:confirm t' still applies (though the
                tool would refuse to run anyway).

  `never'       Suppress `:confirm t' unconditionally.  The
                side-effects flag is the sole pre-commit gate.
                Closes the rename/move-vs-insert asymmetry the
                M9.3 changelog records.

Changing this defcustom via the customisation mechanism
re-registers ELOT's tool table automatically when gptel is
loaded; toggling it via `setq' requires a manual
\\[elot-gptel-register-tools]."
  :type '(choice (const :tag "Always confirm mutations" always)
                 (const :tag "Skip confirmation once side effects armed" once-armed)
                 (const :tag "Never confirm mutations" never))
  :group 'elot-gptel
  :set (lambda (sym val)
         (set-default sym val)
         (when (and (featurep 'gptel)
                    (fboundp 'elot-gptel-register-tools)
                    (bound-and-true-p elot-gptel--tools))
           (ignore-errors (elot-gptel-register-tools)))))

(defcustom elot-gptel-robot-catalog nil
  "Optional path to a ROBOT XML catalog file for import resolution.
When ROBOT is asked to parse an ELOT-tangled ontology that uses
`Import' declarations resolving to local files, an OASIS XML
catalog (`catalog-v001.xml' by convention) maps the import IRIs
to filesystem paths.  ROBOT requires `--catalog PATH' as a
*global* option (placed before the subcommand) to honour it; on
its own ROBOT will try to fetch the import IRI over HTTP and
fail with `Could not load imported ontology'.

Three modes:

  - nil (default) -- auto-detect.  For every ROBOT call that
    derives its OMN from an ELOT .org source, look for a
    `catalog-*.xml' file in the .org file's directory; when
    exactly one (or a deterministically first one when several)
    exists, pass it to ROBOT.  When no catalog is present, no
    `--catalog' option is added and ROBOT runs as before.

  - a string -- explicit override; that path is used as the
    catalog for every ROBOT call (resolved against
    `default-directory' when relative).

  - the symbol `none' -- disable auto-detection even when a
    `catalog-*.xml' is present (escape hatch for the user who
    wants the pre-catalog behaviour back).

ROBOT resolves catalog-relative URIs against the catalog file's
own location, so the file can be passed by absolute path from
any working directory; the relative entries inside it
(`uri=\"../src/itxm-core.ttl\"' and so on) keep working."
  :type '(choice (const :tag "Auto-detect" nil)
                 (const :tag "Disable" none)
                 (file :tag "Explicit catalog path"))
  :group 'elot-gptel)

(defconst elot-gptel--catalog-filename-re
  "\\`catalog-.*\\.xml\\'"
  "Regex matching the conventional ROBOT catalog filename.")

(defun elot-gptel--catalog-find (dir)
  "Return the catalog file in DIR, or nil.
DIR is searched for files matching `catalog-*.xml'; when more
than one matches the lexicographically first is returned (so the
`catalog-v001.xml' convention wins over hypothetical siblings)."
  (when (and dir (file-directory-p dir))
    (let ((hits (directory-files dir t elot-gptel--catalog-filename-re t)))
      (and hits (car (sort hits #'string<))))))

(defun elot-gptel--catalog-args (org-file)
  "Return ROBOT global-option args for catalog resolution, or ().
ORG-FILE is the user's source .org path (not the workspace temp
file).  Returns `(\"--catalog\" PATH)' when a catalog is
available, `()' otherwise.  Honours `elot-gptel-robot-catalog':
explicit string wins, `none' disables, nil triggers auto-detect
in ORG-FILE's directory."
  (let ((conf elot-gptel-robot-catalog))
    (cond
     ((eq conf 'none) nil)
     ((and (stringp conf) (not (string-empty-p conf)))
      (let ((path (expand-file-name conf)))
        (and (file-readable-p path) (list "--catalog" path))))
     ((null conf)
      (let ((path (and org-file
                       (elot-gptel--catalog-find
                        (file-name-directory
                         (expand-file-name org-file))))))
        (and path (list "--catalog" path))))
     (t nil))))

(defcustom elot-gptel-content-arg-max-bytes (* 4 1024 1024)
  "Soft cap on the `content' argument size, in bytes.
Applies to the optional `content' string accepted by
`elot_lint' and `elot_omn_validate' (M7.5 Step 7.5.2).  When
the supplied draft exceeds this many bytes (measured as
`string-bytes'), the tool refuses with a structured
`ERROR:' line instead of writing the content to the
workspace.  Defaults to 4 MiB -- every ELOT ontology shipped
with the package is below 200 KiB, so the cap is generous
and exists as a belt-and-braces guard against a pathological
LLM draft."
  :type 'integer
  :group 'elot-gptel)

;; ROBOT invocation is configured by `elot-robot-command-str' (defined
;; in `elot-tangle.el', built from `elot-robot-jar-path').  Reasoning
;; and validation tools introduced in later milestones reuse that
;; variable; `elot-gptel' deliberately does not introduce a separate
;; ROBOT-program defcustom.

;;; ---------------------------------------------------------------------------
;;; Internal state
;;; ---------------------------------------------------------------------------

(defvar elot-gptel--tools nil
  "Alist (NAME . TOOL) of tools registered by `elot-gptel-register-tools'.
NAME is a string (the tool's external name); TOOL is whatever
`gptel-make-tool' returns for that NAME.")

;;; ---------------------------------------------------------------------------
;;; Path resolution
;;; ---------------------------------------------------------------------------

(defun elot-gptel--project-root ()
  "Return the project root for the current context, or `default-directory'.
The root is returned as an absolute, slash-terminated directory."
  (let* ((proj (ignore-errors (project-current nil)))
         (root (if proj
                   (ignore-errors (project-root proj))
                 default-directory)))
    (file-name-as-directory
     (expand-file-name (or root default-directory)))))

(defun elot-gptel--resolve-file-path (path)
  "Resolve PATH against the project root without requiring existence.
Returns an absolute filename when PATH is under the project root;
signals `user-error' otherwise.  Symlinks are resolved on the
parent directory chain so symlink-based escapes are refused.
Unlike `elot-gptel--resolve-file', this does *not* check that the
resulting path names an existing regular file -- intended for the
M7.5 `content' arg, where the LLM lints an in-flight draft whose
on-disk counterpart may not yet exist."
  (unless (and path (stringp path) (not (string-empty-p path)))
    (user-error "elot-gptel: missing file argument"))
  (let* ((root (file-truename (elot-gptel--project-root)))
         (root-dir (file-name-as-directory root))
         (abs (expand-file-name path root-dir))
         ;; Resolve via the parent directory so we accept paths
         ;; whose final segment does not yet exist on disk.
         (parent (file-truename (file-name-directory abs)))
         (parent-dir (file-name-as-directory parent))
         (true (expand-file-name (file-name-nondirectory abs) parent-dir)))
    (unless (or (string-prefix-p root-dir true)
                (string= (directory-file-name true)
                         (directory-file-name root-dir)))
      (user-error "elot-gptel: refusing path outside project: %s" path))
    true))

(defun elot-gptel--resolve-file (path)
  "Resolve PATH against the current project root.
Returns an absolute filename if PATH is under the project root
and refers to an existing regular file.  Signals `user-error'
otherwise.  Symlinks are resolved before the containment check
so escapes via symlink are refused."
  (let ((true (elot-gptel--resolve-file-path path)))
    (unless (file-exists-p true)
      (user-error "elot-gptel: file not found: %s" path))
    (unless (file-regular-p true)
      (user-error "elot-gptel: not a regular file: %s" path))
    true))

(defun elot-gptel--check-content-size (content)
  "Refuse CONTENT when it exceeds `elot-gptel-content-arg-max-bytes'.
Signals `user-error' on overflow; returns nil otherwise so the
call reads naturally inline."
  (when (and content (stringp content))
    (let ((bytes (string-bytes content))
          (cap (and (integerp elot-gptel-content-arg-max-bytes)
                    (> elot-gptel-content-arg-max-bytes 0)
                    elot-gptel-content-arg-max-bytes)))
      (when (and cap (> bytes cap))
        (user-error
         "elot-gptel: content argument too large (%d bytes > %d cap)"
         bytes cap))))
  nil)

;;; ---------------------------------------------------------------------------
;;; elot_lint tool
;;; ---------------------------------------------------------------------------

(defconst elot-gptel--lint-default-checkers
  '(elot/nodeclare-id-prefix-label
    elot/ontology-header
    elot/prefix-table
    elot/ontology-presence
    elot/required-sections
    elot/ontology-declaration-heading
    elot/description-list-curies
    elot/axiom-value-curies
    elot/omn-syntax
    elot/omn-keyword-appropriateness
    elot/axiom-keyword-range
    elot/blank-omn-axiom-row
    elot/subclass-in-description-list
    ;; OOPS!-derived sliver (M8 follow-up); the full SPARQL-backed
    ;; OOPS! suite remains available via `elot-oops-run' for cases
    ;; the cheap Elisp sliver cannot see.
    elot/oops-missing-annotations
    elot/oops-recursive-definition
    elot/oops-duplicate-labels)
  "Default list of ELOT-specific org-lint checkers exercised by `elot_lint'.")

(defun elot-gptel--classify-severity (text)
  "Classify lint message TEXT as `error', `warning', or `note'."
  (let ((plain (substring-no-properties (or text ""))))
    (cond
     ((string-match-p "\\`ERROR:" plain)    'error)
     ((string-match-p "\\`WARNING:" plain)  'warning)
     ((string-match-p "ERROR"  plain)       'error)
     ((string-match-p "WARNING" plain)      'warning)
     (t                                     'note))))

(defun elot-gptel--checker-by-name (name)
  "Return the `org-lint--checker' struct whose name symbol is NAME."
  (require 'org-lint)
  (seq-find (lambda (c) (eq (org-lint-checker-name c) name))
            org-lint--checkers))

(defun elot-gptel--line-col-at (pos)
  "Return cons (LINE . COL) for buffer POS in the current buffer."
  (save-excursion
    (goto-char (min (max (point-min) (or pos 1)) (point-max)))
    (cons (line-number-at-pos) (1+ (current-column)))))

(defun elot-gptel--run-lint (file checker-names &optional logical-file)
  "Run CHECKER-NAMES on FILE.
Return a list of plists (:line :col :severity :category :trust :message).
LOGICAL-FILE, when non-nil, is used for `buffer-file-name' and
`default-directory' during the lint so checkers and Org-mode
resolution behave as though the bytes lived there (used by the
M7.5 `content' code path, where FILE is a workspace temp file
but diagnostics should be framed against the user's intended
path)."
  (require 'org)
  (require 'org-lint)
  (require 'elot-lint)
  (let ((issues '())
        (logical (or logical-file file)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((default-directory (file-name-directory logical))
            (buffer-file-name  logical))
        ;; Full mode setup -- some ELOT checkers depend on org-mode
        ;; hooks and the resulting buffer-local state
        ;; (e.g. `org-link-abbrev-alist-local').
        (let ((org-inhibit-startup t)
              (org-mode-hook nil))
          (org-mode))
        ;; Refresh slurp data so checkers see fresh prefix tables.
        ;; `elot-label-display-setup' does much more (fontification,
        ;; help-at-pt timer, ...) so any failure in those late steps
        ;; should not abort our lint run.  We still want
        ;; `elot-update-headline-hierarchy' to have run, so call it
        ;; explicitly as a safety net.
        (when (fboundp 'elot-update-headline-hierarchy)
          (condition-case _ (elot-update-headline-hierarchy)
            (error nil)))
        (when (fboundp 'elot-label-display-setup)
          (condition-case _ (elot-label-display-setup) (error nil)))
        (let ((tree (org-element-parse-buffer)))
          ;; Position point inside the first ontology heading (if any)
          ;; so checkers that rely on `org-entry-get-with-inheritance'
          ;; (e.g. `elot-context-type') see the right context.
          (goto-char (point-min))
          (let ((onto-pos
                 (catch 'found
                   (org-element-map tree 'headline
                     (lambda (hl)
                       (when (= (org-element-property :level hl) 1)
                         (let ((beg (org-element-property :begin hl)))
                           (save-excursion
                             (goto-char beg)
                             (when (string= (org-entry-get nil "ELOT-context-type")
                                            "ontology")
                               (throw 'found beg))))))
                     nil)
                   nil)))
            (when onto-pos
              (goto-char onto-pos)
              ;; Move into the section body so inheritance kicks in.
              (forward-line 1)))
          (dolist (name checker-names)
            (let ((checker (elot-gptel--checker-by-name name)))
              (when checker
                (let ((fn    (org-lint-checker-function checker))
                      (trust (ignore-errors (org-lint-checker-trust checker))))
                  (condition-case err
                      (dolist (hit (save-excursion (funcall fn tree)))
                        (let* ((pos     (nth 0 hit))
                               (msg     (substring-no-properties
                                         (or (nth 1 hit) "")))
                               (lc      (elot-gptel--line-col-at pos))
                               (sev     (elot-gptel--classify-severity msg)))
                          (push (list :line     (car lc)
                                      :col      (cdr lc)
                                      :severity sev
                                      :category name
                                      :trust    (or trust 'low)
                                      :message  msg)
                                issues)))
                    (error
                     (push (list :line 1 :col 1
                                 :severity 'error
                                 :category name
                                 :trust 'low
                                 :message
                                 (format "checker %s signalled: %S"
                                         name err))
                           issues))))))))))
    ;; Stable sort by (line, col).
    (sort (nreverse issues)
          (lambda (a b)
            (let ((la (plist-get a :line)) (lb (plist-get b :line)))
              (if (= la lb)
                  (< (plist-get a :col) (plist-get b :col))
                (< la lb)))))))

(defun elot-gptel--severity-match-p (issue requested)
  "Return non-nil when ISSUE matches the REQUESTED severity filter.
REQUESTED is one of `error', `warning', or `all'."
  (let ((sev (plist-get issue :severity)))
    (pcase requested
      ('all     t)
      ('error   (eq sev 'error))
      ('warning (memq sev '(error warning)))
      (_        t))))

(defun elot-gptel--category-symbol (cat)
  "Return CAT as a symbol (accept string or symbol)."
  (cond ((symbolp cat) cat)
        ((stringp cat) (intern cat))
        (t nil)))

(defun elot-gptel--format-issue (issue)
  "Format ISSUE plist as a single line of report text."
  (let ((line (plist-get issue :line))
        (col  (plist-get issue :col))
        (cat  (plist-get issue :category))
        (tr   (plist-get issue :trust))
        (msg  (plist-get issue :message)))
    (format "%d:%d  [%s/%s]  %s" line col cat tr msg)))

(defun elot-gptel-tool-lint (file &optional severity categories content)
  "Implementation of the `elot_lint' tool.
FILE is the path to an ELOT .org file (project-relative or absolute).
SEVERITY is one of \"error\", \"warning\", or \"all\" (default \"all\").
CATEGORIES is an optional list of org-lint checker names (strings
or symbols) to restrict the run to.

CONTENT, when supplied, is an ELOT .org draft string to lint *in
place of* the on-disk contents of FILE.  The bytes are written to
a throwaway file inside a temporary workspace and that file is
linted; the user's source is neither read nor written.  FILE is
still required as the LLM's intended path for the draft (the
project-traversal guard applies and the path may not yet exist
on disk).  Lets an LLM agent re-lint its own in-flight edits
without a save roundtrip.

Returns a plain-text report.  See plan Milestones 1 and 7.5."
  (condition-case err
      (let* ((sev-key (and severity (downcase severity)))
             (sev-sym
              (pcase sev-key
                ((or "error" "errors")     'error)
                ((or "warning" "warnings") 'warning)
                ((or 'nil "all" "")        'all)
                (other
                 (user-error
                  "elot-gptel: unknown severity %S (use error|warning|all)"
                  other))))
             (checker-names
              (if categories
                  (delq nil (mapcar #'elot-gptel--category-symbol
                                    (append categories nil)))
                elot-gptel--lint-default-checkers))
             (content* (and content (stringp content)
                            (not (string-empty-p content))
                            content)))
        (when content*
          (elot-gptel--check-content-size content*))
        (let* ((logical (if content*
                            (elot-gptel--resolve-file-path file)
                          (elot-gptel--resolve-file file)))
               (issues
                (if content*
                    (progn
                      (require 'elot-robot)
                      (elot-robot-call-with-workspace
                       (lambda (ws)
                         (let ((tmp (expand-file-name
                                     (file-name-nondirectory logical)
                                     ws)))
                           (with-temp-file tmp (insert content*))
                           (elot-gptel--run-lint tmp checker-names logical)))
                       "elot-lint-content-"))
                  (elot-gptel--run-lint logical checker-names)))
               (filtered
                (cl-remove-if-not
                 (lambda (i) (elot-gptel--severity-match-p i sev-sym))
                 issues)))
          (if (null filtered)
              "OK: no lint issues"
            (let* ((n-err  (cl-count-if
                            (lambda (i) (eq (plist-get i :severity) 'error))
                            filtered))
                   (n-warn (cl-count-if
                            (lambda (i) (eq (plist-get i :severity) 'warning))
                            filtered))
                   (lines  (mapcar #'elot-gptel--format-issue filtered)))
              (concat (mapconcat #'identity lines "\n")
                      (format "\nSummary: %d error%s, %d warning%s"
                              n-err  (if (= n-err  1) "" "s")
                              n-warn (if (= n-warn 1) "" "s")))))))
    (user-error
     (format "ERROR: %s" (error-message-string err)))
    (error
     (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_omn_validate tool (Milestone 3)
;;; ---------------------------------------------------------------------------
;;
;; Pipeline: tangle the .org file's ontology heading(s) to OMN inside a
;; throw-away workspace, then ask ROBOT to parse them.  ROBOT's full
;; OWLAPI-backed parser catches a class of errors the per-axiom PEG
;; checker in `elot/omn-syntax' cannot -- e.g. an object property used
;; in class position, or a datatype property with a class as its range.
;;
;; Optional profile validation (DL / EL / QL / RL / Full) is gated by
;; the `profile' argument.
;;
;; The pipeline is intentionally read-only: it writes only into the
;; temp workspace allocated by `elot-robot-call-with-workspace', never
;; back into the source tree.  Source-line mapping (Step 3.3) is a
;; follow-up; for now we report OMN-internal coordinates from ROBOT
;; verbatim, prefixed with the OMN filename.

(defconst elot-gptel--omn-validate-profiles
  '("DL" "EL" "QL" "RL" "Full")
  "Recognised OWL profile values for `elot_omn_validate'.")

(defun elot-gptel--tangle-ontologies-to-workspace (org-file workspace &optional source-file)
  "Generate OMN for each ontology node in ORG-FILE into WORKSPACE.

Reads bytes from SOURCE-FILE when supplied, otherwise from
ORG-FILE.  In either case the OMN frames and sidecar map record
ORG-FILE as the logical source -- the M7.5 `content' code path
uses SOURCE-FILE to point at a workspace temp file while
ORG-FILE remains the user's intended path for diagnostics.

Loads the source into a temp buffer in `org-mode', refreshes the
ELOT headline hierarchy, and writes each ontology node's OMN to
\"<localname>.omn\" inside WORKSPACE.  Also writes a sidecar
source map (\"<localname>.omn.map\", see Step 3.3) recording
the Org line of each frame opener.  Returns a list of plists:

  (:name NAME :path ABSPATH :map MAP :org-file ORG-FILE)

where NAME is the ontology's ELOT-context-localname (or `ont-N'
when absent), PATH is the OMN file just written, and MAP is the
list of plists from `elot-omn-line-map-from-string'.  Signals
`user-error' with a descriptive message when no ontology node
can be produced (e.g. malformed ontology-declaration heading)."
  (require 'org)
  (require 'elot-tangle)
  (let ((nodes-info '())
        (read-from (or source-file org-file)))
    (with-temp-buffer
      (insert-file-contents read-from)
      (let ((default-directory (file-name-directory org-file))
            (buffer-file-name  org-file))
        (let ((org-inhibit-startup t)
              (org-mode-hook nil))
          (org-mode))
        (condition-case err
            (elot-update-headline-hierarchy)
          (error
           (user-error "elot-gptel: failed to parse ontology hierarchy: %s"
                       (error-message-string err))))
        (let ((children (plist-get elot-headline-hierarchy :children))
              (idx 0))
          (dolist (node children)
            (setq idx (1+ idx))
            (let* ((ctype (plist-get node :elot-context-type))
                   (lname (or (plist-get node :elot-context-localname)
                              (format "ont-%d" idx)))
                   (omn   (and (equal ctype "ontology")
                               (condition-case _err
                                   (elot-get-ontology-node-omn node)
                                 (error nil)))))
              (when (and omn (not (string-empty-p (string-trim omn))))
                (let* ((path (expand-file-name
                              (concat lname ".omn") workspace))
                       (uri-alist
                        (elot-omn-collect-uri-line-map node nil))
                       (map (elot-omn-line-map-from-string omn uri-alist))
                       (map-path (concat path ".map")))
                  (with-temp-file path (insert omn))
                  (ignore-errors
                    (elot-omn-write-sidecar-map map-path map org-file))
                  (push (list :name     lname
                              :path     path
                              :map      map
                              :org-file org-file)
                        nodes-info))))))))
    (when (null nodes-info)
      (user-error
       "elot-gptel: no ontology OMN produced (check ontology-declaration heading?)"))
    (nreverse nodes-info)))

(defun elot-gptel--format-classify (kp omn-path &optional map org-file)
  "Format classifier result KP for an OMN at OMN-PATH as human text.
When MAP (the per-ontology source map from
`elot-omn-line-map-from-string') and ORG-FILE are non-nil, a
`:syntax-error' carrying an OMN line is back-mapped to the
originating Org heading line."
  (let* ((kind    (car kp))
         (payload (cdr kp))
         (omn     (file-name-nondirectory (or omn-path ""))))
    (pcase kind
      (:syntax-error
       (let* ((line (plist-get payload :line))
              (col  (plist-get payload :column))
              (msg  (plist-get payload :message))
              (anchor (and line map
                           (elot-omn-lookup-org-line line map)))
              (org-line (cdr-safe anchor))
              (uri      (car-safe anchor))
              (src (cond
                    ((and org-line org-file)
                     (format "%s:%d (%s:%s%s%s)"
                             (file-name-nondirectory org-file)
                             org-line
                             omn line
                             (if col ":" "")
                             (or col "")))
                    (t
                     (format "%s:%s%s"
                             omn (or line "?")
                             (if col (format ":%d" col) ""))))))
         (format "ERROR at %s  [robot/syntax-error]%s  %s"
                 src
                 (if uri (format " near %s" uri) "")
                 (or msg "Manchester syntax parse error"))))
      (:io-error
       (format "ERROR  [robot/io-error]  %s%s"
               (or (plist-get payload :message) "I/O error")
               (let ((p (plist-get payload :path)))
                 (if p (format " (%s)" p) ""))))
      (:inconsistent-ontology
       (format "ERROR  [robot/inconsistent-ontology]  %s"
               (or (plist-get payload :explanation) "")))
      (:unsatisfiable-classes
       (format "ERROR  [robot/unsatisfiable-classes]  %s"
               (mapconcat #'identity (plist-get payload :iris) ", ")))
      (:reasoner-error
       (format "ERROR  [robot/reasoner-error]  %s"
               (or (plist-get payload :message) "")))
      (:unknown
       (format "ERROR  [robot/unknown]  %s"
               (let ((s (string-trim (or (plist-get payload :stderr) ""))))
                 (if (string-empty-p s)
                     "ROBOT reported a non-zero exit with no diagnostic output"
                   s))))
      (_ ""))))

(defun elot-gptel--omn-validate-node (node-info profile)
  "Validate a single NODE-INFO plist (:name :path :map :org-file).
Optionally check OWL profile PROFILE.
Return a plist (:name :ok-p :messages :duration-s)."
  (let* ((name     (plist-get node-info :name))
         (omn-path (plist-get node-info :path))
         (map      (plist-get node-info :map))
         (org-file (plist-get node-info :org-file))
         (ofn-path (concat (file-name-sans-extension omn-path) ".ofn"))
         (msgs     '())
         (ok-p     t)
         (t0       (current-time))
         (catalog-args (elot-gptel--catalog-args org-file))
         (res      (elot-robot-run
                    (append catalog-args
                            (list "convert"
                                  "--input"  omn-path
                                  "--output" ofn-path)))))
    (unless (and (numberp (plist-get res :exit))
                 (zerop (plist-get res :exit)))
      (setq ok-p nil)
      ;; Some ROBOT versions emit `convert' diagnostics on stdout
      ;; rather than stderr (the validate-profile path has the same
      ;; quirk).  If stderr is empty but stdout has content, classify
      ;; on stdout so the message body is not lost.
      (let* ((err-raw (or (plist-get res :stderr) ""))
             (out-raw (or (plist-get res :stdout) ""))
             (effective
              (if (string-empty-p (string-trim err-raw))
                  out-raw
                err-raw))
             (res-eff (plist-put (copy-sequence res)
                                 :stderr effective)))
        (push (elot-gptel--format-classify
               (elot-robot-classify-result res-eff)
               omn-path map org-file)
              msgs)))
    (when (and ok-p profile
               (member (upcase profile) elot-gptel--omn-validate-profiles))
      (let* ((res2 (elot-robot-run
                    (append catalog-args
                            (list "validate-profile"
                                  "--profile" (upcase profile)
                                  "--input"   omn-path))))
             (exit (plist-get res2 :exit))
             ;; `robot validate-profile' reports violations on stdout;
             ;; stderr is typically empty on a clean non-zero exit.
             ;; Prefer stdout, fall back to stderr.
             (report (string-trim
                      (let ((out (or (plist-get res2 :stdout) "")))
                        (if (string-empty-p (string-trim out))
                            (or (plist-get res2 :stderr) "")
                          out)))))
        (unless (and (numberp exit) (zerop exit))
          (setq ok-p nil)
          (push (format "ERROR  [robot/profile-%s]  %s"
                        (upcase profile)
                        (if (string-empty-p report)
                            (format "profile violations detected (exit %s)"
                                    exit)
                          report))
                msgs))))
    (list :name name
          :ok-p ok-p
          :messages (nreverse msgs)
          :duration-s (float-time (time-subtract (current-time) t0)))))

(defun elot-gptel-tool-omn-validate (file &optional profile content)
  "Implementation of the `elot_omn_validate' tool.

FILE is the path to an ELOT .org file.  PROFILE is one of
\"DL\", \"EL\", \"QL\", \"RL\", \"Full\" or nil (skip profile
validation).

CONTENT, when supplied, is an ELOT .org draft string to validate
in place of the on-disk contents of FILE.  Same semantics as the
`content' arg on `elot_lint': bytes are written to a throwaway
file inside the workspace, FILE is still required as the LLM's
intended path (project-traversal guard applies; the path may
not yet exist on disk).

Tangles each ontology node in FILE to a temporary OMN file,
runs `robot convert' to force a full OWLAPI parse, and (when
PROFILE is given) also runs `robot validate-profile'.  Returns
a plain-text report."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available — set `elot-robot-jar-path'"))
        (when (and profile
                   (not (member (upcase profile)
                                elot-gptel--omn-validate-profiles)))
          (user-error
           "elot-gptel: unknown profile %S (use DL|EL|QL|RL|Full)"
           profile))
        (let* ((content* (and content (stringp content)
                              (not (string-empty-p content))
                              content))
               (logical (if content*
                            (elot-gptel--resolve-file-path file)
                          (elot-gptel--resolve-file file))))
          (when content*
            (elot-gptel--check-content-size content*))
          (elot-robot-call-with-workspace
           (lambda (ws)
             (let* ((src (when content*
                           (let ((p (expand-file-name
                                     (file-name-nondirectory logical)
                                     ws)))
                             (with-temp-file p (insert content*))
                             p)))
                    (nodes   (elot-gptel--tangle-ontologies-to-workspace
                              logical ws src))
                    (results (mapcar
                              (lambda (n)
                                (elot-gptel--omn-validate-node n profile))
                              nodes))
                    (all-ok  (cl-every (lambda (r) (plist-get r :ok-p))
                                       results)))
               (if all-ok
                   (format "OK: %d ontology%s parsed%s"
                           (length results)
                           (if (= (length results) 1) "" " nodes")
                           (if profile
                               (format " and conform to profile %s"
                                       (upcase profile))
                             ""))
                 (mapconcat
                  (lambda (r)
                    (let ((name (plist-get r :name))
                          (msgs (plist-get r :messages)))
                      (concat
                       (format "Ontology %s:\n" name)
                       (if msgs
                           (mapconcat #'identity msgs "\n")
                         "  OK"))))
                  results
                  "\n\n"))))
           "elot-omn-validate-")))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_omn_report tool (Milestone 8 Step 8.4)
;;; ---------------------------------------------------------------------------
;;
;; Thin wrapper around ROBOT's `report' subcommand.  Tangles FILE the
;; same way `elot_omn_validate' does, then runs `robot report' once
;; per ontology node and surfaces the report verbatim in the standard
;; ELOT envelope.
;;
;; Scope deliberately narrow (M8.4 rewrite, see plan): we do NOT
;; vendor ROBOT's report-query catalogue, we do NOT assert which
;; queries fire on which inputs, and we do NOT batch / re-rank by
;; severity.  The catalogue is documented at
;; https://robot.obolibrary.org/report_queries/ -- defer to it.
;;
;; `--fail-on none' is passed so ROBOT writes a report file even
;; when the default INFO/WARN/ERROR threshold would otherwise mark
;; the run as failed; the LLM sees the violations, not just an exit
;; code.  Read-only with respect to the user's project (writes only
;; into the temporary workspace).

(defconst elot-gptel--omn-report-formats
  '("tsv" "csv" "yaml" "json" "html" "md")
  "Recognised values for the optional `format' arg of `elot_omn_report'.")

(defun elot-gptel--omn-report-node (node-info format)
  "Run `robot report' against NODE-INFO with FORMAT.
NODE-INFO is a plist (:name :path :map :org-file) as produced by
`elot-gptel--tangle-ontologies-to-workspace'.  Returns a plist
\(:name :ok-p :report :messages :duration-s).  REPORT is the
text ROBOT wrote to its output file (possibly empty when the
ontology is clean)."
  (let* ((name     (plist-get node-info :name))
         (omn-path (plist-get node-info :path))
         (org-file (plist-get node-info :org-file))
         (ws-dir   (file-name-directory omn-path))
         (out-path (expand-file-name
                    (concat name "-report." format) ws-dir))
         (t0       (current-time))
         (catalog-args (elot-gptel--catalog-args org-file))
         (res      (elot-robot-run
                    (append catalog-args
                            (list "report"
                                  "--input"   omn-path
                                  "--fail-on" "none"
                                  "--format"  format
                                  "--output"  out-path))))
         (exit     (plist-get res :exit))
         (msgs     '())
         (report   ""))
    (when (file-readable-p out-path)
      (with-temp-buffer
        (insert-file-contents out-path)
        (setq report (buffer-substring-no-properties
                      (point-min) (point-max)))))
    (unless (and (numberp exit) (zerop exit))
      ;; `robot report' prints diagnostics on stderr (or stdout in
      ;; some failure modes); reuse the M3 stdout/stderr fallback.
      (let* ((err-raw (or (plist-get res :stderr) ""))
             (out-raw (or (plist-get res :stdout) ""))
             (effective (if (string-empty-p (string-trim err-raw))
                            out-raw err-raw))
             (res-eff (plist-put (copy-sequence res)
                                 :stderr effective)))
        (push (elot-gptel--format-classify
               (elot-robot-classify-result res-eff)
               omn-path nil org-file)
              msgs)))
    (list :name name
          :ok-p (and (numberp exit) (zerop exit))
          :report report
          :messages (nreverse msgs)
          :duration-s (float-time (time-subtract (current-time) t0)))))

(defun elot-gptel--omn-report-empty-p (report format)
  "Return non-nil when REPORT (a ROBOT report body) is effectively empty.
FORMAT is the ROBOT --format value.  TSV/CSV reports carry a
header line even when no violations were found; treat <= 1
non-blank line as empty.  Other formats: trim and check."
  (let ((trimmed (string-trim (or report ""))))
    (cond
     ((string-empty-p trimmed) t)
     ((member format '("tsv" "csv"))
      (let ((lines (seq-filter
                    (lambda (l) (not (string-empty-p (string-trim l))))
                    (split-string trimmed "\n"))))
        (<= (length lines) 1)))
     (t nil))))

(defun elot-gptel-tool-omn-report (file &optional format content)
  "Implementation of the `elot_omn_report' tool.

FILE is the path to an ELOT .org file.  FORMAT is one of
\"tsv\" (default), \"csv\", \"yaml\", \"json\", \"html\",
\"md\".  CONTENT, when supplied, is an ELOT .org draft string
that replaces the on-disk contents of FILE -- same semantics as
the `content' arg on `elot_lint' / `elot_omn_validate'.

Tangles each ontology node in FILE to a temporary OMN file,
runs `robot report --fail-on none --format FORMAT --output ...'
to defer the choice of /what/ to check to ROBOT's published
catalogue (https://robot.obolibrary.org/report_queries/), and
returns the report body verbatim.  Multiple ontology nodes are
framed per-name.

Returns either:
  OK: ontology N node(s) report clean (no violations)
or a per-node block with the report body (TSV/CSV/...).  ROBOT
errors are surfaced via the same classifier as
`elot_omn_validate'.

Read-only -- writes only into a temporary workspace.  Requires
ROBOT to be configured (`elot-robot-jar-path' or a `robot'
executable on PATH)."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available -- set `elot-robot-jar-path'"))
        (let* ((fmt (downcase (or format "tsv"))))
          (unless (member fmt elot-gptel--omn-report-formats)
            (user-error
             "elot-gptel: unknown format %S (use %s)"
             format
             (mapconcat #'identity elot-gptel--omn-report-formats "|")))
          (let* ((content* (and content (stringp content)
                                (not (string-empty-p content))
                                content))
                 (logical (if content*
                              (elot-gptel--resolve-file-path file)
                            (elot-gptel--resolve-file file))))
            (when content*
              (elot-gptel--check-content-size content*))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((src (when content*
                             (let ((p (expand-file-name
                                       (file-name-nondirectory logical)
                                       ws)))
                               (with-temp-file p (insert content*))
                               p)))
                      (nodes   (elot-gptel--tangle-ontologies-to-workspace
                                logical ws src))
                      (results (mapcar
                                (lambda (n)
                                  (elot-gptel--omn-report-node n fmt))
                                nodes))
                      (all-ok  (cl-every (lambda (r) (plist-get r :ok-p))
                                         results))
                      (any-violations
                       (cl-some
                        (lambda (r)
                          (and (plist-get r :ok-p)
                               (not (elot-gptel--omn-report-empty-p
                                     (plist-get r :report) fmt))))
                        results)))
                 (cond
                  ((and all-ok (not any-violations))
                   (format "OK: %d ontology %s report clean (no violations)"
                           (length results)
                           (if (= (length results) 1) "node" "nodes")))
                  (t
                   (mapconcat
                    (lambda (r)
                      (let* ((name   (plist-get r :name))
                             (ok-p   (plist-get r :ok-p))
                             (rep    (plist-get r :report))
                             (msgs   (plist-get r :messages)))
                        (concat
                         (format "Ontology %s (format=%s):\n" name fmt)
                         (cond
                          ((not ok-p)
                           (if msgs
                               (mapconcat #'identity msgs "\n")
                             (format "  ERROR (no diagnostic output)")))
                          ((elot-gptel--omn-report-empty-p rep fmt)
                           "  (clean -- no violations)")
                          (t rep)))))
                    results
                    "\n\n")))))
             "elot-omn-report-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_diff tool (Milestone 9 Step 9.5)
;;; ---------------------------------------------------------------------------
;;
;; Thin wrapper around ROBOT's `diff' subcommand.  Given two
;; ontologies (FILE = the current state, BASELINE = an earlier
;; snapshot), tangle each ELOT .org input to OMN inside the same
;; workspace and run `robot diff --left BASELINE --right FILE
;; --labels true --format FORMAT --output ...'.  The report body is
;; returned verbatim, prefixed with a one-line provenance summary so
;; the LLM has the two filenames in hand when it digests the diff
;; for the user.
;;
;; Why this matters: ROBOT's diff output is structurally complete
;; but verbose and not particularly easy for humans to scan.  The
;; value here is exactly the integration seam -- get the diff into
;; the LLM's context so the model can summarise / interpret it.
;; ELOT does not re-rank, re-group, or filter ROBOT's output;
;; defer to ROBOT for the structural diff itself, defer to the LLM
;; for the human-friendly retelling.
;;
;; Read-only with respect to the user's project (writes only into
;; the temporary workspace).

(defconst elot-gptel--diff-formats
  '("plain" "pretty" "markdown" "html")
  "Recognised values for the optional `format' arg of `elot_diff'.
These are the formats ROBOT's `diff' subcommand emits.")

(defun elot-gptel--diff-prepare-input (file workspace tag)
  "Return a path ROBOT's `diff' subcommand can read for FILE.
ELOT .org files are tangled into a per-input subdirectory of
WORKSPACE named TAG so the two sides do not clobber each
other's OMN output paths.  RDF inputs are accepted verbatim
(extension in `elot-gptel--sparql-rdf-extensions')."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (cond
     ((string= ext "org")
      (let* ((sub (file-name-as-directory
                   (expand-file-name tag workspace))))
        (make-directory sub t)
        (let ((nodes (elot-gptel--tangle-ontologies-to-workspace
                      file sub)))
          ;; Multi-ontology .org files are unusual here -- ROBOT
          ;; diffs whole graphs, not per-node, so we hand it the
          ;; first ontology node's OMN.  A future step can surface
          ;; a per-node selector if a real use case appears.
          (plist-get (car nodes) :path))))
     ((member ext elot-gptel--sparql-rdf-extensions)
      file)
     (t
      (user-error
       "elot-gptel: unsupported input extension %S for %s (use .org or %s)"
       (or ext "(none)") file
       (mapconcat (lambda (e) (concat "." e))
                  elot-gptel--sparql-rdf-extensions ", "))))))

(defun elot-gptel-tool-diff (file baseline &optional format)
  "Implementation of the `elot_diff' tool.

FILE is the current ontology (ELOT .org or an RDF file ROBOT
can read directly).  BASELINE is an earlier snapshot of the
same ontology, same accepted extensions.  FORMAT is one of
\"plain\" (default), \"pretty\", \"markdown\", \"html\" -- the
output formats ROBOT's `diff' supports.

Runs `robot diff --left BASELINE --right FILE --labels true
--format FORMAT --output ...' and returns the report body
verbatim with a one-line provenance header.  Labels are
included so identifiers in the diff are intelligible without a
second tool call.

Returns either
  OK: no structural differences (baseline=..., file=...)
when the two inputs are structurally identical, or a block:
  Diff: baseline=... file=... format=...

  <ROBOT's diff body>

Read-only -- writes only into a temporary workspace.  Requires
ROBOT to be configured (`elot-robot-jar-path' or a `robot'
executable on PATH)."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available -- set `elot-robot-jar-path'"))
        (unless (and baseline (stringp baseline)
                     (not (string-empty-p baseline)))
          (user-error "elot-gptel: missing baseline argument"))
        (let ((fmt (downcase (or format "plain"))))
          (unless (member fmt elot-gptel--diff-formats)
            (user-error
             "elot-gptel: unknown format %S (use %s)"
             format
             (mapconcat #'identity elot-gptel--diff-formats "|")))
          (let* ((true-file     (elot-gptel--resolve-file file))
                 (true-baseline (elot-gptel--resolve-file baseline)))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((left  (elot-gptel--diff-prepare-input
                              true-baseline ws "left"))
                      (right (elot-gptel--diff-prepare-input
                              true-file ws "right"))
                      (ext (pcase fmt
                             ("markdown" "md")
                             ("html"     "html")
                             (_          "txt")))
                      (out-path (expand-file-name
                                 (concat "diff." ext) ws))
                      (res (elot-robot-run
                            (append (elot-gptel--catalog-args true-file)
                                    (list "diff"
                                          "--left"   left
                                          "--right"  right
                                          "--labels" "true"
                                          "--format" fmt
                                          "--output" out-path))))
                      (exit (plist-get res :exit)))
                 (unless (and (numberp exit) (zerop exit))
                   (let* ((err-raw (or (plist-get res :stderr) ""))
                          (out-raw (or (plist-get res :stdout) ""))
                          (effective
                           (if (string-empty-p (string-trim err-raw))
                               out-raw err-raw))
                          (res-eff (plist-put (copy-sequence res)
                                              :stderr effective)))
                     (user-error
                      "ROBOT diff failed: %s"
                      (elot-gptel--format-classify
                       (elot-robot-classify-result res-eff)
                       right nil true-file))))
                 (let ((body (with-temp-buffer
                               (when (file-readable-p out-path)
                                 (insert-file-contents out-path))
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                   (cond
                    ((or (string-empty-p (string-trim body))
                         (string-match-p "Ontologies are identical"
                                         body))
                     (format "OK: no structural differences (baseline=%s, file=%s)"
                             (file-name-nondirectory true-baseline)
                             (file-name-nondirectory true-file)))
                    (t
                     (format "Diff: baseline=%s file=%s format=%s\n\n%s"
                             (file-name-nondirectory true-baseline)
                             (file-name-nondirectory true-file)
                             fmt
                             (string-trim-right body)))))))
             "elot-diff-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_sparql tool (Milestone 4)
;;; ---------------------------------------------------------------------------
;;
;; Execute a SPARQL query against an ontology -- either an ELOT .org
;; file (tangled to OMN inside the workspace first) or an RDF file
;; ROBOT can read directly (.ttl, .owl, .ofn, .omn, .rdf).  Results
;; are returned as a plain-text payload ROBOT writes for us; the
;; tool selects the output extension so ROBOT picks the right
;; serialiser.
;;
;; Step 4.1 scope: pipeline + tool registration with tsv (default),
;; csv and json forwarding plus a line-based `limit'.  Org-table
;; rendering and richer truncation handling are Step 4.2.  A
;; read-only sibling and per-query update guard are Step 4.3.
;;
;; The pipeline is read-only -- ROBOT writes into the temp workspace
;; only, never back into the source tree.  Update / construct /
;; delete queries that would mutate state are refused unless
;; `elot-gptel-allow-side-effects' is non-nil; even then, ROBOT
;; `query' itself only writes a result file, never the input
;; ontology.  Genuine in-place updates would need ROBOT's `update'
;; subcommand and are out of scope for Step 4.1.

(defconst elot-gptel--sparql-rdf-extensions
  '("ttl" "owl" "ofn" "omn" "rdf" "rdfxml" "obo" "nt" "nq" "jsonld")
  "File extensions ROBOT can read directly as input ontologies.")

(defconst elot-gptel--sparql-result-formats
  '("tsv" "csv" "json" "table")
  "Recognised result-format values for `elot_sparql'.
\"table\" is forwarded to TSV by ROBOT and then re-rendered as
an Org table by `elot-gptel--sparql-format-result'.")

(defconst elot-gptel--sparql-limit-default 200
  "Default row cap for `elot_sparql' results.")

(defconst elot-gptel--sparql-limit-max 5000
  "Hard upper bound on the `limit' argument to `elot_sparql'.")

(defun elot-gptel--sparql-mutating-p (query)
  "Return non-nil when QUERY contains an UPDATE keyword.
Recognises the SPARQL 1.1 Update operations (INSERT, DELETE,
LOAD, CLEAR, CREATE, DROP, COPY, MOVE, ADD).  The check is
deliberately lexical -- a `# DELETE...' comment will be a false
positive, but that errs on the side of safety.

Note: CONSTRUCT and DESCRIBE are read-only query forms (they
shape a result graph but do not modify the dataset) and are
intentionally /not/ flagged by this predicate."
  (let ((case-fold-search t))
    (string-match-p
     "\\_<\\(?:INSERT\\|DELETE\\|LOAD\\|CLEAR\\|CREATE\\|DROP\\|COPY\\|MOVE\\|ADD\\)\\_>"
     (or query ""))))

(defun elot-gptel--sparql-query-form (query)
  "Return the first SPARQL query form keyword in QUERY as a lowercase string.
Strips line comments (`#...') and skips the prologue (PREFIX /
BASE declarations) before looking for the form keyword.  Returns
one of \"select\", \"ask\", \"construct\", \"describe\", or any
SPARQL 1.1 Update keyword in lowercase; nil if no recognised
form keyword is found."
  (let* ((raw (or query ""))
         ;; Strip line comments.
         (no-comments (replace-regexp-in-string "#[^\n]*" "" raw))
         ;; Drop the prologue: zero or more PREFIX / BASE lines.
         (case-fold-search t)
         (body
          (with-temp-buffer
            (insert no-comments)
            (goto-char (point-min))
            ;; Skip prologue declarations.  Each PREFIX / BASE
            ;; clause ends at the closing `>' of its IRI, *not*
            ;; at end-of-line -- a one-line query like
            ;; `PREFIX p: <iri> SELECT ...' must not have its
            ;; SELECT keyword swallowed by `[^\n]*'.
            (let ((case-fold-search t))
              (while (looking-at
                      "[ \t\n\r]*\\(?:PREFIX[ \t\n\r]+[^<]*<[^>]*>\
\\|BASE[ \t\n\r]+<[^>]*>\\)")
                (goto-char (match-end 0))))
            (buffer-substring-no-properties (point) (point-max)))))
    (when (string-match
           "\\_<\\(SELECT\\|ASK\\|CONSTRUCT\\|DESCRIBE\\|INSERT\\|DELETE\\|LOAD\\|CLEAR\\|CREATE\\|DROP\\|COPY\\|MOVE\\|ADD\\)\\_>"
           body)
      (downcase (match-string 1 body)))))

(defun elot-gptel--sparql-select-or-ask-p (query)
  "Return non-nil iff QUERY is a SELECT or ASK query.
CONSTRUCT and DESCRIBE return RDF graphs and are refused by this
predicate even though they are read-only; the
`elot_sparql_select' tool restricts itself to forms that return
solution bindings (SELECT) or a boolean (ASK)."
  (member (elot-gptel--sparql-query-form query) '("select" "ask")))

(defun elot-gptel--sparql-prepare-input (file workspace)
  "Return a path ROBOT can use as `--input' for FILE.
If FILE is an ELOT .org file, tangle each ontology heading into
WORKSPACE and return the OMN path for the first ontology node
\(more than one ontology in a single file is unusual; a future
step may surface a per-node selector).  Otherwise, accept FILE
verbatim provided its extension is in
`elot-gptel--sparql-rdf-extensions'."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (cond
     ((string= ext "org")
      (let* ((nodes (elot-gptel--tangle-ontologies-to-workspace
                     file workspace)))
        (plist-get (car nodes) :path)))
     ((member ext elot-gptel--sparql-rdf-extensions)
      file)
     (t
      (user-error
       "elot-gptel: unsupported input extension %S (use .org or %s)"
       (or ext "(none)")
       (mapconcat (lambda (e) (concat "." e))
                  elot-gptel--sparql-rdf-extensions
                  ", "))))))

(defun elot-gptel--sparql-truncate-text (text limit)
  "Return TEXT truncated to LIMIT data rows, with a trailer when truncated.
The first line is assumed to be a header (TSV/CSV) and kept;
JSON output is returned verbatim.  When TEXT has more data rows
than LIMIT, the surplus is dropped and a trailing
`... N more rows omitted' line is appended."
  (let* ((lines (split-string (or text "") "\n"))
         ;; Trailing empty line is common from ROBOT; drop a single
         ;; final empty line so we don't count it as a data row.
         (lines (if (and lines (string-empty-p (car (last lines))))
                    (butlast lines)
                  lines)))
    (cond
     ((null lines) "")
     ((<= (length lines) (1+ (or limit 0)))
      (mapconcat #'identity lines "\n"))
     (t
      (let* ((head (car lines))
             (rows (cdr lines))
             (kept (cl-subseq rows 0 (min (length rows) (or limit 0))))
             (drop (- (length rows) (length kept))))
        (concat head "\n"
                (mapconcat #'identity kept "\n")
                (format "\n... %d more row%s omitted"
                        drop (if (= drop 1) "" "s"))))))))

(defun elot-gptel--sparql-split-tsv-row (row)
  "Split a single TSV ROW string into a list of cell strings.
Empty cells are preserved as \"\".  Tab is the only separator."
  (split-string (or row "") "\t" nil))

(defun elot-gptel--sparql-org-table-escape (cell)
  "Make CELL safe to put inside an Org table cell.
Replaces literal pipe and newline characters with their Unicode
look-alikes so the table structure isn't broken.  Trims trailing
carriage returns left over from CRLF output."
  (let ((s (or cell "")))
    (setq s (replace-regexp-in-string "\r\\'" "" s))
    (setq s (replace-regexp-in-string "[\r\n]+" " / " s))
    (setq s (replace-regexp-in-string "|" "\u2502" s))
    s))

(defun elot-gptel--sparql-tsv-to-org-table (text)
  "Render TSV TEXT as an aligned Org table string.
The first line is taken as the header.  Returns TEXT unchanged
when it has no tab and only one line (likely an empty result or
a single-column scalar).  Uses `org-table-align' for column
padding when Org is available; otherwise returns an unaligned
but well-formed Org table."
  (let* ((raw (or text ""))
         ;; Drop a single trailing empty line (common from ROBOT).
         (lines (split-string raw "\n"))
         (lines (if (and lines (string-empty-p (car (last lines))))
                    (butlast lines)
                  lines)))
    (cond
     ((null lines) "")
     ((and (= (length lines) 1)
           (not (string-match-p "\t" (car lines))))
      (car lines))
     (t
      (let* ((rows (mapcar #'elot-gptel--sparql-split-tsv-row lines))
             (header (car rows))
             (data   (cdr rows))
             (ncols  (length header))
             (fmt-row (lambda (cells)
                        (concat "| "
                                (mapconcat
                                 #'elot-gptel--sparql-org-table-escape
                                 ;; pad short rows to ncols
                                 (append cells
                                         (make-list
                                          (max 0 (- ncols (length cells)))
                                          ""))
                                 " | ")
                                " |")))
             (sep (concat "|" (mapconcat (lambda (_) "---") header "+") "|"))
             (rendered
              (concat (funcall fmt-row header) "\n"
                      sep "\n"
                      (mapconcat fmt-row data "\n"))))
        (condition-case nil
            (progn
              (require 'org-table)
              (with-temp-buffer
                (insert rendered)
                (goto-char (point-min))
                (when (fboundp 'org-table-align)
                  (org-table-align))
                (buffer-substring-no-properties (point-min) (point-max))))
          (error rendered)))))))

(defun elot-gptel--sparql-format-result (out-path format limit)
  "Read OUT-PATH and format its contents for return.
FORMAT is one of \"tsv\", \"csv\", \"json\" or \"table\".  LIMIT
truncates by data rows (header preserved).  For \"table\" the
truncated TSV is rendered as an aligned Org table via
`elot-gptel--sparql-tsv-to-org-table'."
  (let ((text (with-temp-buffer
                (when (file-readable-p out-path)
                  (insert-file-contents out-path))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (pcase format
      ("json" text)                          ; ROBOT JSON is structured; do not line-truncate
      ("table"
       (let* ((truncated (elot-gptel--sparql-truncate-text text limit))
              ;; Separate trailer (if any) from the data so the table
              ;; itself stays a well-formed Org structure.
              (trailer-re "\n\\(\\.\\.\\. [0-9]+ more rows? omitted\\)\\'")
              (trailer (and (string-match trailer-re truncated)
                            (match-string 1 truncated)))
              (body (if trailer
                        (substring truncated 0 (match-beginning 0))
                      truncated))
              (table (elot-gptel--sparql-tsv-to-org-table body)))
         (if trailer (concat table "\n" trailer) table)))
      (_ (elot-gptel--sparql-truncate-text text limit)))))

(cl-defun elot-gptel--sparql-run (file query format limit &key select-only)
  "Worker shared by `elot-gptel-tool-sparql' and `-sparql-select'.
FILE, QUERY, FORMAT and LIMIT are as documented on those tools.
When SELECT-ONLY is non-nil, only SELECT and ASK queries are
permitted; everything else is refused regardless of
`elot-gptel-allow-side-effects'."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available — set `elot-robot-jar-path'"))
        (unless (and query (stringp query)
                     (not (string-empty-p (string-trim query))))
          (user-error "elot-gptel: missing or empty query"))
        (let* ((fmt (downcase (or format "tsv"))))
          (unless (member fmt elot-gptel--sparql-result-formats)
            (user-error "elot-gptel: unknown format %S (use %s)"
                        format
                        (mapconcat #'identity
                                   elot-gptel--sparql-result-formats
                                   "|")))
          (cond
           (select-only
            (unless (elot-gptel--sparql-select-or-ask-p query)
              (user-error
               "elot-gptel: elot_sparql_select accepts only SELECT or ASK queries (got %s)"
               (or (elot-gptel--sparql-query-form query) "unknown form"))))
           ((and (elot-gptel--sparql-mutating-p query)
                 (not elot-gptel-allow-side-effects))
            (user-error
             "elot-gptel: mutating SPARQL refused (set `elot-gptel-allow-side-effects')")))
          (let* ((lim (min (max (or limit elot-gptel--sparql-limit-default) 0)
                           elot-gptel--sparql-limit-max))
                 (true-file (elot-gptel--resolve-file file)))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((input (elot-gptel--sparql-prepare-input true-file ws))
                      (q-file (expand-file-name "query.rq" ws))
                      (out-ext (pcase fmt
                                 ("csv" "csv")
                                 ("json" "json")
                                 (_ "tsv")))
                      (out-path (expand-file-name
                                 (concat "out." out-ext) ws)))
                 (with-temp-file q-file (insert query))
                 (let* ((res (elot-robot-run
                              (append (elot-gptel--catalog-args true-file)
                                      (list "query"
                                            "--input" input
                                            "--query" q-file
                                            out-path))))
                        (exit (plist-get res :exit)))
                   (unless (and (numberp exit) (zerop exit))
                     (let* ((err-raw (or (plist-get res :stderr) ""))
                            (out-raw (or (plist-get res :stdout) ""))
                            (effective
                             (if (string-empty-p (string-trim err-raw))
                                 out-raw err-raw)))
                       (user-error
                        "ROBOT query failed (exit %s): %s"
                        exit (string-trim effective))))
                   (elot-gptel--sparql-format-result out-path fmt lim))))
             "elot-sparql-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-sparql (file query &optional format limit)
  "Implementation of the `elot_sparql' tool.

FILE is an ELOT .org file or an RDF file ROBOT can read directly
\(.ttl, .owl, .ofn, .omn, .rdf, ...).  QUERY is a SPARQL query
string.  FORMAT is one of \"tsv\" (default), \"csv\", \"json\",
\"table\".  LIMIT caps the number of data rows in the returned
text (default 200, capped at 5000).

SELECT, ASK, CONSTRUCT and DESCRIBE are read-only and always
permitted.  Mutating SPARQL (INSERT / DELETE / LOAD / CLEAR /
CREATE / DROP / COPY / MOVE / ADD) is refused unless
`elot-gptel-allow-side-effects' is non-nil.

Important caveat for ELOT .org inputs: the OMN passed to ROBOT
is freshly tangled into a temporary workspace and discarded
after the query returns.  Any mutating SPARQL therefore has /no
persistent effect/: the user-visible .org source is untouched.
The proper way to edit an ELOT ontology is to modify the .org
source directly and re-lint / re-validate.  Mutating SPARQL is
still permitted (when the side-effects flag is set) so that
queries against raw RDF inputs can be used for dry-run
experimentation."
  (elot-gptel--sparql-run file query format limit))

(defun elot-gptel-tool-sparql-select (file query &optional format limit)
  "Implementation of the `elot_sparql_select' tool.

A read-only sibling of `elot_sparql' that refuses any query
form other than SELECT or ASK -- even when
`elot-gptel-allow-side-effects' is enabled.  CONSTRUCT and
DESCRIBE are refused because their RDF-graph output is awkward
for an LLM to consume row-by-row; use `elot_sparql' for those.

Intended for confirm-free agent loops where the caller wants a
hard guarantee that no UPDATE / INSERT / DELETE keyword can ever
reach ROBOT.  Arguments and return shape are identical to
`elot-gptel-tool-sparql'."
  (elot-gptel--sparql-run file query format limit :select-only t))

;;; ---------------------------------------------------------------------------
;;; elot_unsatisfiable tool (Milestone 5 Step 5.1)
;;; ---------------------------------------------------------------------------
;;
;; Run ROBOT's `reason' subcommand against each ontology node and
;; report any unsatisfiable classes found.  Pipeline mirrors
;; `elot_omn_validate' (M3): tangle inside a workspace, run ROBOT,
;; classify the result.  The classifier already recognises
;; `:unsatisfiable-classes' and `:inconsistent-ontology' kinds, so
;; this tool reuses the M3 stderr/stdout fallback trick rather than
;; extending the classifier.

(defconst elot-gptel--reasoners
  '("hermit" "whelk")
  "Reasoners accepted by ELOT's reasoning tools.
`jfact', `emr' and `structural' are intentionally omitted --
`jfact' is eclipsed by HermiT for ELOT's use cases; `emr' and
`structural' are weak for unsatisfiability / consistency
tasks.  Any of them may be added back later if a use case
appears.")

(defun elot-gptel--db-label-of (iri-or-curie)
  "Return a `(IRI \"label\")' style suffix string when a label is known.
Best-effort lookup via `elot-db-get-label-any'; returns the bare
token unchanged when the DB is unavailable, the lookup fails, or
the label is empty."
  (let ((tok (or iri-or-curie "")))
    (or (and (fboundp 'elot-db-get-label-any)
             (condition-case nil
                 (let ((lbl (elot-db-get-label-any tok)))
                   (and lbl (stringp lbl) (not (string-empty-p lbl))
                        (format "%s (\"%s\")" tok lbl)))
               (error nil)))
        tok)))

(defun elot-gptel--format-unsat-iris (iris)
  "Return a bulleted-list string for IRIS, with labels when available."
  (mapconcat (lambda (i) (concat "  - " (elot-gptel--db-label-of i)))
             iris
             "\n"))

(defun elot-gptel--reason-node (node-info reasoner)
  "Run `robot reason' against NODE-INFO with REASONER.
NODE-INFO is a plist as produced by
`elot-gptel--tangle-ontologies-to-workspace' (:name :path :map
:org-file).  Returns a plist (:name :kind :payload :messages
:duration-s).  KIND is one of :ok / :unsatisfiable-classes /
:inconsistent-ontology / :syntax-error / :reasoner-error /
:io-error / :unknown."
  (let* ((name     (plist-get node-info :name))
         (omn-path (plist-get node-info :path))
         (map      (plist-get node-info :map))
         (org-file (plist-get node-info :org-file))
         (ws-dir   (file-name-directory omn-path))
         (out-path (expand-file-name (concat name "-reasoned.ofn") ws-dir))
         (t0       (current-time))
         (catalog-args (elot-gptel--catalog-args org-file))
         (res      (elot-robot-run
                    (append catalog-args
                            (list "reason"
                                  "--reasoner" reasoner
                                  "--input"    omn-path
                                  "--output"   out-path)))))
    (let* ((exit (plist-get res :exit))
           ;; ROBOT prints `reason' diagnostics on stdout for some
           ;; failure modes (e.g. "There are N unsatisfiable
           ;; classes"); reuse the M3 fallback trick.
           (err-raw (or (plist-get res :stderr) ""))
           (out-raw (or (plist-get res :stdout) ""))
           (effective (if (string-empty-p (string-trim err-raw))
                          out-raw err-raw))
           (res-eff (plist-put (copy-sequence res) :stderr effective))
           (kp (if (and (numberp exit) (zerop exit))
                   (cons :ok nil)
                 (elot-robot-classify-result res-eff)))
           (kind (car kp))
           (payload (cdr kp))
           (msgs '()))
      (pcase kind
        (:ok nil)
        (:unsatisfiable-classes
         (let ((iris (plist-get payload :iris)))
           (push (format "UNSATISFIABLE CLASSES (%d):\n%s"
                         (length iris)
                         (elot-gptel--format-unsat-iris iris))
                 msgs)))
        (:inconsistent-ontology
         (push (format "INCONSISTENT: %s"
                       (or (plist-get payload :explanation)
                           "ontology is inconsistent"))
               msgs))
        (_
         (push (elot-gptel--format-classify
                kp omn-path map org-file)
               msgs)))
      (list :name name
            :kind kind
            :payload payload
            :messages (nreverse msgs)
            :duration-s (float-time (time-subtract (current-time) t0))))))

(defun elot-gptel-tool-consistency (file &optional reasoner)
  "Implementation of the `elot_consistency' tool.

FILE is an ELOT .org file.  REASONER is one of \"hermit\"
(default) or \"whelk\".

Runs `robot reason --reasoner R' against each ontology node and
reports whether the ontology is consistent (has at least one
model).

Note the distinction from `elot_unsatisfiable':

  - An ontology is *inconsistent* when it has no models at all;
    typically caused by an individual asserted to be in two
    disjoint classes, or a functional property with two
    distinct values for the same subject.
  - A *consistent* ontology may still contain *unsatisfiable
    classes* -- classes whose extension is necessarily empty.
    Those are surfaced by `elot_unsatisfiable'.

Returns one of:
  OK: ontology is consistent (reasoner=R, N node(s))
  OK: ontology is consistent but has unsatisfiable classes ...
  INCONSISTENT: <explanation>"
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available — set `elot-robot-jar-path'"))
        (let* ((r (downcase (or reasoner "hermit"))))
          (unless (member r elot-gptel--reasoners)
            (user-error
             "elot-gptel: unknown reasoner %S (use %s)"
             reasoner
             (mapconcat #'identity elot-gptel--reasoners "|")))
          (let ((true-file (elot-gptel--resolve-file file)))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((nodes (elot-gptel--tangle-ontologies-to-workspace
                              true-file ws))
                      (results (mapcar
                                (lambda (n)
                                  (elot-gptel--reason-node n r))
                                nodes))
                      (any-inconsistent
                       (cl-some (lambda (res)
                                  (eq (plist-get res :kind)
                                      :inconsistent-ontology))
                                results))
                      (any-unsat
                       (cl-some (lambda (res)
                                  (eq (plist-get res :kind)
                                      :unsatisfiable-classes))
                                results))
                      (any-error
                       (cl-some (lambda (res)
                                  (memq (plist-get res :kind)
                                        '(:syntax-error :io-error
                                          :reasoner-error :unknown)))
                                results)))
                 (cond
                  (any-inconsistent
                   (mapconcat
                    (lambda (res)
                      (let* ((name (plist-get res :name))
                             (kind (plist-get res :kind))
                             (payload (plist-get res :payload)))
                        (pcase kind
                          (:inconsistent-ontology
                           (format "INCONSISTENT [%s]: %s"
                                   name
                                   (or (plist-get payload :explanation)
                                       "ontology has no models")))
                          (:ok
                           (format "Ontology %s: consistent" name))
                          (:unsatisfiable-classes
                           (format "Ontology %s: consistent (but %d unsatisfiable class%s -- see elot_unsatisfiable)"
                                   name
                                   (length (plist-get payload :iris))
                                   (if (= 1 (length (plist-get payload :iris)))
                                       "" "es")))
                          (_
                           (concat (format "Ontology %s (reasoner=%s):\n" name r)
                                   (mapconcat #'identity
                                              (plist-get res :messages)
                                              "\n"))))))
                    results
                    "\n\n"))
                  (any-error
                   (mapconcat
                    (lambda (res)
                      (let ((name (plist-get res :name))
                            (msgs (plist-get res :messages)))
                        (concat
                         (format "Ontology %s (reasoner=%s):\n" name r)
                         (if msgs
                             (mapconcat #'identity msgs "\n")
                           "  OK"))))
                    results
                    "\n\n"))
                  (any-unsat
                   (format "OK: ontology is consistent but has unsatisfiable classes (reasoner=%s, %d node%s -- run elot_unsatisfiable for details)"
                           r (length results)
                           (if (= (length results) 1) "" "s")))
                  (t
                   (format "OK: ontology is consistent (reasoner=%s, %d node%s)"
                           r (length results)
                           (if (= (length results) 1) "" "s"))))))
             "elot-consistency-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-unsatisfiable (file &optional reasoner)
  "Implementation of the `elot_unsatisfiable' tool.

FILE is an ELOT .org file (each ontology node is tangled to OMN
inside the workspace).  REASONER is one of \"hermit\" (default)
or \"whelk\".

Runs `robot reason --reasoner R' against each ontology node and
reports any unsatisfiable classes the reasoner finds.  An
inconsistent ontology trivially has every class unsatisfiable;
that case is reported by name (\"INCONSISTENT\") and points the
caller at `elot_consistency'.

Returns a plain-text report:
  OK: no unsatisfiable classes (reasoner=R, N node(s))
or a per-node block listing the offending IRIs (with labels when
the ELOT DB has them)."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available — set `elot-robot-jar-path'"))
        (let* ((r (downcase (or reasoner "hermit"))))
          (unless (member r elot-gptel--reasoners)
            (user-error
             "elot-gptel: unknown reasoner %S (use %s)"
             reasoner
             (mapconcat #'identity elot-gptel--reasoners "|")))
          (let ((true-file (elot-gptel--resolve-file file)))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((nodes (elot-gptel--tangle-ontologies-to-workspace
                              true-file ws))
                      (results (mapcar
                                (lambda (n)
                                  (elot-gptel--reason-node n r))
                                nodes))
                      (all-ok (cl-every (lambda (res)
                                          (eq (plist-get res :kind) :ok))
                                        results)))
                 (if all-ok
                     (format "OK: no unsatisfiable classes (reasoner=%s, %d node%s)"
                             r (length results)
                             (if (= (length results) 1) "" "s"))
                   (mapconcat
                    (lambda (res)
                      (let ((name (plist-get res :name))
                            (msgs (plist-get res :messages)))
                        (concat
                         (format "Ontology %s (reasoner=%s):\n" name r)
                         (if msgs
                             (mapconcat #'identity msgs "\n")
                           "  OK"))))
                    results
                    "\n\n"))))
             "elot-reason-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_explain tool (Milestone 5 Step 5.3)
;;; ---------------------------------------------------------------------------
;;
;; Run ROBOT's `explain' subcommand to surface a justification for
;; why a given axiom is (or is not) entailed by the ontology.  Same
;; tangle-into-workspace pipeline as `elot_unsatisfiable' and
;; `elot_consistency'; output is the Markdown explanation ROBOT
;; writes to `--explanation FILE'.
;;
;; This tool is read-only with respect to the user's project:
;; ROBOT only writes inside the temporary workspace, which is
;; deleted on exit.  It is therefore *not* gated behind
;; `elot-gptel-allow-side-effects'.  Wall-clock cost is bounded by
;; `elot-gptel-explain-timeout' (default 60 s); pathological calls
;; abort cleanly via the M2 timeout mechanism.

(defcustom elot-gptel-explain-timeout 60
  "Wall-clock budget for `elot_explain', in seconds.
The ROBOT call is aborted (SIGTERM, then SIGKILL after
`elot-robot-timeout-grace') when it overruns this budget; the
tool then returns a single `ERROR:' line citing the timeout.
Set to 0 to disable the timeout."
  :type 'number
  :group 'elot-gptel)

(defcustom elot-gptel-explain-max 1
  "Default value for `robot explain --max NUM' used by `elot_explain'.
Limits the number of distinct justifications ROBOT returns.
The tool argument MAX overrides this for one call."
  :type 'integer
  :group 'elot-gptel)

(defconst elot-gptel--explain-axiom-curie-re
  "\\b\\([A-Za-z_][A-Za-z0-9_.-]*\\):\\([A-Za-z_][A-Za-z0-9_.-]*\\)\\b"
  "Regex matching CURIE-shaped tokens like `ex:dog' or `owl:Thing'.
The local-name part must start with a letter or underscore, so
Manchester keywords such as `SubClassOf:' (followed by whitespace
or a non-word character) do not match.")

(defconst elot-gptel--explain-axiom-iri-re
  "<\\([^>\n]+\\)>"
  "Regex matching angle-bracket IRIs in a Manchester axiom string.")

(defconst elot-gptel--explain-keep-as-is
  '("owl:Thing" "owl:Nothing"
    "owl:topObjectProperty" "owl:bottomObjectProperty"
    "owl:topDataProperty" "owl:bottomDataProperty"
    "rdfs:Literal")
  "CURIEs whose names ROBOT's `--axiom' parser handles natively.
These are passed through without label translation.")

(defun elot-gptel--axiom-quote-label (label)
  "Return LABEL in a Manchester-safe form.
Wraps LABEL in single quotes when it contains whitespace or
characters that would otherwise tokenise oddly; backslash-escapes
embedded single quotes."
  (if (string-match-p "[][ \t\"'\\\\(){},]" label)
      (format "'%s'"
              (replace-regexp-in-string "'" "\\\\'" label t t))
    label))

(defun elot-gptel--slurp-label-context (file)
  "Return (LABEL-HT . PREFIX-ALIST) for FILE based on `elot-slurp'.

Visits FILE (without selecting it), makes sure
`elot-slurp-to-vars' has populated the buffer-local
`elot-codelist-ht' (CURIE -> rdfs:label) and that
`org-link-abbrev-alist-local' (prefix -> namespace IRI) is up
to date, and returns the pair.  Both halves may be nil when the
buffer is empty / not an ELOT file -- callers must treat that as
\"no map available\" and pass tokens through unchanged.

This is the simple in-buffer alternative to round-tripping
through the ELOT label DB (`elot-db-get-label-any' /
`elot-db-contract-uri'): `elot-slurp' is *the* map of resources
declared in the buffer, and the prefix table sitting next to it
is enough to contract a full IRI back to its CURIE form."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (unless (and (bound-and-true-p elot-codelist-ht)
                   (hash-table-p elot-codelist-ht)
                   (> (hash-table-count elot-codelist-ht) 0))
        (when (fboundp 'elot-slurp-to-vars)
          (condition-case _ (elot-slurp-to-vars) (error nil))))
      (cons (and (hash-table-p (bound-and-true-p elot-codelist-ht))
                 elot-codelist-ht)
            (and (boundp 'org-link-abbrev-alist-local)
                 (copy-sequence org-link-abbrev-alist-local))))))

(defun elot-gptel--contract-iri (iri prefixes)
  "Return the CURIE form of IRI under PREFIXES, or nil on no hit.
PREFIXES is an alist of (PREFIX . NAMESPACE-IRI) entries, as
stored in `org-link-abbrev-alist-local'.  Picks the
longest-namespace match so nested prefixes (e.g. `ex:' under
`exo:') do not shadow more specific ones."
  (let ((best-pre nil) (best-len -1))
    (dolist (p prefixes)
      (let ((pre (car p)) (ns (cdr p)))
        (when (and (stringp ns)
                   (string-prefix-p ns iri)
                   (> (length ns) best-len))
          (setq best-pre pre best-len (length ns)))))
    (and best-pre
         (concat best-pre ":" (substring iri best-len)))))

(defun elot-gptel--axiom-translate-token (token label-ht prefixes)
  "Try to translate TOKEN (a CURIE or `<IRI>') to a Manchester label.
LABEL-HT is a CURIE -> rdfs:label hash-table (typically
`elot-codelist-ht' from `elot-slurp-to-vars'); PREFIXES is an
alist of (PREFIX . NAMESPACE-IRI) entries (typically
`org-link-abbrev-alist-local').

Returns the translated string when LABEL-HT contains a non-empty
label for the (possibly contracted) token; otherwise returns
TOKEN unchanged so ROBOT's parser can report a meaningful
diagnostic on the original input."
  (cond
   ((member token elot-gptel--explain-keep-as-is) token)
   ((null label-ht) token)
   (t
    (let* ((bracketed (and (>= (length token) 2)
                           (eq (aref token 0) ?<)
                           (eq (aref token (1- (length token))) ?>)))
           (key (if bracketed
                    (let ((iri (substring token 1 -1)))
                      (or (elot-gptel--contract-iri iri prefixes) iri))
                  token))
           (lbl (gethash key label-ht)))
      (if (and lbl (stringp lbl) (not (string-empty-p lbl)))
          (elot-gptel--axiom-quote-label lbl)
        token)))))

(defun elot-gptel--axiom-translate (axiom label-ht prefixes)
  "Rewrite AXIOM, substituting known IRIs/CURIEs with their rdfs:label.

ROBOT's `--axiom' Manchester parser resolves names through the
ontology's short-form provider, which by default is label-based.
CURIEs (`ex:dog') and angle-bracket IRIs (`<http://...>') are
therefore not accepted as bare class/property names.  This helper
substitutes them with the corresponding `rdfs:label' from the
buffer's `elot-slurp'-derived map LABEL-HT (with PREFIXES used to
contract a full IRI back to its CURIE form), single-quoting
labels that need it.  Tokens with no known label are left
unchanged so ROBOT's own diagnostic remains informative.

Returns a cons (NEW-AXIOM . SUBSTITUTIONS) where SUBSTITUTIONS is
an alist of (ORIGINAL . REPLACEMENT) pairs actually applied."
  (let ((subs '())
        (result axiom))
    (setq result
          (replace-regexp-in-string
           elot-gptel--explain-axiom-iri-re
           (lambda (m)
             (let ((new (elot-gptel--axiom-translate-token
                         m label-ht prefixes)))
               (unless (equal new m) (push (cons m new) subs))
               new))
           result t t))
    (setq result
          (replace-regexp-in-string
           elot-gptel--explain-axiom-curie-re
           (lambda (m)
             (let ((new (elot-gptel--axiom-translate-token
                         m label-ht prefixes)))
               (unless (equal new m) (push (cons m new) subs))
               new))
           result t t))
    (cons result (nreverse subs))))

(defun elot-gptel--explain-format-subs (subs)
  "Render SUBS as a short NOTE block; return empty string when nil."
  (if (null subs)
      ""
    (concat "NOTE: input axiom rewritten for ROBOT \
(CURIE/IRI -> rdfs:label):\n"
            (mapconcat (lambda (s)
                         (format "  %s -> %s" (car s) (cdr s)))
                       subs "\n")
            "\n\n")))

(defun elot-gptel--explain-augment (md)
  "Append a disambiguation hint when MD contains \"No explanations found.\".

ROBOT prints that sentence when its reasoner could not produce a
justification: either the axiom is not entailed at all, or it is
entailed only trivially (most commonly because the ontology is
inconsistent, in which case every axiom follows).  The LLM cannot
tell those cases apart from MD alone -- the hint points it at
`elot_consistency' / `elot_unsatisfiable'."
  (if (and (stringp md)
           (string-match-p "No explanations found" md))
      (concat (string-trim-right md)
              "\n\nHINT: ROBOT found no justification for this axiom.  \
Either the axiom is not entailed by the ontology, or it is entailed \
only trivially (e.g. because the ontology is inconsistent, in which \
case every axiom follows).  Run `elot_consistency' and \
`elot_unsatisfiable' to disambiguate.\n")
    md))

(defun elot-gptel--explain-node (node reasoner axiom max label-ht prefixes)
  "Run `robot explain' against NODE with REASONER, AXIOM, MAX.
LABEL-HT and PREFIXES are the `elot-slurp'-derived CURIE -> label
map and prefix alist used to rewrite AXIOM for ROBOT's
label-based short-form provider; see `elot-gptel--axiom-translate'.
Returns a plist (:name NAME :exit N :timed-out BOOL :explanation TEXT
:messages (...) :subs ALIST).  EXPLANATION is the (augmented)
Markdown ROBOT wrote to its explanation file, or empty when ROBOT
did not produce one.  SUBS records any CURIE/IRI -> label
rewrites this helper applied to AXIOM before calling ROBOT."
  (let* ((name     (plist-get node :name))
         (omn-path (plist-get node :path))
         (ws-dir   (file-name-directory omn-path))
         (out-md   (expand-file-name (concat name "-explanation.md")
                                     ws-dir))
         (trans    (elot-gptel--axiom-translate axiom label-ht prefixes))
         (axiom*   (car trans))
         (subs     (cdr trans))
         (catalog-args (elot-gptel--catalog-args
                        (plist-get node :org-file)))
         (args     (append catalog-args
                           (list "explain"
                                 "--input"       omn-path
                                 "--reasoner"    reasoner
                                 "--axiom"       axiom*
                                 "--max"         (number-to-string max)
                                 "--explanation" out-md)))
         (timeout  (and (numberp elot-gptel-explain-timeout)
                        (> elot-gptel-explain-timeout 0)
                        elot-gptel-explain-timeout))
         (res      (if timeout
                       (elot-robot-run args :timeout timeout)
                     (elot-robot-run args)))
         (exit     (plist-get res :exit))
         (timed    (plist-get res :timed-out))
         (msgs     '())
         (md       ""))
    (when (file-readable-p out-md)
      (with-temp-buffer
        (insert-file-contents out-md)
        (setq md (buffer-substring-no-properties
                  (point-min) (point-max)))))
    (setq md (elot-gptel--explain-augment md))
    (cond
     (timed
      (push (format "ERROR: ROBOT explain exceeded %s s budget for %s"
                    timeout name)
            msgs))
     ((and (numberp exit) (zerop exit))
      (when (string-empty-p (string-trim md))
        (push (format "(no explanation produced for ontology %s -- \
the axiom may be neither entailed nor a direct cause of \
inconsistency)" name)
              msgs)))
     (t
      (let* ((kp (elot-robot-classify-result res))
             (out-raw (or (plist-get res :stdout) ""))
             (err-raw (or (plist-get res :stderr) "")))
        (push (format "ROBOT explain failed for ontology %s (kind=%s)"
                      name (car kp))
              msgs)
        (unless (string-empty-p (string-trim err-raw))
          (push (string-trim err-raw) msgs))
        (when (and (string-empty-p (string-trim err-raw))
                   (not (string-empty-p (string-trim out-raw))))
          (push (string-trim out-raw) msgs)))))
    (list :name        name
          :exit        exit
          :timed-out   (and timed t)
          :explanation md
          :messages    (nreverse msgs)
          :subs        subs)))

(defun elot-gptel-tool-explain (file axiom &optional reasoner max)
  "Implementation of the `elot_explain' tool.

FILE is an ELOT .org file (each ontology node is tangled to OMN
inside a temporary workspace).  AXIOM is a single Manchester
Syntax axiom whose entailment is to be explained.

ROBOT's `--axiom' parser resolves class/property names through
the ontology's short-form provider, which by default is
*label-based*.  Bare CURIEs (`ex:Dog') and angle-bracket IRIs
(`<http://example.org/dog>') are therefore *not* accepted as
class or property names by `robot explain'.  This tool rewrites
the axiom on the fly: any CURIE or IRI it can resolve to a known
`rdfs:label' via the buffer's `elot-slurp' map (CURIE -> label
in `elot-codelist-ht'; full IRIs contracted through
`org-link-abbrev-alist-local') is substituted with the label
(single-quoted when needed); tokens with no known label are left
unchanged.  The substitution is reported as a NOTE
prepended to the result so the caller can see what was actually
sent to ROBOT.  Examples that work out of the box (case-sensitive
on the label):

  \"Dog SubClassOf: Animal\"           (labels written directly)
  \"ex:dog SubClassOf: ex:animal\"     (CURIEs auto-translated)
  \"<http://example.org/dog> SubClassOf: <http://example.org/animal>\"
  \"owl:Thing SubClassOf: owl:Nothing\" (passthrough -- inconsistency probe)
  \"Foo SubClassOf: owl:Nothing\"      (unsatisfiability probe)

REASONER is one of \"hermit\" (default) or \"whelk\" (ROBOT's
explain command also accepts \"elk\"; passed through verbatim
when given, otherwise validated against `elot-gptel--reasoners').
MAX is the maximum number of distinct justifications to return
(default `elot-gptel-explain-max').

Returns the Markdown explanation ROBOT produces, framed
per-ontology when the file declares more than one ontology node.
When ROBOT prints \"No explanations found.\" the tool appends a
HINT pointing the caller at `elot_consistency' /
`elot_unsatisfiable' to disambiguate the two trivial cases
(not entailed vs. entailed by inconsistency).  On a wall-clock
overrun (`elot-gptel-explain-timeout', default 60 s) the tool
returns a single `ERROR:' line citing the budget.

This tool is read-only with respect to the user's project: the
ROBOT invocation writes only into a temporary workspace that is
deleted on exit."
  (condition-case err
      (progn
        (require 'elot-robot)
        (unless (elot-robot-available-p)
          (user-error
           "elot-gptel: ROBOT not available -- set `elot-robot-jar-path'"))
        (unless (and axiom (stringp axiom) (not (string-empty-p (string-trim axiom))))
          (user-error "elot-gptel: missing or empty axiom argument"))
        (let* ((r-in (and reasoner (downcase reasoner)))
               (r    (or r-in "hermit")))
          ;; Allow `elk' as a pass-through for ROBOT's explain default;
          ;; reject anything else not in `elot-gptel--reasoners'.
          (unless (or (member r elot-gptel--reasoners)
                      (equal r "elk"))
            (user-error
             "elot-gptel: unknown reasoner %S (use %s or elk)"
             reasoner
             (mapconcat #'identity elot-gptel--reasoners "|")))
          (let* ((true-file (elot-gptel--resolve-file file))
                 (m (or max elot-gptel-explain-max))
                 (ctx       (elot-gptel--slurp-label-context true-file))
                 (label-ht  (car ctx))
                 (prefixes  (cdr ctx)))
            (unless (and (integerp m) (>= m 1))
              (user-error "elot-gptel: max must be a positive integer"))
            (elot-robot-call-with-workspace
             (lambda (ws)
               (let* ((nodes   (elot-gptel--tangle-ontologies-to-workspace
                                true-file ws))
                      (results (mapcar
                                (lambda (n)
                                  (elot-gptel--explain-node
                                   n r axiom m label-ht prefixes))
                                nodes))
                      (any-timeout (cl-some (lambda (res)
                                              (plist-get res :timed-out))
                                            results)))
                 (cond
                  (any-timeout
                   (mapconcat
                    (lambda (res)
                      (mapconcat #'identity
                                 (plist-get res :messages) "\n"))
                    (cl-remove-if-not
                     (lambda (res) (plist-get res :timed-out))
                     results)
                    "\n"))
                  ((= 1 (length results))
                   (let* ((res  (car results))
                          (md   (plist-get res :explanation))
                          (msgs (plist-get res :messages))
                          (subs (plist-get res :subs))
                          (note (elot-gptel--explain-format-subs subs)))
                     (cond
                      ((not (string-empty-p (string-trim md)))
                       (concat note md))
                      (msgs (concat note
                                    (mapconcat #'identity msgs "\n")))
                      (t (concat note
                                 (format "OK: no explanation produced \
(reasoner=%s, axiom=%s)" r axiom))))))
                  (t
                   (mapconcat
                    (lambda (res)
                      (let* ((name (plist-get res :name))
                             (md   (plist-get res :explanation))
                             (msgs (plist-get res :messages))
                             (subs (plist-get res :subs))
                             (note (elot-gptel--explain-format-subs subs)))
                        (concat
                         (format "Ontology %s (reasoner=%s):\n" name r)
                         note
                         (cond
                          ((not (string-empty-p (string-trim md))) md)
                          (msgs (mapconcat #'identity msgs "\n"))
                          (t "  (no explanation)")))))
                    results
                    "\n\n")))))
             "elot-explain-"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_db_query tool (Milestone 6 Step 6.2)
;;; ---------------------------------------------------------------------------

(defconst elot-gptel--db-query-default-limit 200
  "Default LIMIT applied by `elot-gptel-tool-db-query' when unset.")

(defconst elot-gptel--db-query-max-limit 5000
  "Hard ceiling on the LIMIT argument of `elot-gptel-tool-db-query'.")

(defun elot-gptel--db-cell-to-string (cell)
  "Render a SQLite CELL value as a TSV-safe string.
nil becomes the empty string; numbers are printed via `prin1';
strings have embedded tabs/newlines escaped so the row grid is
preserved."
  (let ((s (cond
            ((null cell) "")
            ((stringp cell) cell)
            (t (prin1-to-string cell)))))
    (setq s (replace-regexp-in-string "\r\\'" "" s))
    (setq s (replace-regexp-in-string "\t" " " s))
    (setq s (replace-regexp-in-string "[\r\n]+" " / " s))
    s))

(defun elot-gptel--db-format-rows (header rows limit)
  "Render HEADER (list of column names) and ROWS as a TSV string.
Caps the data rows at LIMIT and appends a
`... N more rows omitted' trailer when truncation happens.
HEADER may be nil; in that case the result omits the header
line."
  (let* ((head-line
          (and header
               (mapconcat #'elot-gptel--db-cell-to-string header "\t")))
         (total (length rows))
         (kept  (if (and (integerp limit) (>= limit 0) (> total limit))
                    (cl-subseq rows 0 limit)
                  rows))
         (drop  (- total (length kept)))
         (body  (mapconcat
                 (lambda (row)
                   (mapconcat #'elot-gptel--db-cell-to-string row "\t"))
                 kept "\n")))
    (concat
     (when head-line (concat head-line "\n"))
     body
     (when (> drop 0)
       (format "%s... %d more row%s omitted"
               (if (string-empty-p body) "" "\n")
               drop (if (= drop 1) "" "s"))))))

(defun elot-gptel--db-ensure-open ()
  "Open the ELOT label DB if no connection is active.
Calls `elot-db-init' with its default path when `elot-db' is
nil or not a live SQLite handle.  Errors propagate to the
caller as `user-error' (so the tool wrapper can format them)."
  (require 'elot-db)
  (unless (and (boundp 'elot-db) elot-db (sqlitep elot-db))
    (elot-db-init)))

(defun elot-gptel-tool-db-query (sql &optional limit)
  "Run a read-only SQL query against the ELOT label DB.

SQL must be a SELECT or WITH ... SELECT statement; anything
else is refused by `elot-db-execute-readonly' (which also pins
`PRAGMA query_only = 1' for the duration of the call).

LIMIT caps the number of data rows returned (default
`elot-gptel--db-query-default-limit', max
`elot-gptel--db-query-max-limit').  When the cap fires, a
`... N more rows omitted' trailer is appended.

Returns a TSV string with a header row, or an `ERROR:' line
when the gate refuses the query or the DB is unavailable."
  (condition-case err
      (let* ((lim (cond
                   ((null limit) elot-gptel--db-query-default-limit)
                   ((not (integerp limit))
                    (user-error
                     "elot-gptel: limit must be an integer"))
                   ((< limit 0)
                    (user-error
                     "elot-gptel: limit must be non-negative"))
                   ((> limit elot-gptel--db-query-max-limit)
                    elot-gptel--db-query-max-limit)
                   (t limit))))
        (elot-gptel--db-ensure-open)
        (let* ((full (elot-db-execute-readonly sql nil 'full))
               (header (car full))
               (rows   (cdr full)))
          (if (and (null rows) (null header))
              "OK: query returned no rows"
            (elot-gptel--db-format-rows header rows lim))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_db_schema tool (Milestone 6 Step 6.3)
;;; ---------------------------------------------------------------------------

(declare-function elot-db--read-schema-sql "elot-db" ())
(defvar elot-db-schema-version)

(defun elot-gptel--db-schema-version ()
  "Return the integer value of `schema_version' from the open DB, or nil.
Uses the read-only gate so the call is safe even if a future
caller has invoked this from an unexpected context."
  (condition-case nil
      (let ((row (car (elot-db-execute-readonly
                       "SELECT version FROM schema_version LIMIT 1"))))
        (and row (car row)))
    (error nil)))

(defun elot-gptel-tool-db-schema ()
  "Return the ELOT label DB schema as a Markdown report.

The report consists of three blocks: the code-side schema
version constant (`elot-db-schema-version'); the stored
`schema_version' row read from the open database (via the
read-only gate); and the full DDL from the canonical
`schema.sql' file, fenced as a ```sql``` code block.

Lets the LLM compose correct queries for `elot_db_query'
without hard-coding the table layout in the system prompt.
Read-only."
  (condition-case err
      (progn
        (require 'elot-db)
        (elot-gptel--db-ensure-open)
        (let* ((code-ver (and (boundp 'elot-db-schema-version)
                              elot-db-schema-version))
               (stored   (elot-gptel--db-schema-version))
               (ddl      (elot-db--read-schema-sql)))
          (concat
           (format "schema_version (code):   %s\n"
                   (or code-ver "(unknown)"))
           (format "schema_version (stored): %s\n\n"
                   (or stored "(unknown)"))
           "```sql\n"
           (or ddl "")
           (if (and ddl (string-suffix-p "\n" ddl)) "" "\n")
           "```")))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; Convenience shims (Milestone 6 Step 6.4 -- Wave A)
;;; ---------------------------------------------------------------------------

(declare-function elot-db-get-label-any "elot-db" (token &optional active-sources))
(declare-function elot-db-list-sources "elot-db" ())
(declare-function elot-db-list-sources-like "elot-db"
                  (source-pattern &optional data-source-pattern))
(declare-function elot-db-source-exists-p "elot-db"
                  (source &optional data-source))
(declare-function elot-db-remove-source "elot-db"
                  (source &optional data-source like allow-all))
(declare-function elot-db-expand-curie "elot-db" (curie &optional active-sources))
(defvar elot-active-label-sources)      ; from elot-db.el; buffer-local at use sites

(defun elot-gptel--db-iri-from-bracketed (token)
  "Strip surrounding angle brackets from TOKEN, if present.
`elot-db-get-label-any' and `elot-db-expand-curie' work on the
bare IRI/CURIE form; angle brackets are a Manchester-syntax
convention that the DB layer does not see."
  (if (and (stringp token)
           (string-prefix-p "<" token)
           (string-suffix-p ">" token)
           (>= (length token) 2))
      (substring token 1 -1)
    token))

(defun elot-gptel-tool-db-get-label (token)
  "Return the best label the ELOT DB knows for TOKEN.

TOKEN may be a CURIE (=ex:dog=), a full IRI (=<http://...>= or
its bare form), or a literal `entities.id' string.  Consults
the buffer's active label sources via
`elot-db-get-label-any', then the cross-source CURIE<->IRI
prefix rewrites.

Returns a single line: the label string, or =(no label)= when
the DB has no entry for TOKEN.  Read-only."
  (condition-case err
      (progn
        (unless (and (stringp token) (not (string-empty-p token)))
          (user-error "elot-gptel: token must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let* ((bare  (elot-gptel--db-iri-from-bracketed token))
               (label (elot-db-get-label-any bare)))
          (or label "(no label)")))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-db-list-sources ()
  "Return a TSV listing of every source registered in the ELOT DB.

Columns: SOURCE, DATA-SOURCE, TYPE, LAST-MODIFIED,
LAST-UPDATED.  DATA-SOURCE is the empty string for non-SPARQL
sources (it is the secondary key for SPARQL endpoints, a file
path or endpoint URL otherwise).  No arguments.  Read-only."
  (condition-case err
      (progn
        (elot-gptel--db-ensure-open)
        (let ((rows (elot-db-list-sources)))
          (if (null rows)
              "OK: no sources registered"
            (elot-gptel--db-format-rows
             '("source" "data_source" "type"
               "last_modified" "last_updated")
             rows nil))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(declare-function elot-label-activate-source-for-file
                  "elot-sources" (file))

(defun elot-gptel-tool-db-activate-source (file)
  "Register-if-needed and activate FILE as a label source.

Wraps `elot-label-activate-source-for-file' under the
project-root containment check.  Read-only with respect to the
=.org= source: only mutates the label DB and the default value
of `elot-active-label-sources'.  Not gated by
`elot-gptel-allow-side-effects' -- this is session setup, the
same category as =set-variable=.

Closes the silent-CURIE-passthrough failure mode where
`elot_explain' / SPARQL tools see an unlabelled file because the
DB has no entries for it yet.  After this tool runs, subsequent
gptel tool calls in the same session resolve labels against the
freshly-cached source.

Returns =OK: activated FILE (status=..., N entities)= on
success, or an =ERROR:= line on refusal / parse failure."
  (condition-case err
      (progn
        (require 'elot-sources)
        (let* ((true-file (elot-gptel--resolve-file file))
               (result    (elot-label-activate-source-for-file true-file))
               (status    (plist-get result :status))
               (n         (or (plist-get result :entities) 0)))
          (format "OK: activated %s (status=%s, %d entit%s)"
                  true-file
                  (substring (symbol-name status) 1) ; drop leading ":"
                  n (if (= n 1) "y" "ies"))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-db-remove-source
    (source &optional data-source like allow-all dry-run)
  "Implementation of the `elot_db_remove_source' tool.

Deletes the matching row(s) in the ELOT label DB's `sources'
table, cascading to `entities', `attributes', and `prefixes' via
`ON DELETE CASCADE'.  See `elot-db-remove-source' for the full
semantics; this wrapper adds the gptel envelope: side-effects
gate, dry-run preview, and TSV-shaped response.

When LIKE is non-nil, SOURCE and DATA-SOURCE are SQL LIKE
patterns; DATA-SOURCE defaults to `\"%\"' (any).  ALLOW-ALL
must be supplied to permit SOURCE=`\"\"' or `\"%\"' in pattern
mode -- a guard against accidental whole-table deletion.

DRY-RUN reports the rows that would be removed without touching
the DB (preview via `elot-db-list-sources-like' in pattern mode,
`elot-db-source-exists-p' in exact mode) and bypasses the
side-effects gate.  The real delete is gated by
`elot-gptel-allow-side-effects'."
  (condition-case err
      (progn
        (unless (and (stringp source) (not (string-empty-p source)))
          (user-error "elot-gptel: source must be a non-empty string"))
        (unless (or dry-run elot-gptel-allow-side-effects)
          (user-error
           "elot-gptel: elot_db_remove_source refused -- mutating tool \
(set `elot-gptel-allow-side-effects' to t, or pass dry_run=true)"))
        (elot-gptel--db-ensure-open)
        (let* ((ds (cond ((and (stringp data-source)
                               (not (string-empty-p data-source)))
                          data-source)
                         (t nil))))
          (if dry-run
              ;; Preview only -- no DELETE issued.
              (if like
                  (let* ((rows (elot-db-list-sources-like
                                source (or ds "%"))))
                    (if (null rows)
                        (format "OK: dry-run -- 0 source(s) match %S / %S"
                                source (or ds "%"))
                      (concat
                       (format "OK: dry-run -- %d source(s) would be removed\n"
                               (length rows))
                       (elot-gptel--db-format-rows
                        '("source" "data_source" "type"
                          "last_modified" "last_updated")
                        rows nil))))
                (if (elot-db-source-exists-p source ds)
                    (format "OK: dry-run -- 1 source would be removed (%s, %s)"
                            source (or ds ""))
                  (format "OK: dry-run -- 0 source(s) match %s / %s"
                          source (or ds ""))))
            ;; Real delete.
            (let ((n (elot-db-remove-source source ds like allow-all)))
              (format "OK: removed %d source%s (%s%s%s%s)"
                      n (if (= n 1) "" "s")
                      (if like "pattern " "")
                      source
                      (if ds (concat ", " ds) "")
                      (if allow-all ", allow-all" ""))))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-db-expand-curie (curie)
  "Expand CURIE (=prefix:local=) to a full IRI via the ELOT DB.

Consults the active sources' `prefixes' tables, then the
shared `global_prefixes' table.  The default-prefix form
(=:foo=) is supported via the empty-string prefix row.

Returns the expanded IRI on success, or an =ERROR:= line when
the prefix is unknown or CURIE is malformed.  Read-only."
  (condition-case err
      (progn
        (unless (and (stringp curie) (not (string-empty-p curie)))
          (user-error "elot-gptel: curie must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let ((iri (elot-db-expand-curie curie elot-active-label-sources)))
          (or iri
              (format "ERROR: no expansion found for prefix in '%s'"
                      curie))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; Attribute-driven SQL-select shims (Milestone 6 Step 6.4 -- Wave B)
;;; ---------------------------------------------------------------------------
;;;
;;; Three dedicated SELECTs that the LLM would otherwise have to
;;; compose via `elot_db_query'.  Each is a one-purpose tool with a
;;; stable, columnar output shape; all internally call
;;; `elot-db-execute-readonly', so they inherit the M6.1 gate and the
;;; `query_only' PRAGMA discipline for free.
;;;
;;; A subtlety pinned by the plan (Step 6.4 Wave B assessment): the
;;; ELOT pipeline writes both OMN frame keywords (`SubClassOf',
;;; `SubPropertyOf', `Types') and RDF property CURIEs
;;; (`rdfs:subClassOf', `rdfs:subPropertyOf', `rdf:type') into
;;; `attributes.prop' depending on the source's ingest path.  ELOT
;;; `.org' tangle ingest uses the OMN spellings; TTL ingest uses the
;;; RDF spellings.  The supertypes / individual-types shims therefore
;;; accept BOTH spellings in their WHERE clauses so the LLM sees
;;; uniform results regardless of how a given source was loaded.

(defconst elot-gptel--db-supertype-props
  '("rdfs:subClassOf"
    "rdfs:subPropertyOf"
    "SubClassOf"
    "SubPropertyOf")
  "Property names treated as supertype assertions by `elot_db_supertypes'.
The first two are the RDF-property CURIE form (used by TTL/SPARQL
ingest); the last two are the OMN frame-keyword form (used by ELOT
`.org' tangle ingest).  Both are queried so a single shim works
uniformly across ingest paths.")

(defconst elot-gptel--db-type-props
  '("rdf:type" "Types")
  "Property names treated as type assertions on individuals.
`rdf:type' is the RDF-property CURIE form; `Types' is the OMN
frame-keyword form.")

(defun elot-gptel--db-shim-format (header rows)
  "Render HEADER + ROWS as a TSV report; OK-line when ROWS is empty."
  (if (null rows)
      "OK: no rows"
    (elot-gptel--db-format-rows header rows nil)))

(defun elot-gptel-tool-db-get-attributes (id &optional source)
  "Return all (prop, value, lang, source) rows for entity ID.

When SOURCE is non-nil, restrict the SELECT to that single
source row; otherwise list rows from every source.  ID must
be a string in the form ELOT stores in `entities.id' -- either
a CURIE (=ex:dog=) or a full IRI -- matching exactly how the
source ingested it.  Use `elot_db_get_label' or
`elot_db_expand_curie' first if you are not sure which form a
source uses.

Returns a TSV with columns `prop', `value', `lang', `source',
`data_source', or =OK: no rows= when nothing is stored.

Read-only; uses `elot-db-execute-readonly' so the call goes
through the M6.1 read-only gate."
  (condition-case err
      (progn
        (unless (and (stringp id) (not (string-empty-p id)))
          (user-error "elot-gptel: id must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let* ((sql (if source
                        "SELECT prop, value, lang, source, data_source
                           FROM attributes
                          WHERE id = ? AND source = ?
                          ORDER BY source, prop, value"
                      "SELECT prop, value, lang, source, data_source
                         FROM attributes
                        WHERE id = ?
                        ORDER BY source, prop, value"))
               (params (if source (list id source) (list id)))
               (rows (elot-db-execute-readonly sql params)))
          (elot-gptel--db-shim-format
           '("prop" "value" "lang" "source" "data_source")
           rows)))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-db-supertypes (id &optional source)
  "Return direct supertypes of ID across all registered sources.

Selects `value' from `attributes' where `prop' is one of
`rdfs:subClassOf' / `rdfs:subPropertyOf' (RDF spellings used by
TTL ingest) or `SubClassOf' / `SubPropertyOf' (OMN spellings
used by ELOT `.org' tangle ingest).  Both spellings are
queried so the result is uniform regardless of how a given
source was ingested.

When SOURCE is non-nil, restrict the SELECT to that single
source.  Returns a TSV with columns `value', `prop', `source',
`data_source', or =OK: no rows= when nothing is stored.  ID
must match exactly the form stored in `entities.id' for the
source(s) of interest.  Read-only."
  (condition-case err
      (progn
        (unless (and (stringp id) (not (string-empty-p id)))
          (user-error "elot-gptel: id must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let* ((placeholders
                (mapconcat (lambda (_) "?")
                           elot-gptel--db-supertype-props
                           ", "))
               (sql (if source
                        (format
                         "SELECT value, prop, source, data_source
                            FROM attributes
                           WHERE id = ?
                             AND prop IN (%s)
                             AND source = ?
                           ORDER BY source, prop, value"
                         placeholders)
                      (format
                       "SELECT value, prop, source, data_source
                          FROM attributes
                         WHERE id = ?
                           AND prop IN (%s)
                         ORDER BY source, prop, value"
                       placeholders)))
               (params (append (list id)
                               elot-gptel--db-supertype-props
                               (and source (list source))))
               (rows (elot-db-execute-readonly sql params)))
          (elot-gptel--db-shim-format
           '("value" "prop" "source" "data_source")
           rows)))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-db-individual-types (id &optional source)
  "Return the asserted types of ID, scoped to individuals.

Selects `value' from `attributes' where `prop' is one of
`rdf:type' (RDF spelling used by TTL ingest) or `Types' (OMN
spelling used by ELOT `.org' ingest), excluding the
`owl:NamedIndividual' marker value.  Filters out class- and
property-level rows via a guard: the result is non-empty only
when ID has either (a) a `Types' row -- the OMN frame-keyword
indicator that the row describes an individual -- or (b) a
matching `rdf:type owl:NamedIndividual' attestation in the
same source.  Without that guard, classes (which carry
`rdf:type owl:Class') would be reported as having type
`owl:Class', which is not what the LLM wants when asking
\"what classes does this individual inhabit?\".

When SOURCE is non-nil, restrict the SELECT to that single
source.  Returns a TSV with columns `value', `prop', `source',
`data_source', or =OK: no rows= when ID is not an individual
or has no asserted type.  Read-only."
  (condition-case err
      (progn
        (unless (and (stringp id) (not (string-empty-p id)))
          (user-error "elot-gptel: id must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let* ((type-placeholders
                (mapconcat (lambda (_) "?")
                           elot-gptel--db-type-props
                           ", "))
               (sql (if source
                        (format
                         "SELECT DISTINCT value, prop, source, data_source
                            FROM attributes a1
                           WHERE id = ?
                             AND prop IN (%s)
                             AND value != 'owl:NamedIndividual'
                             AND source = ?
                             AND (
                               EXISTS (SELECT 1 FROM attributes a2
                                        WHERE a2.id = a1.id
                                          AND a2.source = a1.source
                                          AND a2.data_source = a1.data_source
                                          AND a2.prop = 'Types')
                               OR EXISTS (SELECT 1 FROM attributes a3
                                           WHERE a3.id = a1.id
                                             AND a3.source = a1.source
                                             AND a3.data_source = a1.data_source
                                             AND a3.prop = 'rdf:type'
                                             AND a3.value = 'owl:NamedIndividual')
                             )
                           ORDER BY source, prop, value"
                         type-placeholders)
                      (format
                       "SELECT DISTINCT value, prop, source, data_source
                          FROM attributes a1
                         WHERE id = ?
                           AND prop IN (%s)
                           AND value != 'owl:NamedIndividual'
                           AND (
                             EXISTS (SELECT 1 FROM attributes a2
                                      WHERE a2.id = a1.id
                                        AND a2.source = a1.source
                                        AND a2.data_source = a1.data_source
                                        AND a2.prop = 'Types')
                             OR EXISTS (SELECT 1 FROM attributes a3
                                         WHERE a3.id = a1.id
                                           AND a3.source = a1.source
                                           AND a3.data_source = a1.data_source
                                           AND a3.prop = 'rdf:type'
                                           AND a3.value = 'owl:NamedIndividual')
                           )
                         ORDER BY source, prop, value"
                       type-placeholders)))
               (params (append (list id)
                               elot-gptel--db-type-props
                               (and source (list source))))
               (rows (elot-db-execute-readonly sql params)))
          (elot-gptel--db-shim-format
           '("value" "prop" "source" "data_source")
           rows)))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_db_search_label tool (Milestone 6 Step 6.5 -- reuse-before-mint)
;;; ---------------------------------------------------------------------------
;;;
;;; First half of M6.5: search the *entire* DB (not just active
;;; sources) for entities whose label or id matches a substring,
;;; optionally filtered by kind / source / language.  The LLM
;;; consults this BEFORE calling `elot_mint_identifier' so reuse of
;;; existing identifiers is preferred over fresh minting.
;;;
;;; The second M6.5 tool (`elot_db_borrow_term') will produce an
;;; ELOT description-list snippet -- including the `rdfs:isDefinedBy'
;;; back-pointer -- to be embedded under a heading via `elot_add_term'.

(declare-function elot-db-search-entities "elot-db"
                  (query &optional limit kind source lang exact-only))

(defconst elot-gptel--db-search-default-limit 50
  "Default LIMIT for `elot-gptel-tool-db-search-label'.")

(defconst elot-gptel--db-search-max-limit 500
  "Hard ceiling on the LIMIT argument of `elot-gptel-tool-db-search-label'.")

(defun elot-gptel-tool-db-search-label (query &optional kind source lang limit exact-only)
  "Search the ELOT label DB for entities matching QUERY.

QUERY is a non-empty string matched case-insensitively against
both `entities.label' and `entities.id'.  Bare strings are
wrapped as `%QUERY%' (substring match); pass a string already
containing SQL `%' wildcards if you want explicit LIKE
semantics.

KIND, when non-nil, restricts results to one of:
`class', `object-property', `data-property',
`annotation-property', `individual', `datatype', `ontology'.

SOURCE restricts to a single registered source name (see
`elot_db_list_sources').  LANG requires at least one
`rdfs:label' row with the given language tag.

LIMIT caps the number of returned rows (default
`elot-gptel--db-search-default-limit', max
`elot-gptel--db-search-max-limit').

EXACT-ONLY, when non-nil, suppresses the M11.1 cross-prefix
local-name fallback.  By default, when QUERY looks like a CURIE
(`prefix:localname') and the direct prefix:local lookup misses,
the search retries against the local-name part alone, matched
against `substr(id, instr(id, ':')+1)' (the local-name of any
stored id) and against the label column.  This catches the
case where an LLM asks for `ex:Margherita' but the source
stores the entity under the default prefix as `:Margherita'.

Returns a TSV with columns
  id, label, kind, ontology_iri, source, data_source, via

VIA is one of `exact' (id matched the query exactly),
`label-substring' (a LIKE-substring hit on label or id), or
`local-name' (the cross-prefix fallback).  Rows with VIA other
than `exact' may be imperfect matches -- inspect before reuse.

KIND (column) is the entity's asserted `rdf:type' CURIE (or
empty when unknown); ONTOLOGY_IRI is the id of the same
source's `owl:Ontology' declaration -- the citation target for
an `rdfs:isDefinedBy' back-pointer when the LLM borrows the
term into a new ontology (see `elot_db_borrow_term').

Read-only; uses `elot-db-execute-readonly' under the M6.1
read-only gate.  The whole DB is searched, *not* just the
buffer's active label sources -- the cache accumulates every
ontology you have ever registered, and any of those is a
candidate for reuse before minting a fresh identifier."
  (condition-case err
      (let* ((lim (cond
                   ((null limit) elot-gptel--db-search-default-limit)
                   ((not (integerp limit))
                    (user-error
                     "elot-gptel: limit must be an integer"))
                   ((< limit 0)
                    (user-error
                     "elot-gptel: limit must be non-negative"))
                   ((> limit elot-gptel--db-search-max-limit)
                    elot-gptel--db-search-max-limit)
                   ((= limit 0) elot-gptel--db-search-default-limit)
                   (t limit)))
             (kind* (and (stringp kind) (not (string-empty-p kind))
                         kind))
             (source* (and (stringp source) (not (string-empty-p source))
                           source))
             (lang* (and (stringp lang) (not (string-empty-p lang))
                         lang))
             (exact-only* (and exact-only t)))
        (unless (and (stringp query) (not (string-empty-p query)))
          (user-error "elot-gptel: query must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let ((rows (elot-db-search-entities
                     query lim kind* source* lang* exact-only*)))
          (elot-gptel--db-shim-format
           '("id" "label" "kind" "ontology_iri"
             "source" "data_source" "via")
           rows)))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_db_borrow_term tool (Milestone 6 Step 6.5 -- second half)
;;; ---------------------------------------------------------------------------
;;;
;;; Given an entity TOKEN (CURIE or IRI) known to the ELOT DB,
;;; return a plain-text ELOT description-list snippet that the
;;; LLM (or M9.3's `elot_add_term') can embed under a heading.
;;; The snippet ALWAYS carries an `rdfs:isDefinedBy' back-pointer
;;; to the source ontology IRI -- the citation is part of the
;;; tool's output shape, not an instruction in its docstring, so
;;; the LLM mechanically cannot forget the attribution.
;;;
;;; The level-1 `*' in the heading is a deliberate placeholder.
;;; The caller is responsible for re-leveling it to match the
;;; target ontology's structure (typically under a level-2
;;; `:resourcedefs: yes' heading, or as a child of an existing
;;; resource heading).

(declare-function elot-db-entity-citation "elot-db" (token))
(declare-function elot-db-list-prefixes "elot-db" (&optional source data-source))

(defun elot-gptel--db-borrow-format-label (label lang)
  "Quote LABEL for an ELOT heading, appending @LANG when given."
  (let ((q (concat "\"" (or label "") "\"")))
    (if (and lang (stringp lang) (not (string-empty-p lang)))
        (concat q "@" lang)
      q)))

(defun elot-gptel--db-borrow-default-prefix-p (id)
  "Return non-nil when ID is a default-prefix CURIE like `:Food'.
A default-prefix CURIE has the empty prefix on the left of `:'.
Bare full IRIs and regular CURIEs (`pizza:Food') return nil."
  (and (stringp id)
       (not (string-empty-p id))
       (eq (aref id 0) ?:)
       ;; Reject `::' (two colons -- not a CURIE shape).
       (not (and (> (length id) 1) (eq (aref id 1) ?:)))))

(defun elot-gptel--db-borrow-label-missing-p (label id)
  "Return non-nil when LABEL is absent or equals ID (no human label).
The DB stores the id echoed as the label when no `rdfs:label' was
found in the source; in that case the heading carries no real
human-readable name and the borrower should supply one."
  (or (null label)
       (string-empty-p label)
       (string= label id)))

(defun elot-gptel--db-borrow-default-prefix-expansion (source data-source)
  "Return the empty-prefix expansion for SOURCE / DATA-SOURCE, or nil.
Consults the per-source `prefixes' table for a row with the empty
prefix string (the default-prefix declaration).  Read-only."
  (when (and source (fboundp 'elot-db-list-prefixes))
    (let ((rows (ignore-errors
                  (elot-db-list-prefixes source data-source))))
      (catch 'hit
        (dolist (row rows)
          (when (equal "" (nth 2 row))
            (throw 'hit (nth 3 row))))))))

(defun elot-gptel--db-borrow-kind-note (rdf-type)
  "Return a NOTE line for RDF-TYPE describing its nesting role, or nil.

Step 7.5.3: teaches the ELOT heading-nesting convention through
the snippet output itself.  When the borrowed entity is a class
or one of the four property kinds, the snippet appends a NOTE
spelling out how `SubClassOf' / `SubPropertyOf' is expressed in
the borrowing ontology -- by nesting child headings under this
heading, NOT by a description-list `SubClassOf ::' row.  Returns
nil for individuals, datatypes, and unknown / missing types so
the common snippet stays compact."
  (when (stringp rdf-type)
    (pcase rdf-type
      ("owl:Class"
       (concat
        "NOTE: to express SubClassOf in the borrowing ontology, "
        "place new class headings as CHILDREN of this heading "
        "(heading nesting carries SubClassOf in ELOT; do NOT use "
        "a description-list `SubClassOf ::' row for a named parent)."))
      ("owl:ObjectProperty"
       (concat
        "NOTE: to express SubPropertyOf in the borrowing ontology, "
        "place new object-property headings as CHILDREN of this "
        "heading (heading nesting carries SubPropertyOf in ELOT)."))
      ("owl:DatatypeProperty"
       (concat
        "NOTE: to express SubPropertyOf in the borrowing ontology, "
        "place new data-property headings as CHILDREN of this "
        "heading (heading nesting carries SubPropertyOf in ELOT)."))
      ("owl:AnnotationProperty"
       (concat
        "NOTE: to express SubPropertyOf in the borrowing ontology, "
        "place new annotation-property headings as CHILDREN of this "
        "heading (heading nesting carries SubPropertyOf in ELOT)."))
      (_ nil))))

(defun elot-gptel--db-borrow-format-definition (def)
  "Quote DEF (a skos:definition value) for an ELOT description list.
DEF may already be wrapped in quotes; this function ensures
exactly one pair of surrounding quotes."
  (let* ((s (or def "")))
    (when (and (>= (length s) 2)
               (string-prefix-p "\"" s)
               (string-suffix-p "\"" s))
      (setq s (substring s 1 -1)))
    (concat "\"" s "\"")))

(defun elot-gptel--db-borrow-format (citation)
  "Render CITATION (a plist from `elot-db-entity-citation') as
an ELOT heading + description list snippet.  ASCII-only.

Step 7.5.7: emits two contingent NOTE lines when the borrowed
entity exhibits authoring rough edges.  (1) When the source uses
the default-prefix form (id starts with `:'), warn that the
borrowing ontology must add a matching prefix row.  (2) When the
stored label equals the id (or is absent), substitute a
`TODO-label' placeholder in the heading and tell the borrower
to replace it.  Both NOTEs are detected mechanically; neither
fires for a well-shaped CURIE with a real label.

Step 7.5.3: when the borrowed entity is `owl:Class' or one of
the property kinds, append a NOTE explaining that SubClassOf /
SubPropertyOf is expressed by nesting child headings under this
one.  Teaches the ELOT heading-nesting convention through the
snippet output."
  (let* ((id        (plist-get citation :id))
         (label     (plist-get citation :label))
         (lang      (plist-get citation :label-lang))
         (def       (plist-get citation :definition))
         (rdf-type  (plist-get citation :rdf-type))
         (ont-iri   (plist-get citation :ontology-iri))
         (ont-title (plist-get citation :ontology-title))
         (source    (plist-get citation :source))
         (data      (plist-get citation :data-source))
         (default-prefix-p
          (elot-gptel--db-borrow-default-prefix-p id))
         (label-missing-p
          (elot-gptel--db-borrow-label-missing-p label id))
         (heading-label
          (if label-missing-p
              "TODO-label"
            label))
         (heading-lang
          (if label-missing-p nil lang))
         (lines  nil))
    (push (format "* %s (%s)"
                  (elot-gptel--db-borrow-format-label
                   heading-label heading-lang)
                  id)
          lines)
    ;; rdfs:isDefinedBy: prefer the ontology id, fall back to source.
    (cond
     (ont-iri
      (push (format " - rdfs:isDefinedBy :: %s" ont-iri) lines))
     (source
      (push (format " - rdfs:isDefinedBy :: (source: %s)" source) lines)))
    (when def
      (push (format " - skos:definition :: %s"
                    (elot-gptel--db-borrow-format-definition def))
            lines))
    ;; Provenance notes (not part of the OWL axioms, but useful
    ;; for the LLM and the human reviewer).
    (let ((notes nil))
      (when rdf-type
        (push (format "rdf:type %s" rdf-type) notes))
      (when ont-title
        (push (format "source title: %s" ont-title) notes))
      (when source
        (push (format "source: %s" source) notes))
      (when notes
        (push (concat "  # " (mapconcat #'identity (nreverse notes) "; "))
              lines)))
    (push "" lines)
    (push (concat
           "NOTE: the leading `*' is a placeholder -- re-level the "
           "heading to match the target ontology's structure before "
           "inserting (typically under a `:resourcedefs: yes' heading "
           "or as a child of an existing resource heading).")
          lines)
    ;; Step 7.5.3 contingent NOTE: heading-nesting role by kind.
    (let ((kind-note (elot-gptel--db-borrow-kind-note rdf-type)))
      (when kind-note
        (push "" lines)
        (push kind-note lines)))
    ;; Step 7.5.7 contingent NOTE: default-prefix source.
    (when default-prefix-p
      (let* ((localname (substring id 1))
             (expansion (elot-gptel--db-borrow-default-prefix-expansion
                         source data))
             (suggested (cond
                         (ont-title
                          (downcase
                           (replace-regexp-in-string
                            "[^A-Za-z0-9]+" ""
                            (car (split-string ont-title)))))
                         (t "PREFIX"))))
        (push "" lines)
        (push (format
               (concat
                "NOTE: source uses default-prefix form `%s'; before "
                "inserting, add a matching prefix row to the borrowing "
                "ontology's prefix table (e.g. `%s:' -> %s) and use "
                "the CURIE `%s:%s' in the heading.")
               id
               suggested
               (if expansion (format "<%s>" expansion) "<TODO-IRI>")
               suggested
               localname)
              lines)))
    ;; Step 7.5.7 contingent NOTE: label equals id (or missing).
    (when label-missing-p
      (push "" lines)
      (push (concat
             "NOTE: source has no human-readable rdfs:label (stored "
             "label equals the id).  Replace `TODO-label' in the "
             "heading with a human label before inserting.")
            lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun elot-gptel-tool-db-borrow-term (token)
  "Return an ELOT description-list snippet for borrowing TOKEN.

TOKEN is an entity id known to the ELOT label DB -- typically
a CURIE like `ex:dog' or a full IRI (bare or angle-bracketed).
The result is a heading plus a description list that includes
an `rdfs:isDefinedBy' pointer to the source ontology IRI.  The
citation is part of the output shape, not docstring advice, so
mechanical reuse always carries the attribution.

When the source ontology has no `owl:Ontology' declaration in
the DB, `rdfs:isDefinedBy' falls back to a `(source: NAME)'
note.  The leading `*' is a placeholder -- callers (M9.3's
`elot_add_term', or the user) re-level it to match the target
ontology's structure.

Returns a plain-text snippet (no JSON, no escaping beyond ELOT
conventions) so it can be quoted directly back into an Org
buffer.  When TOKEN is unknown, returns
`ERROR: no entity with id ...'.  Read-only."
  (condition-case err
      (progn
        (unless (and (stringp token) (not (string-empty-p token)))
          (user-error "elot-gptel: token must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let ((cit (elot-db-entity-citation token)))
          (if (null cit)
              (format "ERROR: no entity with id %s in the ELOT DB"
                      (if (or (string-prefix-p "<" token)
                              (string-match-p ":" token))
                          token
                        (concat "`" token "'")))
            (elot-gptel--db-borrow-format cit))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_borrow_term tool (Milestone 9 Step 9.7 -- composite reuse-before-mint)
;;; ---------------------------------------------------------------------------
;;;
;;; Composes the three M6.5 atomic tools (`elot_db_search_label' +
;;; `elot_db_get_attributes' + `elot_db_borrow_term') into a single
;;; round-trip so the LLM's canonical reuse-before-mint workflow
;;; collapses from three tool calls to one.  Pipeline: search the
;;; entire DB by LABEL (default KIND=class) -> branch on candidate
;;; count.  Zero candidates -> stable fall-through signal pointing
;;; at `elot_mint_identifier'.  One candidate -> auto-borrow
;;; through `elot-gptel--db-borrow-format', with a one-line
;;; provenance preface.  Multiple candidates -> ranked candidate
;;; table plus a structured disambiguation prompt; the LLM (or
;;; human) narrows via SOURCE or LANG, or calls
;;; `elot_db_borrow_term' directly on the chosen id.  The
;;; composite never silently picks a winner.

(defconst elot-gptel--borrow-default-limit 50
  "Default LIMIT for `elot-gptel-tool-borrow-term' searches.")

(defconst elot-gptel--borrow-max-limit 500
  "Hard ceiling on the LIMIT for `elot-gptel-tool-borrow-term'.")

(defun elot-gptel--borrow-format-candidates (rows)
  "Render ROWS (from `elot-db-search-entities') as a TSV table."
  (elot-gptel--db-shim-format
   '("id" "label" "kind" "ontology_iri"
     "source" "data_source" "via")
   rows))

(defun elot-gptel--borrow-provenance-line (row)
  "Return a one-line provenance preface for a single-candidate ROW.
ROW is the seven-tuple returned by `elot-db-search-entities':
\(id label rdf-type ontology-iri source data-source via)."
  (let ((id     (nth 0 row))
        (rdf    (nth 2 row))
        (source (nth 4 row))
        (via    (nth 6 row)))
    (format "OK: auto-borrow %s (kind=%s, source=%s, via=%s)"
            id
            (or rdf "?")
            (or source "?")
            (or via "?"))))

(defun elot-gptel-tool-borrow-term (label &optional kind source lang)
  "Composite reuse-before-mint: search + borrow in one call.

LABEL is a non-empty string matched as a case-insensitive
substring against the DB's `entities.label' and `entities.id'
columns.  KIND defaults to `class' so the common authoring
case (\"borrow a class named Food\") works without the LLM
having to think about kind filtering; pass another kind
explicitly when borrowing a property / individual / datatype.
SOURCE and LANG narrow further (single-source restriction,
required label language tag).

Pipeline:
- Zero rows  -> returns
    OK: no candidates -- fall through to elot_mint_identifier (M10)
  on a single line.  Stable signal the LLM can match on.
- One row    -> auto-borrow: a one-line provenance preface
  followed by the same ELOT description-list snippet
  `elot_db_borrow_term' produces (including the
  `rdfs:isDefinedBy' back-pointer and any contingent NOTEs).
- Many rows  -> a ranked candidate TSV (same columns as
  `elot_db_search_label'), followed by a
    SELECT: multiple candidates ...
  block telling the caller how to disambiguate (re-call with
  SOURCE / LANG narrowed, or call `elot_db_borrow_term' with
  the chosen id).

The composite never silently picks among candidates;
disambiguation stays with the caller.  Read-only."
  (condition-case err
      (let* ((label* (and (stringp label) label))
             (kind*  (cond
                      ((null kind) "class")
                      ((and (stringp kind) (string-empty-p kind))
                       "class")
                      ((stringp kind) kind)
                      (t (user-error
                          "elot-gptel: kind must be a string"))))
             (source* (and (stringp source) (not (string-empty-p source))
                           source))
             (lang*   (and (stringp lang) (not (string-empty-p lang))
                           lang)))
        (unless (and (stringp label*) (not (string-empty-p label*)))
          (user-error "elot-gptel: label must be a non-empty string"))
        (elot-gptel--db-ensure-open)
        (let* ((rows (elot-db-search-entities
                      label* elot-gptel--borrow-default-limit
                      kind* source* lang* nil))
               (n (length rows)))
          (cond
           ;; Zero candidates: stable fall-through signal.
           ((= n 0)
            (concat
             "OK: no candidates -- fall through to "
             "elot_mint_identifier (M10)"))
           ;; Exactly one candidate: auto-borrow.
           ((= n 1)
            (let* ((row (car rows))
                   (id  (nth 0 row))
                   (cit (elot-db-entity-citation id)))
              (if (null cit)
                  (format
                   "ERROR: candidate %s resolved by search but \
elot-db-entity-citation returned nil (data inconsistency)"
                   id)
                (concat (elot-gptel--borrow-provenance-line row)
                        "\n\n"
                        (elot-gptel--db-borrow-format cit)))))
           ;; Multiple candidates: hand the choice back to the caller.
           (t
            (concat
             (elot-gptel--borrow-format-candidates rows)
             "\n\n"
             (format "SELECT: %d candidates match `%s'.  Re-call with \
`source=NAME' (and optionally `lang=LANG') narrowed to one row; the \
composite will then auto-borrow.  Or call `elot_db_borrow_term \
token=ID' directly with the chosen id."
                     n label*))))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_conventions tool (Milestone 7.5 Step 7.5.1)
;;; ---------------------------------------------------------------------------
;;;
;;; A zero-argument tool returning ELOT's authoring conventions as
;;; LLM-friendly Markdown.  The prose lives in the sibling file
;;; `elot-conventions.md' so developers can review and update it
;;; without touching Elisp; the Elisp side is purely a path
;;; resolver + a tool wrapper, mirroring how `elot-db.el' handles
;;; its sibling `schema.sql'.
;;;
;;; The LLM is expected to call this once at the start of any
;;; authoring session; the document covers heading-nesting,
;;; description-list semantics, reuse via `rdfs:isDefinedBy', and
;;; the (frequently misunderstood) `:nodeclare:' tag.

(defconst elot-gptel--conventions-fallback
  "# ELOT authoring conventions

The on-disk `elot-conventions.md' file could not be located.

Quick reference for the LLM:

- Heading nesting under a `:resourcedefs: yes' section carries
  `SubClassOf' for classes and `SubPropertyOf' for properties.
  This is the cardinal ELOT idiom; do NOT use a description-list
  `SubClassOf ::' axiom to a named class.
- Heading shape: `Label (curie)'.  Prose before the parens is
  `rdfs:label'; CURIE inside is the identifier.
- Description-list `- key :: value' rows carry annotations
  (`rdfs:label', `rdfs:comment', `skos:definition',
  `rdfs:isDefinedBy', ...) and OMN axioms (`Domain ::',
  `Range ::', `Characteristics ::', `DisjointWith ::',
  `Types ::', `Facts ::', ...).
- Reuse an external term by declaring it as a normal heading and
  adding `- rdfs:isDefinedBy :: <source-ontology-iri>'.  Do NOT
  tag reused terms with `:nodeclare:'; that tag suppresses
  declaration entirely and is intended for narrative headings
  interleaved between resource declarations.
- Top-level structure: `Prefixes' (`:prefixdefs: yes'),
  `<name> ontology' (`:resourcedefs: yes'), `Datatypes',
  `Classes', `Object properties', `Data properties',
  `Annotation properties', `Individuals' (all
  `:resourcedefs: yes').

For the full document, see `elot-conventions.md' in the package
source tree."
  "Embedded fallback for `elot_conventions' when the on-disk
sibling `elot-conventions.md' cannot be located.  Kept concise on
purpose -- the authoritative source is the Markdown file, edited
directly by developers; this fallback exists only as a defensive
last resort.")

(defun elot-gptel--conventions-path ()
  "Return the filesystem path to `elot-conventions.md', or nil.
Located alongside this library file; returns nil when the library
itself cannot be located (e.g. running from a raw source tree
before byte-compilation).  Mirrors `elot-db--schema-sql-path'."
  (let ((lib (or (locate-library "elot-gptel")
                 load-file-name
                 (and (boundp 'byte-compile-current-file)
                      byte-compile-current-file))))
    (when lib
      (let ((candidate (expand-file-name
                        "elot-conventions.md"
                        (file-name-directory lib))))
        (and (file-readable-p candidate) candidate)))))

(defun elot-gptel--read-conventions ()
  "Return the contents of `elot-conventions.md' as a string.
Falls back to `elot-gptel--conventions-fallback' when the file is
missing or unreadable."
  (let ((path (elot-gptel--conventions-path)))
    (if path
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))
      elot-gptel--conventions-fallback)))

(defun elot-gptel-tool-conventions ()
  "Return the ELOT authoring-conventions cheat sheet as Markdown.
Read-only.  Never raises; on any internal error returns an
`ERROR:'-prefixed line."
  (condition-case err
      (elot-gptel--read-conventions)
    (error (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_check tool (Milestone 7.5 Step 7.5.6)
;;; ---------------------------------------------------------------------------
;;
;; Composite, fail-fast pipeline: LINT -> OMN PARSE -> CONSISTENCY ->
;; UNSATISFIABLE.  Each stage delegates to an existing atomic tool;
;; this wrapper adds only stage framing and a SUMMARY verdict.  The
;; goal is to collapse the canonical author-edit-verify loop into a
;; single tool call so the LLM does not have to remember the order
;; or pay a round-trip per stage.

(defun elot-gptel--check-stage-header (label)
  "Return a `== LABEL ==' stage header line."
  (format "== %s ==" label))

(defun elot-gptel--check-lint-failed-p (report)
  "Return non-nil when a lint REPORT contains at least one error.
REPORT is the string returned by `elot-gptel-tool-lint'.  The
report's final `Summary:' line names the count; missing summary
means `OK: no lint issues' (a clean run)."
  (when (and report (stringp report))
    (let ((case-fold-search nil))
      (and (string-match
            "^Summary: \\([0-9]+\\) errors?," report)
           (> (string-to-number (match-string 1 report)) 0)))))

(defun elot-gptel--check-omn-failed-p (report)
  "Return non-nil when an OMN-validate REPORT did not parse cleanly."
  (and report (stringp report)
       (not (string-prefix-p "OK:" (string-trim-left report)))))

(defun elot-gptel--check-consistency-classify (report)
  "Classify a consistency REPORT into one of :ok, :unsat, :inconsistent, :error.
:ok           -- ontology consistent, no unsat classes
:unsat        -- ontology consistent but has unsatisfiable classes
:inconsistent -- ontology has no model
:error        -- ROBOT raised or the kind is unknown"
  (cond
   ((null report) :error)
   ((string-match-p "^OK: ontology is consistent but has unsatisfiable"
                    report)
    :unsat)
   ((string-match-p "^OK: ontology is consistent" report) :ok)
   ((string-match-p "^INCONSISTENT" report) :inconsistent)
   (t :error)))

(defun elot-gptel-tool-check (file &optional content profile reasoner)
  "Composite lint -> OMN parse -> consistency -> unsatisfiable check.

FILE is an ELOT .org file (project-relative or absolute).
CONTENT, when supplied, is an in-flight draft string forwarded
to the lint and OMN-validate stages -- same semantics as the
`content' arg on `elot_lint'.
PROFILE selects an OWL 2 profile for the OMN-parse stage
\(\"DL\", \"EL\", \"QL\", \"RL\", \"Full\"; default skip).
REASONER is \"hermit\" (default) or \"whelk\".

Pipeline is fail-fast: a lint *error* skips the remaining
stages; a failed OMN parse skips the reasoning stages; an
inconsistent ontology skips the unsat-class enumeration.  Lint
*warnings* never block.  When ROBOT is not configured, the
ROBOT-backed stages are reported as SKIPPED and the pipeline
still ends with a clean verdict if lint was clean -- partial
coverage beats a hard failure for users without ROBOT.

Returns a multi-block plain-text report with a final SUMMARY
line.  Never raises; on internal error a single ERROR: line is
returned in the affected stage."
  (let* ((sections nil)
         (push-section
          (lambda (label body)
            (push (concat (elot-gptel--check-stage-header label)
                          "\n" body)
                  sections)))
         (verdict nil)
         (verdict-hint nil)
         (robot-ok
          (condition-case _err
              (progn (require 'elot-robot)
                     (and (fboundp 'elot-robot-available-p)
                          (elot-robot-available-p)))
            (error nil))))
    ;; --- Stage 1: LINT ---
    (let* ((label (format "LINT (%s)" file))
           (lint-report (elot-gptel-tool-lint file nil nil content)))
      (funcall push-section label lint-report)
      (cond
       ((string-prefix-p "ERROR:" lint-report)
        (setq verdict (format "FAIL at stage: lint (%s)"
                              (substring lint-report 7))
              verdict-hint "fix the lint error before re-running."))
       ((elot-gptel--check-lint-failed-p lint-report)
        (let ((n (and (string-match "^Summary: \\([0-9]+\\) error"
                                    lint-report)
                      (string-to-number (match-string 1 lint-report)))))
          (setq verdict (format "FAIL at stage: lint (%d error%s)"
                                (or n 1) (if (= (or n 1) 1) "" "s"))
                verdict-hint "fix lint errors before re-running; downstream stages were skipped.")))))
    ;; --- Stage 2: OMN PARSE ---
    (let ((label (if profile
                     (format "OMN PARSE (profile=%s)" (upcase profile))
                   "OMN PARSE")))
      (cond
       (verdict
        (funcall push-section label "SKIPPED: prior stage failed"))
       ((not robot-ok)
        (funcall push-section label
                 "SKIPPED: ROBOT not configured"))
       (t
        (let ((report (elot-gptel-tool-omn-validate file profile content)))
          (funcall push-section label report)
          (when (elot-gptel--check-omn-failed-p report)
            (setq verdict (format "FAIL at stage: OMN parse%s"
                                  (if profile
                                      (format " (profile=%s)" (upcase profile))
                                    ""))
                  verdict-hint "ROBOT could not parse the tangled OMN; fix the diagnostic above."))))))
    ;; --- Stage 3: CONSISTENCY ---
    (let* ((r (downcase (or reasoner "hermit")))
           (label (format "CONSISTENCY (reasoner=%s)" r))
           (consistency-kind nil))
      (cond
       (verdict
        (funcall push-section label "SKIPPED: prior stage failed"))
       ((not robot-ok)
        (funcall push-section label
                 "SKIPPED: ROBOT not configured"))
       (t
        (let ((report (elot-gptel-tool-consistency file reasoner)))
          (funcall push-section label report)
          (setq consistency-kind
                (elot-gptel--check-consistency-classify report))
          (when (eq consistency-kind :inconsistent)
            (setq verdict "FAIL at stage: consistency (ontology is inconsistent)"
                  verdict-hint "the ontology has no model; explain `owl:Thing SubClassOf: owl:Nothing' to find the clash."))
          (when (eq consistency-kind :error)
            (setq verdict "FAIL at stage: consistency (ROBOT error)"
                  verdict-hint "inspect the ROBOT diagnostic above.")))))
      ;; --- Stage 4: UNSATISFIABLE ---
      (let ((unsat-label "UNSATISFIABLE CLASSES"))
        (cond
         (verdict
          (funcall push-section unsat-label "SKIPPED: prior stage failed"))
         ((not robot-ok)
          (funcall push-section unsat-label
                   "SKIPPED: ROBOT not configured"))
         ((eq consistency-kind :unsat)
          (let ((report (elot-gptel-tool-unsatisfiable file reasoner)))
            (funcall push-section unsat-label report)))
         (t
          (funcall push-section unsat-label
                   "SKIPPED: consistency reported no unsatisfiable classes")))))
    ;; --- Final SUMMARY ---
    (let ((summary
           (cond
            (verdict
             (concat verdict
                     (and verdict-hint (concat "\nHINT: " verdict-hint))))
            ((not robot-ok)
             "OK: lint clean (ROBOT-backed stages SKIPPED)")
            (t
             (let* ((parts '("lint" "OMN parse"))
                    (parts (if profile
                               (append parts (list (upcase profile)))
                             parts))
                    (parts (append parts '("consistency"))))
               (format "OK: all checks pass (%s)"
                       (mapconcat #'identity parts " + ")))))))
      (funcall push-section "SUMMARY" summary))
    (mapconcat #'identity (nreverse sections) "\n\n")))

;;; ---------------------------------------------------------------------------
;;; Identifier minting (Milestone 10)
;;; ---------------------------------------------------------------------------
;;;
;;; Thin LLM-facing wrappers around `elot-id-mint' and `elot-id-verify'
;;; (see `elot-id.el').  Identifier minting is read-only with respect
;;; to the user's project: the tools only choose a string, they do not
;;; write the .org file.  Therefore not gated by
;;; `elot-gptel-allow-side-effects'.

(defconst elot-gptel--mint-kinds
  '("class" "object-property" "data-property"
    "annotation-property" "individual" "datatype")
  "Permitted `kind' values for the identifier tools.")

(defun elot-gptel--mint-resolve-scheme (scheme-arg)
  "Coerce SCHEME-ARG to (SCHEME . PARAMS-PLIST).
SCHEME-ARG may be nil, a symbol, or a string of the form
\"NAME\" or \"NAME TOKEN ...\" (see `elot-id-parse-spec' for the
parameter token syntax).  When nil, fall back to the configured
default."
  (require 'elot-id)
  (cond
   ((null scheme-arg)
    (or (and (boundp 'elot-id-default-scheme)
             (ignore-errors
               (cons (elot-id-scheme-by-name elot-id-default-scheme) nil)))
        (user-error "elot-gptel: no scheme configured")))
   ((or (stringp scheme-arg) (symbolp scheme-arg))
    (let* ((parsed (elot-id-parse-spec scheme-arg))
           (name (car parsed)))
      (unless name
        (user-error "elot-gptel: empty scheme spec"))
      (cons (elot-id-scheme-by-name name) (cdr parsed))))
   (t (user-error "elot-gptel: bad scheme argument: %S" scheme-arg))))

(defun elot-gptel--mint-buffer-for-file (true-file)
  "Return a live buffer visiting TRUE-FILE, or nil.
Prefers an already-open buffer; otherwise opens the file via
`find-file-noselect'.  Returns nil when TRUE-FILE is nil or not
readable."
  (when (and true-file (file-readable-p true-file))
    (or (find-buffer-visiting true-file)
        (let ((inhibit-message t))
          (find-file-noselect true-file 'nowarn)))))

(defun elot-gptel--mint-hierarchy-for-file (true-file)
  "Return the parsed `elot-headline-hierarchy' for TRUE-FILE.
Reads the buffer-local variable; populates it via
`elot-update-headline-hierarchy' on first use.  Returns nil when
the file is not available or has no parseable headline tree."
  (let ((buf (elot-gptel--mint-buffer-for-file true-file)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (require 'elot-tangle nil 'noerror)
        (when (and (null (bound-and-true-p elot-headline-hierarchy))
                   (fboundp 'elot-update-headline-hierarchy))
          (elot-update-headline-hierarchy))
        (bound-and-true-p elot-headline-hierarchy)))))

(defun elot-gptel--mint-ontology-node (hierarchy)
  "Return the first ontology node in HIERARCHY, or nil.
An ontology node is a top-level (level 1) child of the ROOT whose
`:elot-context-type' is the string \"ontology\"."
  (when hierarchy
    (cl-find-if (lambda (n)
                  (equal (plist-get n :elot-context-type) "ontology"))
                (plist-get hierarchy :children))))

(defun elot-gptel--mint-collect-uris (node)
  "Walk NODE depth-first; return all non-nil `:uri' strings.
The first whitespace-separated token of a composite `:uri'
\(e.g. \"ex:Ont <http://...>\") is kept."
  (let (acc)
    (cl-labels ((walk (n)
                  (let ((uri (plist-get n :uri)))
                    (when (and uri (stringp uri))
                      (let ((tok (car (split-string uri "[ \t]+" t))))
                        (when (and tok (not (string-empty-p tok)))
                          (push tok acc)))))
                  (mapc #'walk (plist-get n :children))))
      (walk node))
    (delete-dups (nreverse acc))))

(defun elot-gptel--mint-scheme-spec-from-node (node)
  "Return the scheme spec string declared on NODE, or nil.
Reassembles `:elot-id-scheme' and (optional) `:elot-id-scheme-format'
into a single spec string consumable by `elot-id-parse-spec'."
  (when node
    (let ((s (plist-get node :elot-id-scheme))
          (f (plist-get node :elot-id-scheme-format)))
      (cond
       ((and s f) (concat s " " f))
       (s         s)
       (t         nil)))))

(defun elot-gptel--mint-build-context (file kind &optional scheme-params node)
  "Build a CONTEXT plist for the identifier tools.
SCHEME-PARAMS, when non-nil, is forwarded under `:scheme-params'.
NODE, when supplied, is the ontology headline node from
`elot-headline-hierarchy'; its `:elot-default-prefix' seeds the
context prefix and its descendants supply `:existing-iris'.  When
NODE is nil the context falls back to FILE-only -- no prefix, no
existing IRIs."
  (let* ((true (and file (elot-gptel--resolve-file-path file)))
         (prefix (and node (plist-get node :elot-default-prefix)))
         (existing (and node (elot-gptel--mint-collect-uris node)))
         (kind-sym (cond ((null kind) nil)
                         ((symbolp kind) kind)
                         ((stringp kind)
                          (unless (member kind elot-gptel--mint-kinds)
                            (user-error "elot-gptel: unknown kind: %s"
                                        kind))
                          (intern kind)))))
    (append
     (and true          (list :file true))
     (and prefix        (list :prefix prefix))
     (and kind-sym      (list :kind kind-sym))
     (and existing      (list :existing-iris existing))
     (and scheme-params (list :scheme-params scheme-params)))))

(defun elot-gptel--mint-resolve-scheme-with-node (scheme-arg node)
  "Resolve SCHEME-ARG to (SCHEME . PARAMS); fall back to NODE's spec.
When SCHEME-ARG is non-nil it wins.  Otherwise consult NODE's
`:elot-id-scheme' / `:elot-id-scheme-format' properties.  When
neither is available, signal a `user-error' asking for the scheme
to be declared on the ontology heading -- gptel tools cannot
prompt interactively, so the caller surfaces this as an
`ERROR:' line for the LLM to relay to the user."
  (cond
   (scheme-arg
    (elot-gptel--mint-resolve-scheme scheme-arg))
   (t
    (let ((spec (elot-gptel--mint-scheme-spec-from-node node)))
      (if spec
          (elot-gptel--mint-resolve-scheme spec)
        (user-error
         (concat "no `:ELOT-id-scheme:' declared on the ontology "
                 "heading; ask the user which scheme to use "
                 "(uuid, slug, counter, acme) and add the "
                 "property under `:ELOT-context-type: ontology'")))))))

(defun elot-gptel-tool-mint-identifier (file label &optional scheme kind)
  "Mint a fresh CURIE for LABEL using SCHEME (default: configured).

FILE is an ELOT .org file whose `:ELOT-default-prefix:' property
and existing declarations seed the identifier-minting context.
The file need not exist on disk yet (the LLM is often authoring a
new ontology).  KIND is optional but recommended -- some schemes
(notably `acme') encode it in the produced identifier.

The active identifier scheme is /project-defined/: different
projects use radically different conventions (timestamp-encoded,
prefix+counter, UUID, slug, ...).  When the user has not made the
convention explicit, ask before minting -- do not guess.

Returns a single line:
  OK: minted CURIE (scheme=NAME, label=...)
or an `ERROR:' line.  Read-only with respect to the .org file."
  (condition-case err
      (progn
        (require 'elot-id)
        (unless (and (stringp label) (not (string-empty-p label)))
          (user-error "elot-gptel: label must be a non-empty string"))
        (let* ((true (and file (elot-gptel--resolve-file-path file)))
               (hier (elot-gptel--mint-hierarchy-for-file true))
               (node (elot-gptel--mint-ontology-node hier))
               (resolved (elot-gptel--mint-resolve-scheme-with-node
                          scheme node))
               (scheme-obj (car resolved))
               (scheme-params (cdr resolved))
               (context (elot-gptel--mint-build-context
                         file kind scheme-params node))
               (curie (elot-id-mint scheme-obj label context))
               (slug-warn
                (and (stringp curie)
                     (get-text-property 0 'elot-id-acme-slug-warning curie))))
          (concat
           (format "OK: minted %s (scheme=%s, label=%S)"
                   curie
                   (elot-id-scheme-name scheme-obj)
                   label)
           (and slug-warn (format "\nNOTE: %s" slug-warn)))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defun elot-gptel-tool-verify-identifier (file curie &optional scheme)
  "Verify CURIE under the scheme configured for FILE's ontology.

FILE is required so the verifier can locate the containing
ontology heading and read its `:ELOT-id-scheme:' property -- the
same node-driven resolution `elot_mint_identifier' uses.  An
explicit SCHEME argument overrides the configured one.

Returns:
  OK: \='CURIE\=' is valid under scheme \='NAME\='.
or
  FAIL: \='CURIE\=' does not conform to scheme \='NAME\='.
or an `ERROR:' line.  Read-only."
  (condition-case err
      (progn
        (require 'elot-id)
        (unless (and (stringp curie) (not (string-empty-p curie)))
          (user-error "elot-gptel: curie must be a non-empty string"))
        (let* ((true (elot-gptel--resolve-file-path file))
               (hier (elot-gptel--mint-hierarchy-for-file true))
               (node (elot-gptel--mint-ontology-node hier))
               (resolved (elot-gptel--mint-resolve-scheme-with-node
                          scheme node))
               (scheme-obj (car resolved))
               (scheme-params (cdr resolved))
               (context (elot-gptel--mint-build-context
                         file nil scheme-params node))
               (ok (elot-id-verify scheme-obj curie context)))
          (format "%s: '%s' %s scheme '%s'."
                  (if ok "OK" "FAIL")
                  curie
                  (if ok "is valid under" "does not conform to")
                  (elot-id-scheme-name scheme-obj))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_axiom_keywords tool (Milestone 9 Step 9.2.a)
;;; ---------------------------------------------------------------------------
;;;
;;; Read-only authoring helper.  Given a subject CURIE / label,
;;; returns the legal OMN frame keywords for its kind (per static
;;; table), the project's declared annotation properties (universal
;;; rows), the buffer signature from `elot-slurp', and the
;;; subject's existing description-list rows.  Pure: only consults
;;; the buffer; no ROBOT, no DB.

(defconst elot-gptel--axiom-frame-keywords
  '(("owl:Class"
     ("SubClassOf"       . "<ClassExpression>")
     ("EquivalentTo"     . "<ClassExpression>")
     ("DisjointWith"     . "<ClassExpression>[, ...]")
     ("DisjointUnionOf"  . "<ClassExpression>, <ClassExpression>[, ...]")
     ("HasKey"           . "<ObjectProperty>[, ...] <DataProperty>[, ...]"))
    ("owl:ObjectProperty"
     ("Domain"           . "<ClassExpression>")
     ("Range"            . "<ClassExpression>")
     ("SubPropertyOf"    . "<ObjectProperty>")
     ("EquivalentTo"     . "<ObjectProperty>")
     ("DisjointWith"     . "<ObjectProperty>")
     ("InverseOf"        . "<ObjectProperty>")
     ("Characteristics" .
      "<Name>[, ...] (Functional | InverseFunctional | Transitive | \
Symmetric | Asymmetric | Reflexive | Irreflexive)")
     ("SubPropertyChain" . "<ObjectProperty> o <ObjectProperty>[ o ...]"))
    ("owl:DatatypeProperty"
     ("Domain"           . "<ClassExpression>")
     ("Range"            . "<DataRange>")
     ("SubPropertyOf"    . "<DataProperty>")
     ("EquivalentTo"     . "<DataProperty>")
     ("DisjointWith"     . "<DataProperty>")
     ("Characteristics"  . "Functional"))
    ("owl:AnnotationProperty"
     ("Domain"           . "<IRI>")
     ("Range"            . "<IRI>")
     ("SubPropertyOf"    . "<AnnotationProperty>"))
    ("owl:NamedIndividual"
     ("Types"            . "<ClassExpression>")
     ("Facts" .
      "<ObjectProperty> <Individual> | <DataProperty> <Literal> | \
not <ObjectProperty> <Individual>")
     ("SameAs"           . "<Individual>")
     ("DifferentFrom"    . "<Individual>"))
    ("rdfs:Datatype"
     ("EquivalentTo"     . "<DataRange>"))
    ("owl:Ontology"
     ("Import"           . "<IRI>")))
  "Static OMN frame-keyword table indexed by `rdf:type'.
Each entry has the shape (KIND (KEYWORD . SHAPE-HINT) ...).
Drawn from the Manchester Syntax spec; intentionally a
`defconst' so the LLM-facing surface is auditable.")

(defconst elot-gptel--axiom-kind-pretty
  '(("owl:Class"               . "Class")
    ("owl:ObjectProperty"      . "ObjectProperty")
    ("owl:DatatypeProperty"    . "DataProperty")
    ("owl:AnnotationProperty"  . "AnnotationProperty")
    ("owl:NamedIndividual"     . "Individual")
    ("rdfs:Datatype"           . "Datatype")
    ("owl:Ontology"            . "Ontology"))
  "Display names for ELOT `rdf:type' values.")

(defconst elot-gptel--axiom-existing-row-skip
  '("rdfs:label" "rdf:type" "puri")
  "Plist keys that are intrinsic / housekeeping, not user-authored rows.
Filtered out of the `Existing rows' block returned by
`elot_axiom_keywords'.")

(defconst elot-gptel--axiom-signature-kinds-for
  '(("owl:Class"
     "owl:Class" "owl:ObjectProperty" "owl:DatatypeProperty"
     "rdfs:Datatype" "owl:NamedIndividual")
    ("owl:ObjectProperty"
     "owl:Class" "owl:ObjectProperty")
    ("owl:DatatypeProperty"
     "owl:Class" "owl:DatatypeProperty" "rdfs:Datatype")
    ("owl:AnnotationProperty"
     "owl:Class" "owl:ObjectProperty" "owl:DatatypeProperty"
     "owl:AnnotationProperty" "owl:NamedIndividual" "rdfs:Datatype")
    ("owl:NamedIndividual"
     "owl:Class" "owl:ObjectProperty" "owl:DatatypeProperty"
     "owl:NamedIndividual" "rdfs:Datatype")
    ("rdfs:Datatype"
     "rdfs:Datatype")
    ("owl:Ontology"))
  "Per-kind list of signature buckets actually reachable from the
subject's frame keywords.  Mitigation (1) from the M9.2 size
discussion: a property-axiom author has no use for Individuals or
APs in its frame fragments, so don't enumerate them.  Keyed by
`rdf:type'; the fallback (unknown kind) is the union of all six
kinds.")

(defconst elot-gptel--axiom-signature-all-kinds
  '("owl:Class" "owl:ObjectProperty" "owl:DatatypeProperty"
    "owl:AnnotationProperty" "owl:NamedIndividual" "rdfs:Datatype")
  "Full per-kind ordering used when the subject's kind is unknown.")

(defun elot-gptel--axiom-slurp-for-file (file)
  "Open FILE (without selecting), refresh slurp, return `elot-slurp'.
Ensures the buffer is in `org-mode' and its
`elot-headline-hierarchy' has been parsed first -- in batch
contexts the freshly-opened buffer would otherwise be in
`fundamental-mode' with an empty hierarchy, leaving
`elot-build-slurp' with nothing to walk."
  (with-current-buffer (find-file-noselect file)
    (require 'elot-tangle nil 'noerror)
    (unless (derived-mode-p 'org-mode)
      (delay-mode-hooks (org-mode)))
    (when (fboundp 'elot-update-headline-hierarchy)
      (condition-case _ (elot-update-headline-hierarchy) (error nil)))
    (when (fboundp 'elot-slurp-to-vars)
      (condition-case _ (elot-slurp-to-vars) (error nil)))
    (and (boundp 'elot-slurp) elot-slurp)))

(defun elot-gptel--axiom-resolve-subject (subject slurp)
  "Resolve SUBJECT against SLURP; return its (CURIE LABEL ATTRS) row.
Tries CURIE match first (car of each row), then label match
(cadr).  Returns nil when no entry is found."
  (or (assoc subject slurp)
      (cl-find-if (lambda (row) (equal (cadr row) subject)) slurp)))

(defun elot-gptel--axiom-row-kind (row)
  "Return the `rdf:type' attribute stored in ROW, or nil.
ROW's attribute plist uses STRING keys (the OMN/RDF predicate
names), so `plist-get' must compare with `equal' rather than its
default `eq' -- two equal strings are not `eq'."
  (plist-get (nth 2 row) "rdf:type" #'equal))

(defun elot-gptel--axiom-format-keywords (kind)
  "Render the frame-keyword block for KIND."
  (let ((entries (cdr (assoc kind elot-gptel--axiom-frame-keywords))))
    (if (null entries)
        (format "  (no frame keywords defined for kind %s)"
                (or kind "(unknown)"))
      (mapconcat
       (lambda (kv) (format "  - %s :: %s" (car kv) (cdr kv)))
       entries "\n"))))

(defun elot-gptel--axiom-collect-by-kind (slurp kind)
  "Return entries in SLURP whose `rdf:type' is KIND, as (CURIE . LABEL)."
  (let (acc)
    (dolist (row slurp (nreverse acc))
      (when (equal (elot-gptel--axiom-row-kind row) kind)
        (push (cons (car row) (cadr row)) acc)))))

(defun elot-gptel--axiom-format-sig-block (label entries)
  "Render a signature block titled LABEL listing ENTRIES (CURIE . LABEL)."
  (if (null entries)
      (format "  %s: (none)" label)
    (concat (format "  %s (%d):\n" label (length entries))
            (mapconcat
             (lambda (cl)
               (format "    - %s -- %S" (car cl) (or (cdr cl) "")))
             entries "\n"))))

(defun elot-gptel--axiom-format-signature (slurp &optional kind)
  "Render the per-kind buffer signature derived from SLURP.
When KIND is non-nil, restrict the enumerated buckets to those
frame keywords for KIND can actually reference (mitigation (1)
from the M9.2 size discussion).  An unknown / nil KIND falls
back to the full six-bucket render."
  (let ((kinds (or (cdr (assoc kind elot-gptel--axiom-signature-kinds-for))
                   elot-gptel--axiom-signature-all-kinds)))
    (mapconcat
     (lambda (k)
       (elot-gptel--axiom-format-sig-block
        (cdr (assoc k elot-gptel--axiom-kind-pretty))
        (elot-gptel--axiom-collect-by-kind slurp k)))
     kinds
     "\n")))

(defun elot-gptel--axiom-format-existing-rows (row)
  "Render the description-list rows currently on ROW.
Skips intrinsic / housekeeping entries (`rdfs:label',
`rdf:type', `puri')."
  (let ((plist (nth 2 row))
        (out '()))
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (member k elot-gptel--axiom-existing-row-skip)
          (push (format "  - %s :: %s" k v) out))))
    (if out
        (mapconcat #'identity (nreverse out) "\n")
      "  (no description-list rows)")))

(defun elot-gptel-tool-axiom-keywords (file subject)
  "Return the legal OMN frame keywords for SUBJECT in FILE.

Read-only authoring helper for Step 9.2.a.  Resolves SUBJECT
against the buffer's `elot-slurp' (CURIE preferred, label
fallback), reads its `rdf:type', and returns a structured
report with four sections:

  == Frame keywords ==      legal frame keys for the kind, with
                            a one-line shape hint each.
  == Universal annotation rows ==
                            every `owl:AnnotationProperty' declared
                            in the file -- the APs the LLM may use
                            on any subject (rdfs:comment,
                            skos:example, iof-av:naturalLanguage-
                            Definition, ...).
  == Buffer signature ==    per-kind CURIE -> label index drawn
                            from `elot-slurp', scoped to the kinds
                            that SUBJECT's frame keywords can
                            actually reference (e.g. an ObjectProperty
                            subject sees Classes and ObjectProperties
                            only -- no Individuals, no APs).  For
                            very large ontologies, prefer
                            `elot_db_search_label' to enumerate the
                            signature on demand.
  == Existing rows ==       current description-list contents of
                            SUBJECT, so add / replace / remove can
                            target them unambiguously.

Returns an `OK:'-prefixed multi-line report on success, an
`ERROR:'-prefixed single line otherwise.  Never mutates FILE."
  (condition-case err
      (progn
        (unless (and (stringp subject) (not (string-empty-p subject)))
          (user-error "elot-gptel: subject must be a non-empty string"))
        (let* ((true (elot-gptel--resolve-file file))
               (slurp (elot-gptel--axiom-slurp-for-file true)))
          (unless slurp
            (user-error
             "elot-gptel: no resources declared in %s (elot-slurp empty)"
             file))
          (let ((row (elot-gptel--axiom-resolve-subject subject slurp)))
            (unless row
              (user-error
               "elot-gptel: subject %s not found in elot-slurp \
(try a declared CURIE or label)"
               subject))
            (let* ((curie (car row))
                   (label (cadr row))
                   (kind  (elot-gptel--axiom-row-kind row))
                   (pretty (cdr (assoc kind elot-gptel--axiom-kind-pretty)))
                   (aps (elot-gptel--axiom-collect-by-kind
                         slurp "owl:AnnotationProperty")))
              (mapconcat
               #'identity
               (list
                (format "OK: subject %s (label %S), kind %s"
                        curie label (or pretty kind "(unknown)"))
                ""
                "== Frame keywords =="
                (elot-gptel--axiom-format-keywords kind)
                ""
                "== Universal annotation rows =="
                (if aps
                    (mapconcat
                     (lambda (cl)
                       (format "  - %s -- %S" (car cl) (or (cdr cl) "")))
                     aps "\n")
                  "  (no annotation properties declared in this file)")
                ""
                "== Buffer signature =="
                (elot-gptel--axiom-format-signature slurp kind)
                ""
                (format "== Existing rows on %s ==" curie)
                (elot-gptel--axiom-format-existing-rows row))
               "\n")))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_axiom_check tool (Milestone 9 Step 9.2.b)
;;; ---------------------------------------------------------------------------
;;;
;;; Read-only pre-flight validator for a candidate
;;; `- KEYWORD :: FRAGMENT' row.  Synthesises the row inside the
;;; subject's description list in an in-memory copy of FILE, then
;;; runs the existing lint checkers (`elot/omn-keyword-appropri-
;;; ateness', `elot/axiom-value-curies', `elot/axiom-keyword-range')
;;; and ROBOT's OMN parser on the draft.  Optional consistency
;;; probe via `elot_consistency'.

(defconst elot-gptel--axiom-check-checkers
  '(elot/omn-syntax
    elot/omn-keyword-appropriateness
    elot/axiom-value-curies
    elot/axiom-keyword-range)
  "Static checkers exercised by `elot_axiom_check' on the synthesised
draft, in addition to the OMN parse.")

(defvar elot-omn-all-keywords) ; from elot-tangle.el (used opportunistically)
(declare-function elot-omn-keywords-for-kind "elot-lint" (kind))
(declare-function elot-id-heading-curie-regexp "elot-id" (curie))

(defun elot-gptel--axiom-check-declared-aps (slurp)
  "Return the list of declared annotation-property CURIEs from SLURP."
  (mapcar #'car
          (elot-gptel--axiom-collect-by-kind slurp "owl:AnnotationProperty")))

(defun elot-gptel--axiom-check-keyword-legal-p (keyword kind slurp)
  "Return non-nil when KEYWORD is legal as a description-list key for KIND.
A keyword is legal when it is one of KIND's frame keywords
(via `elot-omn-keywords-for-kind') or a declared annotation
property in SLURP."
  (require 'elot-lint)
  (or (member keyword (elot-omn-keywords-for-kind kind))
      (member keyword (elot-gptel--axiom-check-declared-aps slurp))))

(defun elot-gptel--axiom-check-find-subject-heading (curie label)
  "Return buffer position of SUBJECT's resource heading line.
Tries the CURIE-parenthetical heading form first (preferred),
then a bare-label fallback for headings without a CURIE.
Signals `user-error' when neither matches."
  (require 'elot-id)
  (or (save-excursion
        (goto-char (point-min))
        (and (re-search-forward
              (elot-id-heading-curie-regexp curie) nil t)
             (line-beginning-position)))
      (save-excursion
        (goto-char (point-min))
        (and label
             (re-search-forward
              (concat "^\\*+ " (regexp-quote label) "[ \t]*$") nil t)
             (line-beginning-position)))
      (user-error
       "elot-gptel: cannot locate heading for %s in buffer" curie)))

(defun elot-gptel--axiom-synthesise-draft
    (file-contents subject-curie subject-label keyword fragment)
  "Append `- KEYWORD :: FRAGMENT' under SUBJECT's heading in FILE-CONTENTS.
Returns a plist (:draft STRING :line N) where :line is the
1-based line number of the synthesised row in the returned
draft string."
  (with-temp-buffer
    (insert file-contents)
    (let ((org-inhibit-startup t) (org-mode-hook nil))
      (org-mode))
    (let* ((heading-pos
            (elot-gptel--axiom-check-find-subject-heading
             subject-curie subject-label))
           insertion-pos)
      (save-excursion
        (goto-char heading-pos)
        (let* ((heading-end (line-end-position))
               (subtree-end (save-excursion
                              (org-end-of-subtree t t)
                              (point)))
               (next-heading
                (save-excursion
                  (goto-char heading-end)
                  (if (re-search-forward "^\\*+ " subtree-end t)
                      (line-beginning-position)
                    subtree-end))))
          (setq insertion-pos next-heading)))
      (goto-char insertion-pos)
      (unless (bolp)
        (insert "\n"))
      (let ((line-before (line-number-at-pos)))
        (insert (format " - %s :: %s\n" keyword fragment))
        (list :draft (buffer-substring-no-properties
                      (point-min) (point-max))
              :line line-before)))))

(defun elot-gptel--axiom-check-parse-lint-line (line)
  "Parse one line of an `elot_lint' report.
Return (LINE-NUM . MESSAGE) or nil when LINE has no
`LINE:COL  [CATEGORY/TRUST]  MESSAGE' shape (e.g. the trailing
`Summary:' line, blank lines, `OK:' header)."
  (when (string-match
         "\\`\\([0-9]+\\):[0-9]+[ \t]+\\[[^]]+\\][ \t]+\\(.*\\)\\'"
         line)
    (cons (string-to-number (match-string 1 line))
          (match-string 2 line))))

(defun elot-gptel--axiom-check-filter-issues (lint-report inserted-line)
  "Return issues from LINT-REPORT whose line matches INSERTED-LINE.
Each element is a string -- the diagnostic message verbatim."
  (when (and lint-report (stringp lint-report)
             (not (string-prefix-p "OK:" lint-report))
             (not (string-prefix-p "ERROR:" lint-report)))
    (let (matches)
      (dolist (raw (split-string lint-report "\n" t))
        (let ((parsed (elot-gptel--axiom-check-parse-lint-line raw)))
          (when (and parsed (= (car parsed) inserted-line))
            (push (cdr parsed) matches))))
      (nreverse matches))))

(defun elot-gptel--axiom-check-consistency-on-draft (file draft reasoner)
  "Run `elot-gptel-tool-consistency' against DRAFT bytes.
Writes DRAFT to a temp .org file next to FILE inside the
project (so the project-root containment guard succeeds), runs
consistency, then deletes the temp file.  Returns the report
string."
  (let* ((dir (file-name-directory file))
         (tmp (make-temp-file
               (expand-file-name "elot-axcheck-" dir)
               nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert draft))
          (elot-gptel-tool-consistency tmp reasoner))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun elot-gptel-tool-axiom-check
    (file subject keyword fragment &optional consistency)
  "Pre-flight validate a candidate `KEYWORD :: FRAGMENT' row on SUBJECT.

Read-only.  Milestone 9 Step 9.2.b.  Resolves SUBJECT against
the buffer's `elot-slurp', checks that KEYWORD is legal on the
subject's kind (frame keyword for the subject's `rdf:type', or
an annotation property declared in FILE), synthesises the row
inside an in-memory copy of FILE, runs the existing lint
checkers (`elot/omn-keyword-appropriateness',
`elot/axiom-value-curies', `elot/axiom-keyword-range') against
the draft.  When CONSISTENCY is non-nil, also runs the
consistency reasoner on the draft (the only ROBOT-backed stage;
off by default to keep the inner authoring loop sub-second --
whole-file OMN parse is the post-commit responsibility of
`elot_edit_axiom' (9.2.c) and `elot_check').

Returns one of:
  - `OK: SUBJECT KEYWORD :: FRAGMENT validates ...' on full pass
  - `FAIL: ...' for any blocker, with an actionable next step
  - `ERROR: ...' for malformed input (unknown subject, etc.)

Never mutates FILE.  Pairs with `elot_axiom_keywords' (9.2.a)
and the future `elot_edit_axiom' (9.2.c)."
  (catch 'elot-axiom-check-result
    (condition-case err
        (progn
          (unless (and (stringp subject) (not (string-empty-p subject)))
            (user-error "subject must be a non-empty string"))
          (unless (and (stringp keyword) (not (string-empty-p keyword)))
            (user-error "keyword must be a non-empty string"))
          (unless (and (stringp fragment)
                       (not (string-empty-p (string-trim fragment))))
            (user-error "fragment must be a non-empty string"))
          (let* ((true (elot-gptel--resolve-file file))
                 (slurp (elot-gptel--axiom-slurp-for-file true)))
            (unless slurp
              (user-error
               "no resources declared in %s (elot-slurp empty)" file))
            (let ((row (elot-gptel--axiom-resolve-subject subject slurp)))
              (unless row
                (user-error
                 "subject %s not found in elot-slurp \
(try a declared CURIE or label)"
                 subject))
              (let* ((curie (car row))
                     (label (cadr row))
                     (kind  (elot-gptel--axiom-row-kind row))
                     (pretty (or (cdr (assoc kind
                                             elot-gptel--axiom-kind-pretty))
                                 kind "(unknown)")))
                ;; --- Stage 1: keyword legality (no draft needed) ---
                (unless (elot-gptel--axiom-check-keyword-legal-p
                         keyword kind slurp)
                  (let* ((legal (elot-omn-keywords-for-kind kind))
                         (aps (elot-gptel--axiom-check-declared-aps slurp))
                         (legal-str
                          (if legal (mapconcat #'identity legal ", ")
                            "(none)"))
                         (aps-str
                          (if aps (mapconcat #'identity aps ", ")
                            "(none)")))
                    (throw 'elot-axiom-check-result
                           (format
                            "FAIL: keyword %S is not legal on a%s %s.\n\
  Legal frame keywords on %s: %s.\n\
  Universal annotation rows are also legal: %s.\n\
  See elot_axiom_keywords for the full surface."
                            keyword
                            (if (string-match-p "\\`[AEIOU]" pretty)
                                "n" "")
                            pretty pretty legal-str aps-str))))
                ;; --- Stage 2: synthesise draft ---
                (let* ((src (with-temp-buffer
                              (insert-file-contents true)
                              (buffer-substring-no-properties
                               (point-min) (point-max))))
                       (synth (elot-gptel--axiom-synthesise-draft
                               src curie label keyword fragment))
                       (draft (plist-get synth :draft))
                       (ins-line (plist-get synth :line))
                       ;; --- Stage 3: static checkers on draft ---
                       (lint (elot-gptel-tool-lint
                              file "all"
                              (mapcar #'symbol-name
                                      elot-gptel--axiom-check-checkers)
                              draft))
                       (static-issues
                        (elot-gptel--axiom-check-filter-issues
                         lint ins-line)))
                  (when static-issues
                    (throw 'elot-axiom-check-result
                           (format
                            "FAIL: static checks rejected the row.\n\
  row: - %s :: %s\n  on subject %s (%s):\n    %s\n\
  HINT: declare any undeclared CURIE via elot_insert_sibling_resource / \
elot_insert_child_resource, or revise the fragment to match \
keyword %s's expected leaf kind."
                            keyword fragment curie pretty
                            (mapconcat #'identity static-issues
                                       "\n    ")
                            keyword)))
                  ;; --- Stage 4: optional consistency probe ---
                  ;; (Whole-file OMN parse is deliberately NOT run here;
                  ;; it duplicates the post-commit auto-revalidate done
                  ;; by `elot_edit_axiom' / `elot_check', and a JVM
                  ;; spin-up per fragment defeats the cheap-loop point
                  ;; of 9.2.b.  The static checkers above already cover
                  ;; keyword/kind/signature.  Reasoning has no static
                  ;; substitute, so the consistency probe stays.)
                  (let* ((robot-ok
                          (condition-case _e
                              (progn (require 'elot-robot)
                                     (and (fboundp 'elot-robot-available-p)
                                          (elot-robot-available-p)))
                            (error nil)))
                         (cons-report
                          (and consistency robot-ok
                               (elot-gptel--axiom-check-consistency-on-draft
                                true draft nil)))
                         (cons-fail
                          (and cons-report
                               (string-prefix-p "INCONSISTENT"
                                                cons-report))))
                    (when cons-fail
                      (throw 'elot-axiom-check-result
                             (format
                              "FAIL: consistency probe failed:\n\
%s\n  HINT: run elot_explain to surface the clashing axiom set."
                              cons-report)))
                    ;; --- Success summary ---
                    (let* ((stages
                            (append
                             '("keyword legal" "static checkers")
                             (and consistency robot-ok
                                  '("consistency"))))
                           (skipped
                            (cond
                             ((and consistency (not robot-ok))
                              " (consistency SKIPPED: ROBOT unavailable)")
                             ((not consistency)
                              " (consistency not requested)")
                             (t ""))))
                      (throw 'elot-axiom-check-result
                             (format
                              "OK: %s %s :: %s validates%s\n\
  stages run: %s"
                              curie keyword fragment skipped
                              (mapconcat #'identity stages
                                         " + "))))))))))
      (user-error (format "ERROR: %s" (error-message-string err)))
      (error      (format "ERROR: %s" (error-message-string err))))))

;;; ---------------------------------------------------------------------------
;;; Mutation HOF and Macro
;;; ---------------------------------------------------------------------------

(defun elot-gptel--rename-revalidate (file)
  "Re-run lint (+ OMN parse when ROBOT is up) against FILE.
Returns a plist (:ok BOOL :report STRING).  REPORT is empty when
:ok is t and there is nothing to surface; otherwise it carries
the diagnostic body the LLM should see."
  (let* ((lint (elot-gptel-tool-lint file))
         (lint-err (or (string-prefix-p "ERROR:" lint)
                       (elot-gptel--check-lint-failed-p lint)))
         (robot-ok (condition-case _err
                       (progn (require 'elot-robot)
                              (and (fboundp 'elot-robot-available-p)
                                   (elot-robot-available-p)))
                     (error nil)))
         (omn (and (not lint-err) robot-ok
                   (elot-gptel-tool-omn-validate file)))
         (omn-err (and omn (elot-gptel--check-omn-failed-p omn))))
    (list :ok (not (or lint-err omn-err))
          :report
          (concat
           (when lint-err
             (concat "== LINT ==\n" lint))
           (when omn-err
             (concat (when lint-err "\n\n")
                     "== OMN PARSE ==\n" omn))
           (unless (or lint-err omn-err)
             (concat "== LINT ==\n" lint
                     (when omn
                       (concat "\n\n== OMN PARSE ==\n" omn))))))))

(defun elot-gptel--axioms-revalidate-content (file draft)
  "Run lint (+ OMN parse when ROBOT is up) against DRAFT for FILE.
Mirrors `elot-gptel--rename-revalidate' but drives the read-only
`content=' code path so DRAFT can be evaluated without touching
the file on disk -- used by `dry_run' batches."
  (let* ((lint (elot-gptel-tool-lint file nil nil draft))
         (lint-err (or (string-prefix-p "ERROR:" lint)
                       (elot-gptel--check-lint-failed-p lint)))
         (robot-ok (condition-case _err
                       (progn (require 'elot-robot)
                              (and (fboundp 'elot-robot-available-p)
                                   (elot-robot-available-p)))
                     (error nil)))
         (omn (and (not lint-err) robot-ok
                   (elot-gptel-tool-omn-validate file nil draft)))
         (omn-err (and omn (elot-gptel--check-omn-failed-p omn))))
    (list :ok (not (or lint-err omn-err))
          :report
          (concat
           (when lint-err
             (concat "== LINT ==\n" lint))
           (when omn-err
             (concat (when lint-err "\n\n")
                     "== OMN PARSE ==\n" omn))
           (unless (or lint-err omn-err)
             (concat "== LINT ==\n" lint
                     (when omn
                       (concat "\n\n== OMN PARSE ==\n" omn))))))))

(defun elot-gptel--apply-mutation (file dry-run callback)
  "Execute a side-effecting mutation on FILE via CALLBACK.
Handles side-effects gating, buffer setup, snapshotting, saving,
auto-revalidation, and rollback on failure.

CALLBACK is a function of zero arguments, executed within the context
of FILE's buffer. It must perform the buffer modifications and return
the success header string (e.g. \"OK: inserted 1 heading\").

Returns the final response string (success header + validation reports,
or ERROR/FAIL line)."
  (condition-case err
      (progn
        (unless (or dry-run elot-gptel-allow-side-effects)
          (user-error
           "elot-gptel: mutation refused -- side effects disabled \
(set `elot-gptel-allow-side-effects' to t, or pass dry_run=true)"))
        (let* ((true-file (elot-gptel--resolve-file file))
               (buf (or (find-buffer-visiting true-file)
                        (let ((inhibit-message t))
                          (find-file-noselect true-file 'nowarn)))))
          (unless (buffer-live-p buf)
            (user-error "elot-gptel: cannot open %s" file))
          (with-current-buffer buf
            (unless (derived-mode-p 'org-mode)
              (let ((org-inhibit-startup t) (org-mode-hook nil))
                (org-mode)))
            (let ((before-text (buffer-substring-no-properties (point-min) (point-max)))
                  (before-modified-p (buffer-modified-p))
                  (success-header nil))
              (setq success-header (funcall callback))
              (unless (stringp success-header)
                (error "elot-gptel--apply-mutation: callback must return a string"))
              
              (let ((after-text (buffer-substring-no-properties (point-min) (point-max))))
                (if (equal before-text after-text)
                    success-header
                  (cond
                   (dry-run
                    (let* ((verdict (elot-gptel--axioms-revalidate-content file after-text))
                           (ok (plist-get verdict :ok))
                           (report (plist-get verdict :report)))
                      (let ((inhibit-read-only t))
                        (widen)
                        (erase-buffer)
                        (insert before-text))
                      (set-buffer-modified-p before-modified-p)
                      (when (fboundp 'elot-headline-hierarchy-mark-stale)
                        (ignore-errors (elot-headline-hierarchy-mark-stale)))
                      (cond
                       ((not ok)
                        (concat "FAIL: dry-run revalidation failed (no file written)\n\n" report))
                       (t
                        (let ((dry-note "  (dry_run: file unchanged on disk)"))
                          (concat (if (string-match "\n" success-header)
                                      (replace-match (concat dry-note "\n") t t success-header)
                                    (concat success-header dry-note))
                                  (if (and report (not (string-empty-p report)))
                                      (concat "\n\n" report)
                                    "")))))))
                   (t
                    (save-buffer)
                    (let* ((verdict (elot-gptel--rename-revalidate file))
                           (ok (plist-get verdict :ok))
                           (report (plist-get verdict :report)))
                      (cond
                       ((not ok)
                        (let ((inhibit-read-only t))
                          (widen)
                          (erase-buffer)
                          (insert before-text))
                        (save-buffer)
                        (set-buffer-modified-p before-modified-p)
                        (when (fboundp 'elot-headline-hierarchy-mark-stale)
                          (ignore-errors (elot-headline-hierarchy-mark-stale)))
                        (concat "ERROR: revalidation failed -- changes rolled back\n\n" report))
                       (t
                        (concat success-header
                                (if (and report (not (string-empty-p report)))
                                    (concat "\n\n" report)
                                  "")))))))))))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

(defmacro elot-gptel-with-mutation (args &rest body)
  "Evaluate BODY as a buffer mutation.
ARGS is (FILE DRY-RUN).
The current buffer will be the target file's buffer.
BODY must return the success header string."
  (declare (indent 1) (debug (listp body)))
  `(elot-gptel--apply-mutation ,(car args) ,(cadr args) (lambda () ,@body)))

;;; ---------------------------------------------------------------------------
;;; elot_edit_axiom tool (Milestone 9 Step 9.2.c)
;;; ---------------------------------------------------------------------------
;;;
;;; Writer counterpart of the 9.2.b pre-flight: commits a single
;;; description-list row `- KEYWORD :: FRAGMENT' on SUBJECT to
;;; FILE, with auto-revalidate + rollback.  Three operations:
;;;   add     -- append a new row as the LAST top-level row of
;;;              SUBJECT's description list.
;;;   replace -- find the row matched by (keyword, match_fragment
;;;              or fragment fallback) and rewrite its value to
;;;              FRAGMENT.  Nested annotation children are
;;;              preserved verbatim.
;;;   delete  -- find the matched row and delete it together with
;;;              its nested annotation children.
;;;
;;; Match resolution refuses on 0 or >1 hits with an actionable
;;; ERROR: line.  Index-based addressing is deliberately absent
;;; (silent invalidation between turns).
;;;
;;; Pre-flight handshake: 9.2.c does NOT re-run 9.2.b's static
;;; checkers; the caller is expected to have run
;;; `elot_axiom_check' first.  Post-commit revalidation is the
;;; standard `elot-gptel--rename-revalidate' pipeline (lint +
;;; OMN parse when ROBOT is configured).  Failure rolls the
;;; buffer back to its pre-edit contents.

(defconst elot-gptel--axiom-toplevel-row-re
  "^ - \\(?:\\[[^]]*\\] \\)?\\(\\S-+\\) ::\\(?:[ \t]+\\(.*\\)\\|[ \t]*\\)$"
  "Regex matching a top-level ELOT description-list row.
Group 1 is the keyword (after an optional `[X]'-style checkbox
cookie).  Group 2 is the value text, or nil when the row has no
value (Org-mode writes empty rows as `- KEY ::' with no trailing
whitespace).  Top-level rows carry a single leading space; nested
annotation rows under an axiom row carry three or more.")

(defconst elot-gptel--axiom-match-empty 'elot-gptel--match-empty-value
  "Sentinel returned by `elot-gptel--axiom-normalise-fragment' for
an empty / whitespace-only input string.  Distinct from nil (which
means \"no matcher supplied\"), so the matcher in
`elot-gptel--axiom-match-rows' can target rows whose value is
empty or whitespace-only.  A symbol is used so it cannot collide
with any string a caller might write as a fragment.")

(defun elot-gptel--axiom-normalise-fragment (s)
  "Return S with internal whitespace runs collapsed and trimmed.

Returns nil for nil or non-string input (= no matcher supplied),
EXCEPT that `elot-gptel--axiom-match-empty' passes through
unchanged so the helper is idempotent on the sentinel (callers
that hand a previously-normalised matcher back through the
normaliser -- e.g. `elot-gptel--axiom-match-rows', which
re-normalises its MATCH-FRAGMENT argument -- must not lose the
empty-row signal that distinguishes \"match empty\" from \"no
matcher\").  Returns `elot-gptel--axiom-match-empty' for a string
that is empty or whitespace-only after trimming (= match rows
whose value is empty); see that constant's docstring for the
rationale.  Otherwise returns the normalised non-empty string."
  (cond
   ((eq s elot-gptel--axiom-match-empty) s)
   ((null s) nil)
   ((not (stringp s)) nil)
   ((string-empty-p s) elot-gptel--axiom-match-empty)
   (t
    (let ((n (string-trim
              (replace-regexp-in-string "[ \t\n\r]+" " " s))))
      (if (string-empty-p n)
          elot-gptel--axiom-match-empty
        n)))))

(defun elot-gptel--axiom-find-rows-in-buffer (heading-pos subtree-end)
  "Return the list of top-level description-list rows under HEADING-POS.
Each element is a plist (:keyword K :fragment V :start-line L
:end-line E :nested-count N), where :end-line is the last line
of the row's nested-annotation subtree (or :start-line when the
row has no nested children) and :nested-count is the number of
nested annotation rows under the parent.  Operates on the
current buffer; SUBTREE-END caps the scan."
  (save-excursion
    (goto-char heading-pos)
    (forward-line 1)
    ;; Skip an optional :PROPERTIES: drawer.
    (when (looking-at "[ \t]*:PROPERTIES:")
      (when (re-search-forward "^[ \t]*:END:[ \t]*$" subtree-end t)
        (forward-line 1)))
    (let (rows)
      (while (and (< (point) subtree-end)
                  (not (looking-at "^\\*+ ")))
        (cond
         ((looking-at elot-gptel--axiom-toplevel-row-re)
          (let* ((kw   (match-string-no-properties 1))
                 (val  (or (match-string-no-properties 2) ""))
                 (start (line-number-at-pos))
                 (end   start)
                 (nested 0))
            (forward-line 1)
            (while (and (< (point) subtree-end)
                        (not (looking-at "^\\*+ "))
                        (looking-at "^ \\{3,\\}"))
              (when (looking-at "^ \\{3,\\}- ")
                (cl-incf nested))
              (setq end (line-number-at-pos))
              (forward-line 1))
            (push (list :keyword kw :fragment val
                        :start-line start :end-line end
                        :nested-count nested)
                  rows)))
         (t (forward-line 1))))
      (nreverse rows))))

(defun elot-gptel--axiom-find-rows
    (file-contents subject-curie subject-label)
  "Return rows on SUBJECT's description list in FILE-CONTENTS."
  (with-temp-buffer
    (insert file-contents)
    (let ((org-inhibit-startup t) (org-mode-hook nil))
      (org-mode))
    (let* ((heading-pos
            (elot-gptel--axiom-check-find-subject-heading
             subject-curie subject-label))
           (subtree-end
            (save-excursion
              (goto-char heading-pos)
              (org-end-of-subtree t t)
              (point))))
      (elot-gptel--axiom-find-rows-in-buffer heading-pos subtree-end))))

(defun elot-gptel--axiom-match-rows (rows keyword &optional match-fragment)
  "Return rows whose :keyword equals KEYWORD (and optionally :fragment matches).
Fragment match is ASCII-normalised (whitespace collapsed and
trimmed) on both sides.  When MATCH-FRAGMENT is nil all rows
with the matching keyword pass.  When MATCH-FRAGMENT normalises
to `elot-gptel--axiom-match-empty' (the sentinel returned for
empty / whitespace-only input), only rows whose value is empty
or whitespace-only pass -- this is how callers target an empty
row when a populated sibling exists under the same keyword."
  (let ((nf (elot-gptel--axiom-normalise-fragment match-fragment)))
    (cl-remove-if-not
     (lambda (r)
       (and (equal (plist-get r :keyword) keyword)
            (cond
             ((null nf) t)
             ((eq nf elot-gptel--axiom-match-empty)
              (let ((v (plist-get r :fragment)))
                (or (null v)
                    (and (stringp v)
                         (string-empty-p (string-trim v))))))
             (t (equal (elot-gptel--axiom-normalise-fragment
                        (plist-get r :fragment))
                       nf)))))
     rows)))

(defun elot-gptel--axiom-apply-edit
    (file-contents subject-curie subject-label
                   op keyword fragment match-row)
  "Return (:draft STRING :verb STRING :nested-count N) for the edit.
OP is one of `add', `replace', `delete'.  For `add' MATCH-ROW
is ignored; for `replace' / `delete' it is the row plist
returned by `elot-gptel--axiom-find-rows'."
  (with-temp-buffer
    (insert file-contents)
    (let ((org-inhibit-startup t) (org-mode-hook nil))
      (org-mode))
    (let* ((heading-pos
            (elot-gptel--axiom-check-find-subject-heading
             subject-curie subject-label))
           (subtree-end
            (save-excursion
              (goto-char heading-pos)
              (org-end-of-subtree t t)
              (point))))
      (pcase op
        ('add
         (goto-char heading-pos)
         (let* ((heading-end (line-end-position))
                (next-heading
                 (save-excursion
                   (goto-char heading-end)
                   (if (re-search-forward "^\\*+ " subtree-end t)
                       (line-beginning-position)
                     subtree-end))))
           (goto-char next-heading)
           (unless (bolp) (insert "\n"))
           (insert (format " - %s :: %s\n" keyword fragment)))
         (list :draft (buffer-substring-no-properties
                       (point-min) (point-max))
               :verb "added" :nested-count 0))
        ('replace
         (goto-char (point-min))
         (forward-line (1- (plist-get match-row :start-line)))
         (delete-region (point) (line-end-position))
         (insert (format " - %s :: %s" keyword fragment))
         (list :draft (buffer-substring-no-properties
                       (point-min) (point-max))
               :verb "replaced"
               :nested-count (plist-get match-row :nested-count)))
        ('delete
         (let (beg end)
           (goto-char (point-min))
           (forward-line (1- (plist-get match-row :start-line)))
           (setq beg (point))
           (goto-char (point-min))
           (forward-line (plist-get match-row :end-line))
           (setq end (point))
           (delete-region beg end)
           ;; Tidy (Option A): collapse any run of blank lines that
           ;; became adjacent to the cut down to zero.  A deletion
           ;; never adds blanks, but it can surface a run of
           ;; consecutive blanks previously masked by the removed
           ;; row (e.g. blanks between the heading and the row, or
           ;; between two rows).  Inside an Org description list
           ;; blank lines break the list, so removing them is a
           ;; correctness fix, not a style choice.  The scan is
           ;; strictly local to the cut point; it touches no other
           ;; whitespace in the subject region or beyond.
           (save-excursion
             (goto-char beg)
             ;; Eat blank lines at/after the cut.
             (while (and (not (eobp))
                         (looking-at "^[ \t]*$"))
               (delete-region (point)
                              (min (point-max)
                                   (1+ (line-end-position)))))
             ;; Eat blank lines immediately preceding the cut.
             (while (and (> (point) (point-min))
                         (save-excursion
                           (forward-line -1)
                           (looking-at "^[ \t]*$")))
               (forward-line -1)
               (delete-region (point)
                              (min (point-max)
                                   (1+ (line-end-position)))))))
         (list :draft (buffer-substring-no-properties
                       (point-min) (point-max))
               :verb "deleted"
               :nested-count (plist-get match-row :nested-count)))
        (_ (user-error "elot-gptel: unknown op: %S" op))))))

(defun elot-gptel--axiom-row-empty-p (row)
  "Return non-nil when ROW's :fragment is nil or whitespace-only.
ROW is a row plist returned by `elot-gptel--axiom-find-rows'."
  (let ((v (plist-get row :fragment)))
    (or (null v)
        (and (stringp v)
             (string-empty-p (string-trim v))))))

(defun elot-gptel--axiom-delete-empty-rows
    (file-contents subject-curie subject-label &optional keyword)
  "Delete every empty (or whitespace-only) top-level row on SUBJECT.

FILE-CONTENTS is the buffer-contents string to operate on (the
running draft for the batch tool, or the buffer's current bytes
for the singleton).  SUBJECT-CURIE / SUBJECT-LABEL identify the
subject heading the same way `elot-gptel--axiom-apply-edit'
does.  When KEYWORD is non-nil and non-empty, restrict the
sweep to rows whose left-hand keyword equals KEYWORD;
otherwise sweep every empty row regardless of keyword.

Returns a plist (:draft STRING :deleted-keywords (KW ...))
where :draft is FILE-CONTENTS with the matching rows (and
their nested annotation children) removed, and
:deleted-keywords is the list of keywords whose rows were
removed, in document order.  When no row matches, the draft
equals FILE-CONTENTS unchanged and :deleted-keywords is nil --
callers treat that as the idempotent no-op success case.

The implementation finds all matching rows in one pass, then
deletes them via `elot-gptel--axiom-apply-edit' from the
bottom up (highest :start-line first) so the line-number
coordinates recorded for earlier rows remain valid through
the sweep."
  (let* ((kw (and (stringp keyword) (not (string-empty-p keyword))
                  keyword))
         (rows (elot-gptel--axiom-find-rows
                file-contents subject-curie subject-label))
         (empties (cl-remove-if-not
                   (lambda (r)
                     (and (elot-gptel--axiom-row-empty-p r)
                          (or (null kw)
                              (equal (plist-get r :keyword) kw))))
                   rows))
         ;; Document-order list of keywords we will delete (for the
         ;; caller's success summary).
         (deleted-keywords
          (mapcar (lambda (r) (plist-get r :keyword)) empties))
         ;; Delete from the bottom up so earlier rows' line
         ;; coordinates stay valid.
         (descending (sort (copy-sequence empties)
                           (lambda (a b)
                             (> (plist-get a :start-line)
                                (plist-get b :start-line)))))
         (draft file-contents))
    (dolist (row descending)
      (let ((res (elot-gptel--axiom-apply-edit
                  draft subject-curie subject-label
                  'delete (plist-get row :keyword) nil row)))
        (setq draft (plist-get res :draft))))
    (list :draft draft
          :deleted-keywords deleted-keywords)))

(defun elot-gptel-tool-edit-axiom
    (file subject keyword &optional fragment op match-fragment)
  "Commit a single description-list row `- KEYWORD :: FRAGMENT' on SUBJECT.

Implementation of the `elot_edit_axiom' tool (Milestone 9 Step
9.2.c).  Side-effecting -- gated by
`elot-gptel-allow-side-effects'.

OP is one of `add' (default), `replace', `delete', `delete-empty':
  - `add' appends a new row as the last top-level row of
    SUBJECT's description list.  FRAGMENT is the new value.
  - `replace' finds the row whose keyword equals KEYWORD and
    whose existing value matches MATCH-FRAGMENT (or FRAGMENT
    when MATCH-FRAGMENT is omitted and the keyword is unique on
    SUBJECT), and rewrites its value to FRAGMENT.  Nested
    annotation children are preserved verbatim.
  - `delete' finds the matched row (matcher: MATCH-FRAGMENT or
    FRAGMENT) and deletes it together with its nested
    annotation children.
  - `delete-empty' sweeps every row on SUBJECT whose value is
    empty or whitespace-only and deletes them in one atomic
    edit.  When KEYWORD is supplied the sweep is restricted to
    rows with that keyword; when KEYWORD is the empty string the
    sweep runs over every keyword on SUBJECT.  FRAGMENT and
    MATCH-FRAGMENT are ignored.  Zero matches is a success
    (idempotent no-op).

Match resolution refuses with an ERROR: line on 0 matches or
>1 ambiguous matches (does not apply to `delete-empty', which
treats 0 matches as success).

Caller is expected to have run `elot_axiom_check' first; this
tool trusts its input and does not re-run the 9.2.b static
checkers.  Post-commit revalidation runs `elot_lint' (and
`elot_omn_validate' when ROBOT is configured); failure rolls
the buffer back to its pre-edit contents and returns the
diagnostic under a `FAIL:' header."
  (condition-case err
      (let* ((op-sym
              (cond
               ((or (null op)
                    (and (stringp op) (string-empty-p op)))
                'add)
               ((stringp op) (intern op))
               ((symbolp op) op)
               (t (user-error
                   "op must be `add', `replace', `delete', or `delete-empty': %S" op)))))
        (unless (memq op-sym '(add replace delete delete-empty))
          (user-error
           "op must be `add', `replace', `delete', or `delete-empty': %S" op))
        ;; `delete-empty' relaxes the keyword requirement (empty /
        ;; nil means "sweep every keyword").  Other ops still
        ;; require a non-empty keyword.
        (unless (eq op-sym 'delete-empty)
          (unless (and (stringp keyword) (not (string-empty-p keyword)))
            (user-error "keyword must be a non-empty string")))
        (when (and (memq op-sym '(add replace))
                   (or (null fragment)
                       (and (stringp fragment)
                            (string-empty-p (string-trim fragment)))))
          (user-error "fragment must be a non-empty string for %s"
                      (symbol-name op-sym)))
        (elot-gptel-with-mutation (file nil)
          (unless (and (stringp subject) (not (string-empty-p subject)))
            (user-error "subject must be a non-empty string"))
          (let* ((true (elot-gptel--resolve-file file))
                 (slurp (elot-gptel--axiom-slurp-for-file true)))
            (unless slurp
              (user-error
               "no resources declared in %s (elot-slurp empty)" file))
            (let ((row (elot-gptel--axiom-resolve-subject subject slurp)))
              (unless row
                (user-error
                 "subject %s not found in elot-slurp \
(try a declared CURIE or label)"
                 subject))
              (let* ((curie (car row))
                     (label (cadr row))
                     (before-text (buffer-string)))
                (cond
                 ;; --- delete-empty branch: bulk sweep --------------
                 ((eq op-sym 'delete-empty)
                  (let* ((sweep (elot-gptel--axiom-delete-empty-rows
                                 before-text curie label
                                 (and (stringp keyword)
                                      (not (string-empty-p keyword))
                                      keyword)))
                         (draft (plist-get sweep :draft))
                         (deleted (plist-get sweep :deleted-keywords))
                         (n (length deleted)))
                    (cond
                     ((zerop n)
                      ;; Idempotent no-op: return success WITHOUT
                      ;; writing or saving (file is unchanged).
                      (format
                       "OK: no empty rows on %s%s"
                       curie
                       (if (and (stringp keyword)
                                (not (string-empty-p keyword)))
                           (format " for keyword %S" keyword)
                         "")))
                     (t
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert draft))
                      (format
                       "OK: deleted %d empty row%s on %s: %s"
                       n (if (= n 1) "" "s") curie
                       (mapconcat #'identity deleted ", "))))))
                 ;; --- add / replace / delete branch (original) ----
                 (t
                  (let* (;; Matcher: explicit MATCH-FRAGMENT wins; else
                         ;; fall back to FRAGMENT (delete's natural
                         ;; shape and 9.2.b's `old' equivalent).
                         (matcher
                          (or (elot-gptel--axiom-normalise-fragment
                               match-fragment)
                              (and (eq op-sym 'delete)
                                   (elot-gptel--axiom-normalise-fragment
                                    fragment))))
                         match-row)
                    (when (memq op-sym '(replace delete))
                      (let* ((rows (elot-gptel--axiom-find-rows
                                    before-text curie label))
                             (matches (elot-gptel--axiom-match-rows
                                       rows keyword matcher)))
                        (cond
                         ((null matches)
                          (user-error
                           "no row matches keyword %S%s on %s"
                           keyword
                           (if matcher
                               (format " with fragment %S" matcher)
                             "")
                           curie))
                         ((> (length matches) 1)
                          (user-error
                           "ambiguous; %d rows match keyword %S on %s; \
supply `match_fragment'"
                           (length matches) keyword curie))
                         (t (setq match-row (car matches))))))
                    (let* ((edit (elot-gptel--axiom-apply-edit
                                  before-text curie label
                                  op-sym keyword fragment match-row))
                           (draft (plist-get edit :draft))
                           (nested (plist-get edit :nested-count)))
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert draft))
                      (pcase op-sym
                        ('add
                         (format
                          "OK: added 1 row on %s: - %s :: %s"
                          curie keyword fragment))
                        ('replace
                         (format
                          "OK: replaced 1 row on %s: - %s :: %s%s"
                          curie keyword fragment
                          (if (> nested 0)
                              (format
                               " (preserved %d nested annotation%s)"
                               nested
                               (if (= nested 1) "" "s"))
                            "")))
                        ('delete
                         (format
                          "OK: deleted 1 row + %d nested annotation%s on %s"
                          nested
                          (if (= nested 1) "" "s")
                          curie))))))))))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_edit_axioms batch tool (Milestone 9 Step 9.2.c.1)
;;; ---------------------------------------------------------------------------
;;;
;;; Batch counterpart of `elot_edit_axiom'.  Applies an ordered list
;;; of (subject, op, keyword, fragment, match_fragment) edits to a
;;; single FILE atomically: snapshot -> per-edit in-memory apply ->
;;; save once -> single post-commit revalidate -> rollback on
;;; failure.  Motivating use case: the chicken-and-egg case where
;;; two independent OMN axiom errors block each other -- fixing
;;; either one in isolation still triggers a revalidate rollback
;;; because the other is still broken.  A batch fixes both in the
;;; same revalidate window.
;;;
;;; Per-edit match resolution sees the buffer as the prior edits
;;; left it, so a `replace' that follows an `add' of the same
;;; keyword resolves against the running draft, not the original
;;; bytes.  A failure at edit K (subject unknown, no row matches,
;;; ambiguous match) aborts the batch with `FAIL at edits[K]:' and
;;; leaves disk untouched.
;;;
;;; DRY_RUN runs the full pipeline (each edit applied to the
;;; running draft, then `elot_lint' / `elot_omn_validate' invoked
;;; with `content=DRAFT') but skips the actual `save-buffer'.  The
;;; LLM gets back the same OK / FAIL envelope it would have got
;;; from a real commit; the file on disk is byte-identical to its
;;; pre-call state.

(defun elot-gptel--axioms-edit-get (edit key)
  "Return EDIT's value for KEY.
EDIT may arrive as a hash-table (gptel's default JSON decode), an
alist (string- or symbol-keyed), or a plist.  KEY is a string."
  (cond
   ((hash-table-p edit)
    (or (gethash key edit)
        (gethash (intern key) edit)
        (gethash (intern (concat ":" key)) edit)))
   ((and (consp edit) (consp (car edit)))
    (or (cdr (assoc key edit))
        (cdr (assoc (intern key) edit))
        (let ((kw (intern (concat ":" key))))
          (and (memq kw edit) (plist-get edit kw)))))
   ((and (listp edit) (keywordp (car-safe edit)))
    (plist-get edit (intern (concat ":" key))))
   (t nil)))

(defun elot-gptel--axioms-coerce-edits (edits)
  "Return EDITS as a list (decoding vector / nil shapes).
Empty input yields nil; non-sequence input is returned unchanged
so the validator surfaces the structured ERROR."
  (cond
   ((vectorp edits) (append edits nil))
   ((listp edits) edits)
   (t edits)))

(defun elot-gptel--axioms-normalise-edit (edit idx)
  "Return a normalised plist
(:subject :op :keyword :fragment :match-fragment) for EDIT.
IDX is the 0-based position used in error messages.  Signals
`user-error' with a `edits[IDX]:' prefix on shape problems so the
caller can attribute the failure to a specific entry."
  (unless (or (hash-table-p edit) (listp edit))
    (user-error "edits[%d]: must be an object, got %S" idx edit))
  (let* ((subject (elot-gptel--axioms-edit-get edit "subject"))
         (op      (elot-gptel--axioms-edit-get edit "op"))
         (keyword (elot-gptel--axioms-edit-get edit "keyword"))
         (fragment (elot-gptel--axioms-edit-get edit "fragment"))
         (match-fragment
          (elot-gptel--axioms-edit-get edit "match_fragment"))
         (op-sym
          (cond
           ((or (null op) (and (stringp op) (string-empty-p op))) 'add)
           ((stringp op) (intern op))
           ((symbolp op) op)
           (t (user-error
               "edits[%d]: op must be `add', `replace', `delete', or `delete-empty': %S"
               idx op)))))
    (unless (memq op-sym '(add replace delete delete-empty))
      (user-error
       "edits[%d]: op must be `add', `replace', `delete', or `delete-empty': %S"
       idx op))
    (unless (and (stringp subject) (not (string-empty-p subject)))
      (user-error "edits[%d]: subject must be a non-empty string"
                  idx))
    ;; `delete-empty' relaxes the keyword requirement (empty / nil
    ;; means "sweep every keyword on SUBJECT").  Other ops still
    ;; require a non-empty keyword.
    (unless (eq op-sym 'delete-empty)
      (unless (and (stringp keyword) (not (string-empty-p keyword)))
        (user-error "edits[%d]: keyword must be a non-empty string"
                    idx)))
    (when (and (memq op-sym '(add replace))
               (or (null fragment)
                   (and (stringp fragment)
                        (string-empty-p (string-trim fragment)))))
      (user-error
       "edits[%d]: fragment must be a non-empty string for %s"
       idx (symbol-name op-sym)))
    (list :subject subject
          :op op-sym
          :keyword keyword
          :fragment fragment
          :match-fragment match-fragment)))

(defun elot-gptel--axioms-apply-one (draft slurp edit idx)
  "Apply EDIT (a normalised plist) to DRAFT.  Return the result plist.
DRAFT is the running file-contents string; SLURP is the initial
buffer's `elot-slurp' (subject identities do not change as
description-list rows are added / replaced / deleted).  IDX is
the edit's 0-based position, used to scope error messages.
Returns (:draft NEW :verb V :curie C :keyword K :nested N)."
  (let* ((subject (plist-get edit :subject))
         (op      (plist-get edit :op))
         (keyword (plist-get edit :keyword))
         (fragment (plist-get edit :fragment))
         (match-fragment (plist-get edit :match-fragment))
         (row (elot-gptel--axiom-resolve-subject subject slurp)))
    (unless row
      (user-error
       "edits[%d]: subject %s not found (try a declared CURIE or label)"
       idx subject))
    (let* ((curie (car row))
           (label (cadr row)))
      (cond
       ;; `delete-empty' sweep: bypass the match-rows path entirely.
       ((eq op 'delete-empty)
        (let* ((kw (and (stringp keyword)
                        (not (string-empty-p keyword))
                        keyword))
               (sweep (elot-gptel--axiom-delete-empty-rows
                       draft curie label kw))
               (deleted (plist-get sweep :deleted-keywords)))
          (list :draft (plist-get sweep :draft)
                :verb "swept"
                :nested 0
                :curie curie
                :keyword (or kw "(any)")
                :swept-keywords deleted)))
       (t
        (let* ((matcher
                (or (elot-gptel--axiom-normalise-fragment match-fragment)
                    (and (eq op 'delete)
                         (elot-gptel--axiom-normalise-fragment fragment))))
               match-row)
          (when (memq op '(replace delete))
            (let* ((rows (elot-gptel--axiom-find-rows draft curie label))
                   (matches (elot-gptel--axiom-match-rows
                             rows keyword matcher)))
              (cond
               ((null matches)
                (user-error
                 "edits[%d]: no row matches keyword %S%s on %s"
                 idx keyword
                 (if matcher (format " with fragment %S" matcher) "")
                 curie))
               ((> (length matches) 1)
                (user-error
                 "edits[%d]: ambiguous; %d rows match keyword %S on %s; \
supply `match_fragment'"
                 idx (length matches) keyword curie))
               (t (setq match-row (car matches))))))
          (let ((result (elot-gptel--axiom-apply-edit
                         draft curie label op keyword fragment match-row)))
            (list :draft (plist-get result :draft)
                  :verb  (plist-get result :verb)
                  :nested (plist-get result :nested-count)
                  :curie curie
                  :keyword keyword))))))))

(defun elot-gptel--axioms-format-summary (summaries)
  "Render SUMMARIES (a list of per-edit plists) as a bulleted block."
  (mapconcat
   (lambda (s)
     (let ((swept (plist-get s :swept-keywords)))
       (cond
        (swept
         (format "  - edits[%d]: swept %d empty row%s on %s%s"
                 (plist-get s :idx)
                 (length swept)
                 (if (= (length swept) 1) "" "s")
                 (plist-get s :curie)
                 (if swept
                     (concat ": " (mapconcat #'identity swept ", "))
                   "")))
        ((and (stringp (plist-get s :verb))
              (string= (plist-get s :verb) "swept"))
         ;; Sweep with zero matches (idempotent no-op).
         (format "  - edits[%d]: no empty rows on %s%s"
                 (plist-get s :idx)
                 (plist-get s :curie)
                 (let ((kw (plist-get s :keyword)))
                   (if (and kw (not (string= kw "(any)")))
                       (format " for keyword %S" kw)
                     ""))))
        (t
         (format "  - edits[%d]: %s on %s (%s)%s"
                 (plist-get s :idx)
                 (plist-get s :verb)
                 (plist-get s :curie)
                 (plist-get s :keyword)
                 (let ((n (plist-get s :nested)))
                   (if (and (integerp n) (> n 0))
                       (format " [%d nested]" n) "")))))))
   summaries "\n"))

(defun elot-gptel-tool-edit-axioms (file edits &optional dry-run)
  "Commit a batch of description-list-row edits to FILE atomically.

Implementation of the `elot_edit_axioms' tool (Milestone 9 Step
9.2.c.1).  Side-effecting -- gated by
`elot-gptel-allow-side-effects' unless DRY-RUN is non-nil.

EDITS is an ordered list (or JSON array) of edit objects; each
object has the same shape as the args to `elot_edit_axiom' minus
FILE:
  - subject         (required) -- CURIE or rdfs:label
  - op              (optional, default `add')
                     -- `add' / `replace' / `delete' / `delete-empty'
  - keyword         (required for add/replace/delete; optional for delete-empty)
  - fragment        (required for `add' / `replace')
  - match_fragment  (optional matcher for `replace' / `delete';
                     pass \"\" to target an empty-value row)

Edits apply in order.  Per-edit match resolution runs against
the running draft, so later edits see earlier ones.  A failure
at edit K (subject not found, no row matches, ambiguous match)
aborts the batch with `FAIL at edits[K]:' and leaves FILE
byte-identical to its pre-call state.  The `delete-empty' op
treats zero matches as success (idempotent no-op), in line with
the singleton tool.

The whole batch runs inside a single revalidate window: after
all edits have been applied to the in-memory draft, the file is
saved once and `elot_lint' / `elot_omn_validate' run on the
post-batch bytes.  Revalidation failure rolls the buffer back to
its pre-batch contents and re-saves.

DRY-RUN, when non-nil, skips the actual `save-buffer'.  The full
edit pipeline still runs in memory and the revalidate stage runs
against the draft via the read-only `content=' code path; the
LLM gets back the same OK / FAIL envelope it would have got from
a real commit, but FILE on disk is unchanged."
  (condition-case err
      (let* ((coerced (elot-gptel--axioms-coerce-edits edits)))
        (unless (and (listp coerced) coerced)
          (user-error "edits must be a non-empty array"))
        ;; Normalise every edit up front so shape errors surface
        ;; before we open the buffer or touch anything.
        (let* ((normalised
                (let ((i -1))
                  (mapcar (lambda (e)
                            (elot-gptel--axioms-normalise-edit
                             e (cl-incf i)))
                          coerced))))
          (elot-gptel-with-mutation (file dry-run)
            (let* ((true (elot-gptel--resolve-file file))
                   (slurp (elot-gptel--axiom-slurp-for-file true)))
              (unless slurp
                (user-error
                 "no resources declared in %s (elot-slurp empty)" file))
              (let* ((draft (buffer-string))
                     (i -1)
                     (summaries nil))
                ;; Per-edit apply on the running draft.  A failure
                ;; here aborts the whole batch before any save.
                (dolist (edit normalised)
                  (cl-incf i)
                  (let ((res (elot-gptel--axioms-apply-one
                              draft slurp edit i)))
                    (setq draft (plist-get res :draft))
                    (push (list :idx i
                                :verb (plist-get res :verb)
                                :curie (plist-get res :curie)
                                :keyword (plist-get res :keyword)
                                :nested (plist-get res :nested)
                                :swept-keywords
                                (plist-get res :swept-keywords))
                          summaries)))
                (setq summaries (nreverse summaries))
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert draft))
                (format
                 "OK: applied %d edit%s\n%s"
                 (length summaries)
                 (if (= (length summaries) 1) "" "s")
                 (elot-gptel--axioms-format-summary summaries)))))))
    (user-error (format "FAIL at %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))

;;; ---------------------------------------------------------------------------
;;; elot_rename_resource tool (Milestone 12 Step 12.4)
;;; ---------------------------------------------------------------------------
;;;
;;; LLM-facing wrapper around `elot-rename-resource' (see
;;; `elot-id-rename.el').  Side-effecting: mutates the user's .org
;;; file in place, gated by `elot-gptel-allow-side-effects'.
;;;
;;; Pipeline:
;;;   1. Gate check (side-effects, arg shape).
;;;   2. Probe target prefix: when undeclared in the file's
;;;      prefix-table and TARGET_IRI was not supplied, refuse with
;;;      a structured ERROR: line carrying `elot-db' candidates so
;;;      the LLM can retry.
;;;   3. Snapshot buffer; run `elot-rename-resource' (programmatic
;;;      path: passes :target-iri when supplied).
;;;   4. Auto-revalidate via `elot_lint' (+ `elot_omn_validate'
;;;      when ROBOT is configured).  On failure roll the buffer
;;;      back to its pre-rewrite snapshot and save, returning the
;;;      diagnostics.
;;;   5. Otherwise save buffer, format the success line per the
;;;      plan's Step 12.4 spec.

(declare-function elot-rename-resource "elot-id-rename"
                  (source target &rest keys))
(declare-function elot-id-rename--split-curie "elot-id-rename" (curie))
(declare-function elot-id-rename--read-prefix-table "elot-id-rename" ())
(declare-function elot-id-rename--db-candidates "elot-id-rename" (prefix))

(defun elot-gptel--rename-format-candidates (prefix candidates)
  "Render PREFIX + CANDIDATES as a structured ERROR: line for the LLM.
Matches the shape pinned by Step 12.4's `Notes for later wiring
up the gptel tool': single-line ASCII, `<IRI1> | <IRI2>'
separator, `(none)' when the candidate list is empty, retry
hint as a trailing clause."
  (format
   "undeclared prefix `%s:'; candidates: %s; %s"
   prefix
   (if candidates
       (mapconcat (lambda (i) (format "<%s>" i)) candidates " | ")
     "(none)")
   (if candidates
       "retry with target_iri=<IRI>"
     "supply a fresh target_iri=<IRI>")))

(defun elot-gptel-tool-rename-resource
    (file source target &optional ontology target-iri new-label op dry-run)
  "Implementation of the `elot_rename_resource' tool.

Rewrites every occurrence of SOURCE (a CURIE declared on a
resource heading in FILE) to TARGET, both as CURIE tokens and
as full-IRI references that expand to SOURCE's IRI under the
file's prefix table.  Heading parentheticals, description-list
keys and values (including OMN axiom bodies), and OMN/SPARQL
src-block bodies are all rewritten; prefix-table rows and
running prose are left alone.

ONTOLOGY is reserved for the multi-ontology disambiguation case
described in the plan; the v1 implementation operates on the
whole buffer and accepts ONTOLOGY for forward compatibility
without acting on it.

TARGET_IRI, when supplied, is the IRI expansion for TARGET's
prefix.  Required only when that prefix is not declared in
FILE's prefix table.  When supplied, the rename adds a new row
to the prefix table inside the same atomic group as the CURIE
rewrite; ignored when the prefix is already declared.

NEW_LABEL, when supplied, also rewrites the rdfs:label carried
in the resource-declaring heading's title (the text before the
`(CURIE)' parenthetical), inside the same atomic-change-group
as the CURIE rewrite.  Empty or nil leaves the label untouched.
Trailing statistics cookies and tags on the heading line are
preserved verbatim.

Gated by `elot-gptel-allow-side-effects': refuses when the
flag is nil.  On a successful rewrite the file is saved, lint
and OMN-validate are re-run, and on revalidation failure the
buffer is restored to its pre-rewrite contents.  Returns:

  OK: renamed SOURCE -> TARGET (N CURIE, M IRI; K prose ...)
      [(declared prefix prefix2: -> <IRI>) when applicable]
  == LINT ==
  <lint report>
  [== OMN PARSE ==
   <omn-validate report>]

or an `ERROR:' line on refusal / failure."
  (ignore ontology)
  (elot-gptel-with-mutation (file dry-run)
    (unless (and (stringp source) (not (string-empty-p source)))
      (user-error "elot-gptel: source must be a non-empty CURIE string"))
    (unless (and (stringp target) (not (string-empty-p target)))
      (user-error "elot-gptel: target must be a non-empty CURIE string"))
    (require 'elot-id-rename)
    (setq op
          (cond
           ((null op) 'rename)
           ((symbolp op) op)
           ((and (stringp op)
                 (member op '("rename" "merge")))
            (intern op))
           (t (user-error
               "elot-gptel: op must be `rename' or `merge' (got %S)"
               op))))
    (unless (eq op 'merge)
      (let* ((tgt-parts (elot-id-rename--split-curie target))
             (tgt-prefix (car tgt-parts))
             (prefix-table (elot-id-rename--read-prefix-table))
             (declared-p (assoc tgt-prefix prefix-table)))
        (when (and (not declared-p)
                   (or (null target-iri)
                       (and (stringp target-iri)
                            (string-empty-p target-iri))))
          (let ((candidates
                 (ignore-errors
                   (elot-id-rename--db-candidates tgt-prefix))))
            (user-error
             "%s"
             (elot-gptel--rename-format-candidates
              tgt-prefix candidates))))))
    (let* ((tiri (and (stringp target-iri)
                      (not (string-empty-p target-iri))
                      target-iri))
           (nlab (and (stringp new-label)
                      (not (string-empty-p new-label))
                      new-label))
           (keys (list :op op))
           rename-result)
      (when tiri
        (setq keys (append (list :target-iri tiri) keys)))
      (when nlab
        (setq keys (append (list :new-label nlab) keys)))
      (setq rename-result (apply #'elot-rename-resource source target keys))
      (let* ((c (plist-get rename-result :curie-count))
             (i (plist-get rename-result :iri-count))
             (k (plist-get rename-result :prose-skipped))
             (decl (plist-get rename-result :declared-prefix))
             (verb (if (eq op 'merge) "merged" "renamed"))
             (arrow (if (eq op 'merge) "into" "->"))
             (head
              (format
               "OK: %s %s %s %s (%d CURIE, %d IRI; %d prose mention%s skipped%s)%s"
               verb source arrow target c i k
               (if (= k 1) "" "s")
               (if (> k 0) " -- audit recommended" "")
               (if decl
                   (format " (declared prefix %s: -> <%s>)"
                           (car decl) (cdr decl))
                 ""))))
        head))))

;;; ---------------------------------------------------------------------------
;;; elot_move_resource tool (Milestone 12 Step 12.1)
;;; ---------------------------------------------------------------------------
;;;
;;; LLM-facing wrapper around `elot-move-resource' (see
;;; `elot-id-move.el').  Side-effecting: mutates the user's .org
;;; file in place, gated by `elot-gptel-allow-side-effects'.
;;;
;;; Pipeline (mirrors the M12.4 rename wrapper):
;;;   1. Gate check (side-effects, arg shape).
;;;   2. Open the buffer for FILE (visiting if necessary), put it in
;;;      `org-mode' when it is not already.
;;;   3. Snapshot the buffer text + modified flag for rollback.
;;;   4. Call `elot-move-resource' (programmatic path); a `user-error'
;;;      raised by the move command becomes the structured `ERROR:'
;;;      line the LLM sees.
;;;   5. Save the buffer.
;;;   6. Re-run `elot-gptel--rename-revalidate' (lint + OMN parse
;;;      when ROBOT is configured).  On revalidation failure, restore
;;;      the pre-move bytes, save again, and return the diagnostics.
;;;   7. On success format the result line per the plan's Step 12.1
;;;      "Output" spec.

(declare-function elot-move-resource "elot-id-move"
                  (source target &optional as))

;;; ---------------------------------------------------------------------------
;;; elot_insert_* tools (Milestone 9 Step 9.3)
;;; ---------------------------------------------------------------------------
;;;
;;; Thin LLM-facing wrappers over the M10.6 interactive insert
;;; commands in `elot-id-insert.el'.  Side-effecting: each call
;;; mutates the user's .org file in place, gated by
;;; `elot-gptel-allow-side-effects'.
;;;
;;; The underlying Elisp commands already do scheme + prefix
;;; resolution, batch-aware identifier minting via
;;; `elot-id-mint-batch', blanked description-list inheritance,
;;; and kind-by-section refusal under Datatypes / Individuals.
;;; These wrappers are pure plumbing: anchor resolution + atomic
;;; apply + auto-revalidate + rollback.  The result envelope
;;; mirrors the M12.1 / M12.4 wrappers (OK: one-liner + lint/OMN
;;; blocks).  Minted CURIEs are listed as a bulleted block in the
;;; success body.

(declare-function elot-insert-sibling-resource "elot-id-insert"
                  (&optional n labels))
(declare-function elot-insert-child-resource "elot-id-insert"
                  (&optional n labels))
(declare-function elot-insert-labels-tree "elot-id-insert"
                  (tree &optional as))
(declare-function elot-id-insert--goto-heading-for-curie
                  "elot-id-insert" (curie))
(declare-function elot-id-insert--do-insert "elot-id-insert"
                  (child-p n &optional labels))
(declare-function elot-id-heading-curie-regexp "elot-id" (curie))

(defun elot-gptel--insert-curie-shape-p (s)
  "Return non-nil when S looks like a CURIE (`prefix:local')."
  (and (stringp s)
       (string-match-p
        "\\`[A-Za-z_][A-Za-z0-9_.-]*:[A-Za-z_][A-Za-z0-9_.-]*\\'"
        s)))

(defun elot-gptel--insert-find-heading-by-label (label)
  "Return position of an Org heading identified by LABEL.

Resolution strategy (first match wins; ambiguity at any step is
an error):

1. /CURIE-parenthetical/ -- a heading line of the shape
   `STARS SPACE LABEL SPACE (curie)'.  This is the canonical
   form for resource headings and stays the preferred path.

2. /Property/ -- a heading whose `:ID:' or `:CUSTOM_ID:'
   property value equals LABEL.  Used to address ELOT section
   roots by their property id (e.g. `my-ont-object-property-
   hierarchy'), which 9.3.F6 added so the wrappers can seed
   empty resource sections.

3. /Plain title/ -- a heading whose title text (sans tags,
   priority cookies, and TODO keywords) equals LABEL exactly.
   Catches section-root headings like `Object properties' that
   carry no CURIE parenthetical.

Returns the buffer position at the start of the matching
heading, or nil when no heading matches.  Signals
`user-error' when the chosen step matches more than one
heading."
  (or
   ;; Step 1: CURIE-parenthetical match (legacy / preferred).
   (let (matches)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward
               (concat "^\\*+ "
                       (regexp-quote label)
                       "[ \t]+(")
               nil t)
         (push (line-beginning-position) matches)))
     (cond
      ((null matches) nil)
      ((= 1 (length matches)) (car matches))
      (t (user-error
          "elot-gptel: anchor label %S is ambiguous (%d headings)"
          label (length matches)))))
   ;; Steps 2 + 3: property or plain-title match (9.3.F6 -- enables
   ;; section-root anchors).  One outline pass collects candidates
   ;; from both sources so ambiguity is reported across them.
   (let (matches)
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^\\*+ " nil t)
         (let* ((title  (org-get-heading t t t t))
                (id     (org-entry-get nil "ID"))
                (cusid  (org-entry-get nil "CUSTOM_ID")))
           (when (or (and id    (string= label (string-trim id)))
                     (and cusid (string= label (string-trim cusid)))
                     (and title (string= label (string-trim title))))
             (push (line-beginning-position) matches)))))
     (setq matches (delete-dups matches))
     (cond
      ((null matches) nil)
      ((= 1 (length matches)) (car matches))
      (t (user-error
          "elot-gptel: anchor label %S is ambiguous (%d headings)"
          label (length matches)))))))

(defun elot-gptel--insert-goto-anchor (anchor)
  "Move point to the resource heading named by ANCHOR in the current buffer.
ANCHOR is a CURIE (preferred, unambiguous), a heading title,
or a section heading's `:ID:' / `:CUSTOM_ID:' property value.
Signals `user-error' when no matching heading is found, or when
a non-CURIE anchor matches more than one heading."
  (unless (and (stringp anchor) (not (string-empty-p anchor)))
    (user-error "elot-gptel: anchor must be a non-empty string"))
  (cond
   ((elot-gptel--insert-curie-shape-p anchor)
    (condition-case _err
        (progn
          (elot-id-insert--goto-heading-for-curie anchor)
          (point))
      (error
       (user-error "elot-gptel: no heading declares CURIE %s" anchor))))
   (t
    (let ((pos (elot-gptel--insert-find-heading-by-label anchor)))
      (unless pos
        (user-error
         "elot-gptel: no heading with label %S found" anchor))
      (goto-char pos)
      pos))))

(defun elot-gptel--insert-coerce-labels (labels)
  "Coerce LABELS to a list.
Accepts a list or a vector (gptel decodes JSON arrays as vectors
in some configurations).  Returns the input as a list; non-sequence
inputs are returned unchanged so the validator can flag them."
  (cond
   ((listp labels) labels)
   ((vectorp labels) (append labels nil))
   (t labels)))

(defun elot-gptel--insert-label-curie-parenthetical-p (label)
  "Return non-nil when LABEL ends with a CURIE parenthetical.
Example: \"identifier (dcterms:identifier)\".  The insert-resource
GPT tools mint fresh identifiers, so accepting this shape is a footgun:
the heading renderer would produce `identifier (dcterms:identifier)
(rdl:minted)', while ELOT's heading parser treats the first
parenthetical as the declaration CURIE."
  (and (stringp label)
       (string-match-p
        "[ \\t]*(\\(?:[A-Za-z_][A-Za-z0-9_.-]*\\):[^() \\t]+)[ \\t]*\\'"
        (string-trim label))))

(defun elot-gptel--insert-validate-labels (labels)
  "Signal `user-error' when LABELS is not a non-empty list of strings.
LABELS must already have been passed through
`elot-gptel--insert-coerce-labels'.  These tools mint fresh
identifiers, so LABELS are plain rdfs:label strings, not full ELOT
heading titles.  Reject labels that smuggle a CURIE parenthetical such
as \"identifier (dcterms:identifier)\": otherwise the underlying
inserter would mint a fresh CURIE but the ELOT heading parser would
read the first parenthetical as the declared CURIE, producing a file
whose actual declaration disagrees with the tool's \"Minted CURIEs\"
report."
  (unless (and (listp labels) labels)
    (user-error "elot-gptel: labels must be a non-empty array"))
  (dolist (l labels)
    (unless (and (stringp l) (not (string-empty-p (string-trim l))))
      (user-error
       "elot-gptel: every label must be a non-empty string (got %S)" l))
    (when (elot-gptel--insert-label-curie-parenthetical-p l)
      (user-error
       "elot-gptel: labels are plain rdfs:label strings, not `Label (curie)' headings; got %S.  To reuse an existing CURIE, declare/borrow that resource explicitly instead of using an insert-* minting tool."
       l))))

(defun elot-gptel--insert-walk-tree-node (node path)
  "Translate JSON tree NODE to the shape `elot-insert-labels-tree' expects.
PATH is a list of integers describing the position of NODE within
the request (root=0), used for error messages.  Recursively
walks arrays."
  (cond
   ((stringp node)
    (when (string-empty-p (string-trim node))
      (user-error "elot-gptel: empty label at tree path %S" (nreverse path)))
    (when (elot-gptel--insert-label-curie-parenthetical-p node)
      (user-error
       "elot-gptel: tree labels are plain rdfs:label strings, not `Label (curie)' headings; got %S at path %S"
       node (nreverse path)))
    node)
   ((and (listp node) node (stringp (car node)))
    (when (string-empty-p (string-trim (car node)))
      (user-error "elot-gptel: empty label at tree path %S" (nreverse path)))
    (when (elot-gptel--insert-label-curie-parenthetical-p (car node))
      (user-error
       "elot-gptel: tree labels are plain rdfs:label strings, not `Label (curie)' headings; got %S at path %S"
       (car node) (nreverse path)))
    (cons (car node)
          (let ((i 0))
            (mapcar (lambda (child)
                      (prog1
                          (elot-gptel--insert-walk-tree-node
                           child (cons (cl-incf i) path))
                        nil))
                    (cdr node)))))
   ((and (vectorp node) (> (length node) 0) (stringp (aref node 0)))
    ;; JSON arrays decode as vectors in some configurations.
    (elot-gptel--insert-walk-tree-node
     (append node nil) path))
   (t
    (user-error
     "elot-gptel: malformed tree node at path %S (expected string or [label, ...])"
     (nreverse path)))))

(defun elot-gptel--insert-walk-tree (tree)
  "Translate a JSON forest TREE to the Elisp shape elisp-side expects.
TREE is a list (or vector) of nodes; each node is a string leaf
or a [LABEL, CHILD, ...] array.  Signals `user-error' on malformed
shape."
  (let ((tree (if (vectorp tree) (append tree nil) tree)))
    (unless (and (listp tree) tree)
      (user-error
       "elot-gptel: tree must be a non-empty array of nodes"))
    (let ((i 0))
      (mapcar (lambda (node)
                (elot-gptel--insert-walk-tree-node
                 node (list (cl-incf i))))
              tree))))

(defun elot-gptel--insert-format-curies (pairs)
  "Render PAIRS as a bulleted `Minted CURIEs:' block string.

PAIRS is a list of (CURIE . LABEL) cons cells; the LABEL part may
be nil when no label is known for an entry, in which case only
the CURIE is rendered.  For backward compatibility PAIRS may also
be a list of bare CURIE strings -- those render without a label
suffix.  Returns the empty string when PAIRS is nil.

Format (F13): `  - ex:foo -- \"Foo\"' when both are present;
`  - ex:foo' when LABEL is nil or empty."
  (if (null pairs)
      ""
    (concat
     "Minted CURIEs:\n"
     (mapconcat
      (lambda (entry)
        (let* ((curie (if (consp entry) (car entry) entry))
               (label (and (consp entry) (cdr entry))))
          (if (and (stringp label) (not (string-empty-p label)))
              (format "  - %s -- %S" curie label)
            (concat "  - " curie))))
      pairs "\n"))))

(defun elot-gptel--insert-do (file anchor child-p labels)
  "Common driver for sibling / child insert.  Returns the formatted result.
FILE / ANCHOR / CHILD-P / LABELS as in the public tools.  Performs
the gate check, opens the file, snapshots for rollback, positions
point at ANCHOR, calls `elot-id-insert--do-insert', re-validates,
and on success returns an OK envelope; on validation failure
restores the pre-insert bytes."
  (elot-gptel-with-mutation (file nil)
    (setq labels (elot-gptel--insert-coerce-labels labels))
    (elot-gptel--insert-validate-labels labels)
    (require 'elot-id-insert)
    (let (curies)
      (save-excursion
        (elot-gptel--insert-goto-anchor anchor)
        (atomic-change-group
          (setq curies
                (if child-p
                    (elot-id-insert--do-insert
                     t (length labels) labels)
                  (elot-id-insert--do-insert
                   nil (length labels) labels)))))
      (let* ((n (length curies))
             (head
              (format
               "OK: inserted %d %s under %s"
               n
               (if child-p
                   (if (= n 1) "child" "children")
                 (if (= n 1) "sibling" "siblings"))
               anchor))
             ;; F13: pair CURIEs with their input labels so the
             ;; response is unambiguous for follow-up renames.
             ;; `do-insert' mints in label order, so a positional
             ;; zip is the right pairing.
             (pairs (cl-mapcar #'cons curies labels))
             (block (elot-gptel--insert-format-curies pairs)))
        (if (string-empty-p block)
            head
          (concat head "\n\n" block))))))

(defun elot-gptel-tool-insert-sibling-resource (file anchor labels)
  "Implementation of the `elot_insert_sibling_resource' tool.

Inserts (length LABELS) new resource headings as siblings of the
heading named by ANCHOR in FILE.  ANCHOR is a CURIE (preferred)
or an unambiguous label.  Each LABEL becomes the `rdfs:label' of
one new heading; its identifier is minted under the ontology
heading's `:ELOT-id-scheme:' via `elot-id-mint-batch'
(collision-aware against existing declarations).

Gated by `elot-gptel-allow-side-effects'.  The file is saved and
re-linted (plus OMN-parsed when ROBOT is configured); a
revalidation failure restores the pre-insert bytes.  On success
returns

  OK: inserted N sibling(s) under ANCHOR

followed by the list of minted CURIEs and the lint (and
OMN-parse) reports."
  (elot-gptel--insert-do file anchor nil labels))

(defun elot-gptel-tool-insert-child-resource (file anchor labels)
  "Implementation of the `elot_insert_child_resource' tool.

Inserts (length LABELS) new resource headings as first children
of the heading named by ANCHOR in FILE.  ANCHOR is a CURIE
(preferred) or an unambiguous label.  Inherits the level-2
section refusal contract from the underlying Elisp command --
ANCHOR under Datatypes / Individuals (at level 3+) returns an
`ERROR:' line citing the user-error.  Anchor directly on the
level-2 section heading is always allowed (seeds the first
resource).

Same write-back contract as `elot_insert_sibling_resource'."
  (elot-gptel--insert-do file anchor t labels))

(defun elot-gptel-tool-insert-resource-tree (file anchor tree &optional as)
  "Implementation of the `elot_insert_resource_tree' tool.

Inserts a forest of resource headings under (or after) the
heading named by ANCHOR in FILE.  TREE is a JSON array whose
elements are either strings (leaves) or arrays of the form
[LABEL, CHILD, ...] (internal nodes, child shape is the same
recursively).

AS is `\"sibling\"' (default) or `\"child\"' -- how the
top-level nodes are placed relative to ANCHOR.  Nested levels
are always inserted as children of their parent.

Gated by `elot-gptel-allow-side-effects'.  Returns

  OK: inserted N heading(s) under ANCHOR (as AS)

followed by the list of minted CURIEs in level order
(top-level siblings first, then each branch's children level
by level -- a direct consequence of `elot-insert-labels-tree'
batch-minting one level at a time) and the lint (and
OMN-parse) reports.

CURIE collection: identifiers are accumulated by wrapping
`elot-id-insert--do-insert' with `cl-letf' for the duration of
the call -- the primitive returns its inserted slice on each
call, so concatenating those slices in invocation order gives
the level-ordered sequence."
  (elot-gptel-with-mutation (file nil)
    (let ((as-sym (cond
                   ((or (null as) (and (stringp as) (string-empty-p as)))
                    'sibling)
                   ((stringp as) (intern as))
                   ((symbolp as) as)
                   (t (user-error
                       "elot-gptel: as must be \"child\" or \"sibling\": %S"
                       as)))))
      (unless (memq as-sym '(child sibling))
        (user-error
         "elot-gptel: as must be \"child\" or \"sibling\": %S" as))
      (require 'elot-id-insert)
      (let ((lisp-tree (elot-gptel--insert-walk-tree tree)))
        (let ((orig-fn (symbol-function 'elot-id-insert--do-insert))
              (collected nil))
          ;; F13: pair each minted CURIE with the label that
          ;; produced it.  `do-insert' mints in label order, so
          ;; positional zip of `cs' with `labels' is correct.
          ;; The walker preserves level-order across calls, so
          ;; the concatenated pair list also matches level order.
          (cl-letf (((symbol-function 'elot-id-insert--do-insert)
                     (lambda (child-p n &optional labels)
                       (let* ((cs (funcall orig-fn child-p n labels))
                              (these (cl-mapcar #'cons cs labels)))
                         (setq collected (append collected these))
                         cs))))
            (save-excursion
              (elot-gptel--insert-goto-anchor anchor)
              (atomic-change-group
                (elot-insert-labels-tree lisp-tree as-sym))))
          (let* ((n (length collected))
                 (head (format
                        "OK: inserted %d heading%s under %s (as %s)"
                        n (if (= n 1) "" "s")
                        anchor (symbol-name as-sym)))
                 ;; F13: `collected' is already a list of
                 ;; (CURIE . LABEL) cons cells.
                 (block (elot-gptel--insert-format-curies collected)))
            (if (string-empty-p block)
                head
              (concat head "\n\n" block))))))))

(defun elot-gptel-tool-move-resource
    (file source target &optional as)
  "Implementation of the `elot_move_resource' tool.

Relocates the resource heading declared by SOURCE -- with its
whole subtree -- to a new parent (or section root) within the
same ontology, within FILE.  The CURIE itself is not rewritten;
this is a pure subtree relocation.  Use `elot_rename_resource'
when the CURIE needs to change.

Arguments:

  FILE    -- project-relative path to an ELOT .org file.
  SOURCE  -- CURIE of the resource heading to move
             (e.g. `\"ex:dog\"').
  TARGET  -- CURIE of the new parent / previous-sibling
             (e.g. `\"ex:mammal\"'), or the literal string
             `\"top\"' to place SOURCE directly under the
             section root of its kind.
  AS      -- `\"child\"' (default) or `\"sibling\"'.

Refuses (returning an `ERROR:' line) when:

  - `elot-gptel-allow-side-effects' is nil;
  - SOURCE is not a declared resource heading in FILE, or is
    declared in more than one ontology in the buffer;
  - TARGET is not a declared resource heading (and not the
    literal `\"top\"');
  - SOURCE's section kind disagrees with TARGET's section kind;
  - the move is a no-op (TARGET already SOURCE's parent) or
    would place SOURCE inside its own subtree;
  - under Datatypes / Individuals, AS=`\"child\"' was requested
    with a level-3+ TARGET (no inherent sub-relationship).

On success the file is saved, lint and OMN-validate are
re-run, and the response has the shape:

  OK: moved SOURCE from FROM-PARENT to TO-PARENT (as AS)
  == LINT ==
  <lint report>
  [== OMN PARSE ==
   <omn-validate report>]

On revalidation failure the pre-move bytes are restored and an
`ERROR: revalidation failed -- changes rolled back' header
precedes the diagnostic body."
  (elot-gptel-with-mutation (file nil)
    (unless (and (stringp source) (not (string-empty-p source)))
      (user-error "elot-gptel: source must be a non-empty CURIE string"))
    (unless (and (stringp target) (not (string-empty-p target)))
      (user-error
       "elot-gptel: target must be a non-empty CURIE string or \"top\""))
    (let* ((as-sym (cond
                    ((null as) 'child)
                    ((and (stringp as) (string-empty-p as)) 'child)
                    ((stringp as) (intern as))
                    ((symbolp as) as)
                    (t (user-error
                        "elot-gptel: as must be \"child\" or \"sibling\": %S"
                        as)))))
      (unless (memq as-sym '(child sibling))
        (user-error
         "elot-gptel: as must be \"child\" or \"sibling\": %S"
         as))
      (require 'elot-id-move)
      (let* ((move-result (elot-move-resource source target as-sym))
             (from-parent (or (plist-get move-result :from-parent)
                              "(top-level)"))
             (to-parent   (or (plist-get move-result :to-parent)
                              "(top-level)")))
        (format
         "OK: moved %s from %s to %s (as %s)"
         source from-parent to-parent
         (symbol-name as-sym))))))

;;; ---------------------------------------------------------------------------
;;; M9.9 -- reference-scan helper for resource deletion
;;; ---------------------------------------------------------------------------
;;;
;;; Shared scanner that answers "what would dangle if I removed
;;; SUBJECT?".  Foundation for `elot_delete_resource' and
;;; `elot_delete_resource_tree' (Step 9.9), and explicitly designed
;;; to be reusable by a future `elot_rename_resource --safe' mode,
;;; which has the same dependency-picture question to answer before
;;; rewriting external references.
;;;
;;; Reads the buffer-local `elot-headline-hierarchy' for the
;;; subject's node + direct children -- so freshly inserted / moved
;;; / renamed headings are picked up via the F8 staleness markers
;;; (`elot-headline-hierarchy-ensure-fresh' is called up front).
;;; Then makes a single buffer pass classifying each line via
;;; `elot-id-rename--line-class' (the established desc-list /
;;; src-block / prose classifier) and records axiom-row hits on
;;; *other* subjects, OMN/SPARQL src-block hits, and a count of
;;; prose mentions.  Pure read -- never mutates the buffer.

(declare-function elot-id-rename--line-class "elot-id-rename" ())
(declare-function elot-headline-hierarchy-ensure-fresh "elot-tangle" ())
(defvar elot-omn-all-keywords)

(defun elot-gptel--delete-curie-mentioned-p (curie value)
  "Return non-nil when VALUE mentions CURIE as a whole token.
Boundary-aware: mirrors the predicate used by
`elot/axiom-value-curies' (Manchester signature check) and
`elot--oops-curie-mentioned-p' (OOPS! P24).  Boundary characters
are anything outside the CURIE-name set (letters, digits, `_',
`-', `.', `/').  Pure helper used by
`elot-gptel--delete-reference-scan'."
  (and (stringp value) (stringp curie)
       (let ((case-fold-search nil))
         ;; Boundary char class mirrors `elot-id-rename--curie-char-re'
         ;; -- letters, digits, `_', `-', `:'.  `.' is intentionally
         ;; EXCLUDED so a sentence-ending period (prose) or trailing
         ;; punctuation terminates the CURIE token, matching the
         ;; rename pipeline's notion of a word boundary.
         (string-match-p
          (concat "\\(?:^\\|[^-_:[:alnum:]]\\)"
                  (regexp-quote curie)
                  "\\(?:[^-_:[:alnum:]]\\|$\\)")
          value))))

(defun elot-gptel--delete--node-curie (node)
  "First whitespace-separated token of NODE's `:uri', or nil.
NODE is an `elot-headline-hierarchy' node plist.  Ontology
headings carry composite `:uri' strings like `ex:my-ont
<http://.../0.0>'; the first token is the CURIE proper."
  (let ((uri (and node (plist-get node :uri))))
    (and uri (stringp uri) (car (split-string uri "[ \t]+" t)))))

(defun elot-gptel--delete--node-for-curie (curie)
  "Return the `elot-headline-hierarchy' node whose CURIE is CURIE.
Walks the buffer-local hierarchy depth-first.  Returns nil when
CURIE is not declared in the buffer.  Caller is responsible for
calling `elot-headline-hierarchy-ensure-fresh' first when the
buffer may have been mutated since the cache was built."
  (let (found)
    (cl-labels ((walk (n)
                  (unless found
                    (when (string= (elot-gptel--delete--node-curie n) curie)
                      (setq found n))
                    (unless found
                      (dolist (c (plist-get n :children))
                        (walk c))))))
      (let ((root (bound-and-true-p elot-headline-hierarchy)))
        (when root (walk root))))
    found))

(defun elot-gptel--delete--collect-children-curies (node)
  "List of CURIE strings, the direct resource children of NODE.
Skips children tagged `:nodeclare:' (which carry no
declaration).  Caller passes the parent NODE plist."
  (let (acc)
    (dolist (c (plist-get node :children))
      (unless (member "nodeclare" (plist-get c :tags))
        (let ((uri (elot-gptel--delete--node-curie c)))
          (when uri (push uri acc)))))
    (nreverse acc)))

(defconst elot-gptel--delete-heading-curie-re
  (concat "^\\*+ .*?("
          "\\([A-Za-z_][A-Za-z0-9_-]*"
          ":[A-Za-z_][A-Za-z0-9_.-]*\\)"
          ")")
  "Regex matching `* Label (CURIE)' on a heading line, capturing CURIE.
Used by `elot-gptel--delete-reference-scan' to track the
enclosing resource heading while walking the buffer line by
line.")

(defconst elot-gptel--delete-desc-row-re
  "\\`[ \t]+-[ \t]+\\([^[:space:]:]+\\(?::[^[:space:]:]+\\)?\\)[ \t]*::[ \t]*\\(.*\\)\\'"
  "Regex matching a description-list row `- KEY :: VALUE'.
Group 1 captures KEY (bare keyword or CURIE), group 2 the
trailing VALUE.  Used by `elot-gptel--delete-reference-scan'.")

(defun elot-gptel--delete-reference-scan (subject)
  "Scan the current buffer for references to SUBJECT (a CURIE string).
Returns a plist with the following keys.  All four lists are in
document order; axiom-row / src-row entries carry 1-based line
numbers so the caller can format `FILE:LINE  ...' diagnostics:

  :node         The `elot-headline-hierarchy' node plist for
                SUBJECT, or nil when SUBJECT is not declared in
                the buffer.

  :children     List of CURIE strings -- the direct resource
                children of SUBJECT (heading nesting carries
                SubClassOf / SubPropertyOf).  Drawn straight
                from the hierarchy; `:nodeclare:'-tagged
                children are skipped.

  :axiom-rows   List of plists (:curie SUBJ :keyword KW
                :fragment FRAG :line LINE), one per
                description-list row on a subject OTHER THAN
                SUBJECT whose KEY is an OMN axiom keyword
                (member of `elot-omn-all-keywords') and whose
                VALUE mentions SUBJECT as a whole token.
                Axiom-annotation sub-rows (indented under a
                frame row) are attributed to the enclosing
                resource heading.

  :src-rows     List of plists (:line LINE :language LANG), one
                per line inside an OMN / SPARQL `src-block' that
                mentions SUBJECT as a whole token.  Other
                src-block languages are silently skipped.

  :annotation-rows
                List of plists (:curie SUBJ :keyword KW
                :fragment FRAG :line LINE), one per
                description-list row on a subject OTHER THAN
                SUBJECT whose KEY is NOT an OMN axiom keyword
                (so it is an annotation-property row -- e.g.
                `rdfs:seeAlso', `rdfs:comment', `skos:related')
                and whose VALUE mentions SUBJECT as a whole
                token.  Reported as an advisory NOTE; never a
                refusal -- OWL permits annotation-property
                values to point at non-declared IRIs, so a
                dangling annotation after delete is
                syntactically fine but worth a human audit.

  :prose-count  Count of prose lines (running text -- neither in
                a description list nor in a recognised src-block
                nor in the prefix-table) that mention SUBJECT.
                Prose mentions are reported, never rewritten;
                matches the established \"audit recommended\"
                shape used by `elot_rename_resource'.

`elot-headline-hierarchy-ensure-fresh' is called up front (F8
contract), so freshly inserted / moved / renamed headings are
visible to the scan.  Line classification delegates to
`elot-id-rename--line-class' for parity with the rename
wrapper's notion of \"rewritable line\"; a minimal regex
fallback applies when `elot-id-rename' is not yet loaded.

Pure read -- never mutates the buffer.  Public-by-convention
for reuse by a future `elot_rename_resource --safe' mode."
  (when (fboundp 'elot-headline-hierarchy-ensure-fresh)
    (ignore-errors (elot-headline-hierarchy-ensure-fresh)))
  (require 'elot-id-rename nil 'noerror)
  (let* ((node (and subject
                    (elot-gptel--delete--node-for-curie subject)))
         (heading-children (and node
                                (elot-gptel--delete--collect-children-curies
                                 node)))
         (axiom-children nil)
         (axiom-rows nil)
         (annotation-rows nil)
         (src-rows nil)
         (prose-count 0)
         (current-curie nil))
    (when (and subject (not (string-empty-p subject)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (line (buffer-substring-no-properties bol eol))
                 (class (if (fboundp 'elot-id-rename--line-class)
                            (elot-id-rename--line-class)
                          (cond
                           ((string-match-p "\\`\\*+ " line) 'heading)
                           ((string-match-p "\\`[ \t]+-[ \t]" line)
                            'desc-list-nested)
                           ((string-match-p "\\`[ \t]*-[ \t]" line)
                            'desc-list)
                           (t 'prose)))))
            (cond
             ((eq class 'heading)
              ;; Update the enclosing-resource tracker.  Headings
              ;; without a CURIE parenthetical (section roots,
              ;; ontology declarations) clear the tracker so a
              ;; following row is not misattributed.
              (setq current-curie
                    (and (string-match
                          elot-gptel--delete-heading-curie-re line)
                         (match-string 1 line))))
             ((memq class '(desc-list desc-list-nested))
              (when (and current-curie
                         (not (string= current-curie subject))
                         (string-match
                          elot-gptel--delete-desc-row-re line))
                (let ((key (match-string 1 line))
                      (value (match-string 2 line)))
                  (cond
                   ((and (member
                          key
                          (bound-and-true-p elot-omn-all-keywords))
                         (elot-gptel--delete-curie-mentioned-p
                          subject value))
                    ;; Axiom-derived child: an OTHER subject whose
                    ;; `SubClassOf' / `SubPropertyOf' row names SUBJECT
                    ;; -- this is the "Puppy is a level-3 sibling of
                    ;; Dog but declares SubClassOf :: ex:dog" case.
                    ;; Counted as a child for cascade purposes; not
                    ;; reported as a dangling axiom-row reference.
                    (if (member key '("SubClassOf" "SubPropertyOf"))
                        (cl-pushnew current-curie axiom-children
                                    :test #'string=)
                      (push (list :curie current-curie
                                  :keyword key
                                  :fragment (string-trim value)
                                  :line (line-number-at-pos bol))
                            axiom-rows)))
                   ((and (not (member
                               key
                               (bound-and-true-p elot-omn-all-keywords)))
                         (elot-gptel--delete-curie-mentioned-p
                          subject value))
                    ;; Annotation-property row on another subject
                    ;; whose value mentions SUBJECT (e.g.
                    ;; `rdfs:seeAlso :: ex:foo').  Surfaced as an
                    ;; advisory; never a refusal.
                    (push (list :curie current-curie
                                :keyword key
                                :fragment (string-trim value)
                                :line (line-number-at-pos bol))
                          annotation-rows))))))
             ((eq class 'src-rewrite)
              (when (elot-gptel--delete-curie-mentioned-p subject line)
                (let* ((ctx (ignore-errors (org-element-context)))
                       (lang (or (and ctx
                                      (org-element-property
                                       :language ctx))
                                 "")))
                  (push (list :line (line-number-at-pos bol)
                              :language lang)
                        src-rows))))
             ((eq class 'prose)
              (when (elot-gptel--delete-curie-mentioned-p subject line)
                (cl-incf prose-count)))
             ;; `prefix-row', `src-other' -- silently skipped.
             (t nil)))
          (forward-line 1))))
    (list :node node
          :children (delete-dups
                     (append heading-children (nreverse axiom-children)))
          :axiom-rows (nreverse axiom-rows)
          :annotation-rows (nreverse annotation-rows)
          :src-rows (nreverse src-rows)
          :prose-count prose-count)))


;;; ---------------------------------------------------------------------------
;;; M9.9.B.1 -- `elot-gptel--delete-apply' core primitive
;;; ---------------------------------------------------------------------------
;;;
;;; Pure buffer mutator over a resolved resource heading.  No tool spec
;;; yet (that's B.2), no reference scan (caller's job, via
;;; `elot-gptel--delete-reference-scan'), no revalidation (caller wraps
;;; us in the standard `elot-gptel--rename-revalidate' envelope).  This
;;; layer is intentionally thin: it picks the right outline-surgery
;;; primitive from `elot-id-move' based on CASCADE and runs it inside
;;; an `atomic-change-group' so a signal mid-surgery rolls back.

(declare-function elot-id-remove-heading-promote-children
                  "elot-id-move" (marker))
(declare-function elot-id-delete-heading-subtree
                  "elot-id-move" (marker))
(declare-function elot-id-rename--heading-marker-for-curie
                  "elot-id-rename" (curie))

(defun elot-gptel--delete-apply (subject cascade)
  "Delete SUBJECT's heading from the current buffer per CASCADE.

SUBJECT is the CURIE of a resource heading already known to be
declared in the buffer (caller's responsibility -- typically via
`elot-gptel--delete-reference-scan' returning a non-nil :node).
CASCADE is the symbol `reparent' or `delete':

  `reparent' -- delete SUBJECT's heading + its description-list
    payload; promote each direct child subtree one outline
    level (so they reattach to SUBJECT's former outline parent).
    Delegates to `elot-id-remove-heading-promote-children'.
    Safe when SUBJECT has zero children (the helper degenerates
    to a plain heading delete in that case).

  `delete' -- delete SUBJECT's heading together with its entire
    subtree (every descendant heading + payload, recursively).
    Delegates to `elot-id-delete-heading-subtree'.  Used by the
    `cascade=delete' branch of `elot_delete_resource' and by the
    bulk `elot_delete_resource_tree' deepest-first sweep.

Both helpers must run inside an `atomic-change-group'; this
function provides one so callers may invoke it directly.  Pure
byte-level surgery -- no reference rewrite, no lint, no save.
The caller wraps this in the standard write-back contract
(snapshot, save, revalidate, rollback) before any tool envelope
is built.

Returns a plist:
  (:subject SUBJECT :cascade CASCADE
   :promoted-children N | :deleted-descendants N)

where the trailing key matches CASCADE: `reparent' reports the
count of promoted direct children; `delete' reports the count of
removed descendant headings (heading-nested only; resource
references in /other/ subjects' axiom rows are the caller's
concern -- B.1 deliberately does not touch them).  Signals on a
malformed CASCADE."
  (require 'elot-id-move)
  (require 'elot-id-rename nil 'noerror)
  (unless (memq cascade '(reparent delete))
    (error "elot-gptel--delete-apply: CASCADE must be `reparent' or `delete' (got %S)"
           cascade))
  (let ((marker (and (fboundp 'elot-id-rename--heading-marker-for-curie)
                     (elot-id-rename--heading-marker-for-curie subject))))
    (unless marker
      (error "elot-gptel--delete-apply: cannot locate heading for %s"
             subject))
    (let (count)
      (atomic-change-group
        (pcase cascade
          ('reparent
           ;; Count direct children up front so the return plist can
           ;; report `:promoted-children N'.  The helper itself does
           ;; not return a count.
           (setq count
                 (save-excursion
                   (goto-char marker)
                   (org-back-to-heading t)
                   (let ((n 0))
                     (save-excursion
                       (when (and (fboundp 'org-goto-first-child)
                                  (org-goto-first-child))
                         (cl-incf n)
                         (let ((prev nil))
                           (while (progn
                                    (setq prev (point))
                                    (org-forward-heading-same-level 1 t)
                                    (/= (point) prev))
                             (cl-incf n)))))
                     n)))
           (elot-id-remove-heading-promote-children marker))
          ('delete
           ;; Count every descendant heading inside SUBJECT's subtree
           ;; (not counting SUBJECT itself) so the return plist can
           ;; report `:deleted-descendants N'.
           (setq count
                 (save-excursion
                   (goto-char marker)
                   (org-back-to-heading t)
                   (let ((end (save-excursion
                                (org-end-of-subtree t t)
                                (point)))
                         (n 0))
                     (save-excursion
                       (forward-line 1)
                       (while (re-search-forward "^\\*+ " end t)
                         (cl-incf n)))
                     n)))
           (elot-id-delete-heading-subtree marker)))
        (cond
         ((fboundp 'elot-headline-hierarchy-mark-stale)
          (elot-headline-hierarchy-mark-stale))
         ((fboundp 'elot-update-headline-hierarchy)
          (ignore-errors (elot-update-headline-hierarchy)))))
      (set-marker marker nil)
      (list :subject subject
            :cascade cascade
            (if (eq cascade 'reparent)
                :promoted-children
              :deleted-descendants)
            count))))


;;; ---------------------------------------------------------------------------
;;; M9.9.B.2 -- `elot_delete_resource' singleton tool wrapper
;;; ---------------------------------------------------------------------------
;;;
;;; Thin LLM-facing wrapper over the B.1 core primitive.  Adds the
;;; pre-flight reference scan (Slice A), the cascade branch
;;; (`refuse' / `reparent' / `delete'), the M9 write-back contract
;;; (side-effects gate, save + revalidate + rollback), and a
;;; `dry_run' mode that validates against an in-memory draft and
;;; restores the buffer afterwards.
;;;
;;; Refusal contract:
;;;   - Any axiom-row or src-block reference to SUBJECT on *other*
;;;     subjects -> refuse, regardless of cascade.  B.2 does not
;;;     rewrite dependent rows; that is `elot_edit_axiom' /
;;;     `elot_replace_with_parent' territory.  The diagnostic
;;;     enumerates the dangling rows (file line + subject + keyword
;;;     + fragment) so the LLM can compose the repair calls.
;;;   - Children present + cascade=refuse (default) -> refuse with
;;;     the child list and a cascade hint.
;;;
;;; Out of scope for v1 (deferred to a future slice):
;;;   - Recursive per-descendant reference scan on cascade=delete.
;;;     The current implementation scans SUBJECT only; if a
;;;     descendant has unrelated incoming axiom-row references the
;;;     revalidate step will surface the signature violation and
;;;     roll the change back.  Acceptable for v1; revisitable when
;;;     batch-aware deletion lands (`elot_delete_resource_tree').
;;;   - Unified-diff envelope (the rename / move wrappers also
;;;     return just lint+OMN reports; consistency wins over a
;;;     bespoke shape).

(declare-function elot-slurp-to-vars "elot-tangle" ())
(defvar elot-slurp)

(defun elot-gptel--delete-format-refusal (curie axiom-rows src-rows)
  "Render an actionable refusal line listing dangling references."
  (let ((n (+ (length axiom-rows) (length src-rows))))
    (with-temp-buffer
      (insert
       (format "cannot delete %s: %d dangling reference%s on other subjects"
               curie n (if (= n 1) "" "s")))
      (dolist (r axiom-rows)
        (insert (format "\n  line %d  %s  %s :: %s"
                        (or (plist-get r :line) 0)
                        (or (plist-get r :curie) "?")
                        (or (plist-get r :keyword) "?")
                        (or (plist-get r :fragment) ""))))
      (dolist (r src-rows)
        (insert (format "\n  line %d  (%s src-block)"
                        (or (plist-get r :line) 0)
                        (or (plist-get r :language) "src"))))
      (insert
       "\nrewrite the dependent rows first (elot_edit_axiom) or weaken via elot_replace_with_parent")
      (buffer-string))))

(defun elot-gptel--delete-format-ok (curie cascade result prose-count
                                           annotation-rows suffix)
  "Render the OK envelope head for a successful delete.
CURIE is the deleted subject; CASCADE is the symbol the
primitive ran under (`reparent' or `delete'); RESULT is the
B.1 return plist; PROSE-COUNT is the number of prose
mentions the reference scan flagged; ANNOTATION-ROWS is the
list of advisory annotation-property rows on other subjects
that still mention CURIE (each plist carries :line :curie
:keyword :fragment); SUFFIX is appended verbatim (used by
`dry_run' to mark the file untouched).

When ANNOTATION-ROWS is non-empty, a `NOTE:' block is
appended after the OK line enumerating each row in the
`line N  curie  KEY :: FRAGMENT' shape -- the same shape
used by the dangling-reference refusal, but advisory only
(OWL tolerates annotation values pointing at undeclared
IRIs).  The OK line itself also picks up an ` -- audit
recommended' tail."
  (let* ((cascade-tail
          (pcase cascade
            ('reparent
             (let ((n (or (plist-get result :promoted-children) 0)))
               (if (zerop n) ""
                 (format "; %d child heading%s promoted"
                         n (if (= n 1) "" "s")))))
            ('delete
             (let ((n (or (plist-get result :deleted-descendants) 0)))
               (if (zerop n) ""
                 (format "; %d descendant heading%s removed with subtree"
                         n (if (= n 1) "" "s")))))
            (_ "")))
         (n-ann (length annotation-rows))
         (audit (if (and (integerp prose-count) (> prose-count 0))
                    " -- audit recommended" ""))
         (head
          (format
           "OK: deleted %s (%d prose mention%s skipped%s%s)%s"
           curie (or prose-count 0)
           (if (= (or prose-count 0) 1) "" "s")
           audit cascade-tail suffix)))
    (if (zerop n-ann)
        head
      (with-temp-buffer
        (insert head)
        (insert (format
                 "\n\nNOTE: %d annotation row%s on other subjects still reference %s -- audit recommended:"
                 n-ann (if (= n-ann 1) "" "s") curie))
        (dolist (r annotation-rows)
          (insert (format "\n  line %d  %s  %s :: %s"
                          (or (plist-get r :line) 0)
                          (or (plist-get r :curie) "?")
                          (or (plist-get r :keyword) "?")
                          (or (plist-get r :fragment) ""))))
        (buffer-string)))))

(defun elot-gptel-tool-delete-resource
    (file subject &optional cascade dry-run)
  "Implementation of the `elot_delete_resource' tool (M9.9 B.2).

Removes the resource heading SUBJECT (CURIE preferred or
unambiguous label) from FILE, with collateral handling
governed by CASCADE:

  `refuse'   (default)  Refuse when SUBJECT has semantic
                        dependents (outline children, or other
                        subjects whose SubClassOf/SubPropertyOf
                        row names SUBJECT); the LLM must
                        explicitly request `reparent' or
                        `delete'.
  `reparent'            Promote SUBJECT's direct children one
                        outline level so they re-attach to
                        SUBJECT's former outline parent.
  `delete'              Remove SUBJECT together with its entire
                        subtree (every descendant heading).

Independent of CASCADE the tool refuses whenever the
pre-flight reference scan finds an axiom-row or
OMN/SPARQL src-block reference to SUBJECT on some *other*
subject -- B.2 does not rewrite dependent rows.  The
diagnostic enumerates the dangling rows so the LLM can
repair them via `elot_edit_axiom' or fold SUBJECT into a
parent via `elot_replace_with_parent' first.

DRY-RUN, when non-nil, runs the full pipeline against an
in-memory draft, validates via the read-only `content='
code path, and restores the buffer afterwards -- the LLM
gets the same OK / FAIL envelope a real commit would
produce while nothing on disk changes.  DRY-RUN bypasses
the side-effects gate (read-only by construction).

Returns:
  OK: deleted SUBJECT (N prose mention(s) skipped[; K child
  heading(s) promoted | M descendant heading(s) removed
  with subtree])
  == LINT ==
  ...
  [== OMN PARSE ==
   ...]

or an `ERROR:' / `FAIL:' line on refusal / failure."
  (condition-case err
      (progn
        (unless (or dry-run elot-gptel-allow-side-effects)
          (user-error
           "elot-gptel: delete refused -- side effects disabled \
(set `elot-gptel-allow-side-effects' to t, or pass dry_run=true)"))
        (unless (and (stringp subject) (not (string-empty-p subject)))
          (user-error
           "elot-gptel: subject must be a non-empty CURIE or label"))
        (let* ((cascade-sym
                (cond
                 ((null cascade) 'refuse)
                 ((and (stringp cascade) (string-empty-p cascade)) 'refuse)
                 ((stringp cascade)
                  (if (member cascade '("refuse" "reparent" "delete"))
                      (intern cascade)
                    (user-error
                     "elot-gptel: cascade must be \"refuse\", \"reparent\" or \"delete\": %S"
                     cascade)))
                 ((symbolp cascade)
                  (if (memq cascade '(refuse reparent delete))
                      cascade
                    (user-error
                     "elot-gptel: cascade must be `refuse', `reparent' or `delete': %S"
                     cascade)))
                 (t (user-error
                     "elot-gptel: invalid cascade %S" cascade))))
               (true-file (elot-gptel--resolve-file file))
               (buf (or (find-buffer-visiting true-file)
                        (let ((inhibit-message t))
                          (find-file-noselect true-file 'nowarn)))))
          (unless (buffer-live-p buf)
            (user-error "elot-gptel: cannot open %s" file))
          (require 'elot-id-rename)
          (require 'elot-tangle nil 'noerror)
          (with-current-buffer buf
            (unless (derived-mode-p 'org-mode)
              (let ((org-inhibit-startup t) (org-mode-hook nil))
                (org-mode)))
            (when (fboundp 'elot-update-headline-hierarchy)
              (condition-case _ (elot-update-headline-hierarchy)
                (error nil)))
            (when (fboundp 'elot-slurp-to-vars)
              (condition-case _ (elot-slurp-to-vars) (error nil)))
            (let* ((slurp (and (boundp 'elot-slurp) elot-slurp))
                   (row (and slurp
                             (elot-gptel--axiom-resolve-subject
                              subject slurp)))
                   (curie (car row)))
              (unless curie
                (user-error
                 "%s is not a declared resource heading in %s"
                 subject file))
              (let* ((scan (elot-gptel--delete-reference-scan curie))
                     (children    (plist-get scan :children))
                     (axiom-rows  (plist-get scan :axiom-rows))
                     (annotation-rows (plist-get scan :annotation-rows))
                     (src-rows    (plist-get scan :src-rows))
                     (prose-count (plist-get scan :prose-count)))
                (unless (plist-get scan :node)
                  (user-error
                   "%s is not a declared resource heading in %s"
                   curie file))
                (when (or axiom-rows src-rows)
                  (user-error
                   "%s"
                   (elot-gptel--delete-format-refusal
                    curie axiom-rows src-rows)))
                (when (and children (eq cascade-sym 'refuse))
                  (user-error
                   "%s has %d semantic dependent%s: %s; pass cascade=reparent or cascade=delete"
                   curie (length children)
                   (if (= (length children) 1) "" "s")
                   (mapconcat #'identity children ", ")))
                (let* ((effective-cascade
                        (if children cascade-sym 'reparent))
                       (before-text
                        (buffer-substring-no-properties
                         (point-min) (point-max)))
                       (before-modified-p (buffer-modified-p))
                       result)
                  (setq result
                        (elot-gptel--delete-apply curie effective-cascade))
                  (cond
                   (dry-run
                    (let* ((draft
                            (buffer-substring-no-properties
                             (point-min) (point-max)))
                           (verdict
                            (elot-gptel--axioms-revalidate-content
                             file draft))
                           (ok (plist-get verdict :ok))
                           (report (plist-get verdict :report)))
                      ;; Restore buffer to baseline; disk never touched.
                      ;; Note: do NOT bind `inhibit-modification-hooks'
                      ;; here -- that hides the erase/insert from
                      ;; `org-element--cache', which then trips its
                      ;; "Unregistered buffer modifications detected"
                      ;; warning and force-resets on the next access.
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert before-text))
                      (set-buffer-modified-p before-modified-p)
                      (when (fboundp 'elot-headline-hierarchy-mark-stale)
                        (elot-headline-hierarchy-mark-stale))
                      (cond
                       ((not ok)
                        (concat
                         "FAIL: dry-run revalidation failed (no file written)\n\n"
                         report))
                       (t
                        (concat
                         (elot-gptel--delete-format-ok
                          curie effective-cascade result prose-count
                          annotation-rows
                          "  (dry_run: file unchanged on disk)")
                         "\n\n" report)))))
                   (t
                    (save-buffer)
                    (let* ((verdict (elot-gptel--rename-revalidate file))
                           (ok (plist-get verdict :ok))
                           (report (plist-get verdict :report)))
                      (cond
                       ((not ok)
                        (let ((inhibit-read-only t))
                          (erase-buffer)
                          (insert before-text))
                        (save-buffer)
                        (set-buffer-modified-p before-modified-p)
                        (concat
                         "ERROR: revalidation failed -- changes rolled back\n\n"
                         report))
                       (t
                        (concat
                         (elot-gptel--delete-format-ok
                          curie effective-cascade result prose-count
                          annotation-rows "")
                         "\n\n" report)))))))))))) 
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))


;;; ---------------------------------------------------------------------------
;;; M9.9.F1 -- parent enumeration for `elot_replace_with_parent'
;;; ---------------------------------------------------------------------------
;;;
;;; The parent-enumeration slice of M9.9.F1.  Pure read; no tool spec,
;;; no dispatcher entry, no rename call, no `dry_run', no NOTE.  Just
;;; the helper that answers "what are SUBJECT's immediate parents?" --
;;; the guard whose presence is the whole value F1 adds over a bare
;;; `elot_rename_resource' call.

(defconst elot-gptel--replace-parent-bare-curie-re
  "\\`[A-Za-z_][A-Za-z0-9_-]*:[A-Za-z_][A-Za-z0-9_.-]+\\'"
  "Regex matching a bare CURIE (no whitespace, no class-expression operators).
Used by `elot-gptel--replace--immediate-parents' to admit only
plain-CURIE values on a `SubClassOf' / `SubPropertyOf' row; class
expressions (`ex:foo and ex:bar', `not ex:foo', restrictions) are
intentionally rejected -- this mirrors the conservative tokeniser
stance the lint checker takes on axiom values.")

(defun elot-gptel--replace--find-with-parent (curie)
  "Walk `elot-headline-hierarchy' for CURIE; return cons (NODE . PARENT).
PARENT is the immediately enclosing hierarchy node (which may be
the buffer root, an ontology heading, a section heading, or a
resource heading).  Returns nil when CURIE is not declared in
the buffer.  Caller is responsible for ensuring the cache is
fresh."
  (let (found)
    (cl-labels ((walk (n parent)
                  (unless found
                    (when (string= (elot-gptel--delete--node-curie n) curie)
                      (setq found (cons n parent)))
                    (unless found
                      (dolist (c (plist-get n :children))
                        (walk c n))))))
      (let ((root (bound-and-true-p elot-headline-hierarchy)))
        (when root (walk root nil))))
    found))

(defun elot-gptel--replace--resource-parent-p (node)
  "Non-nil when NODE is a resource heading (not section root / ontology).
A resource parent carries a `:uri' (set by the tangle parser for
declared resources) and does NOT carry `:elot-context-type'
\"ontology\" (which marks the ontology heading even when it has
a composite `:uri').  Section roots have no `:uri' at all, so
they fall out of this predicate naturally."
  (and node
       (plist-get node :uri)
       (not (equal (plist-get node :elot-context-type) "ontology"))))

(defun elot-gptel--replace--subclass-row-curies (node)
  "List of bare-CURIE values on NODE's `SubClassOf' / `SubPropertyOf' rows.
Each top-level entry of NODE's `:descriptions' is a list whose
`car' is the row KEY and `cadr' the row VALUE (the tangle parser
appends any nested axiom-annotation sub-rows after that pair,
which we ignore here -- only the top-level value contributes a
parent).  Class expressions (`ex:foo and ex:bar', `not ex:foo',
`ex:r some ex:c') are rejected by the bare-CURIE matcher;
returns parents in document order, deduplicated."
  (let (acc)
    (dolist (item (plist-get node :descriptions))
      (when (and (consp item)
                 (stringp (car item))
                 (member (car item) '("SubClassOf" "SubPropertyOf"))
                 (stringp (cadr item)))
        (let ((val (string-trim (cadr item))))
          (when (string-match-p
                 elot-gptel--replace-parent-bare-curie-re val)
            (cl-pushnew val acc :test #'string=)))))
    (nreverse acc)))

(defun elot-gptel-tool-replace-with-parent (file subject &optional parent dry-run)
  "Implementation of the `elot_replace_with_parent' tool (M9.9.F1).

Thin composite over `elot_rename_resource' with a parent-guard.
Enumerates SUBJECT's immediate parents via
`elot-gptel--replace--immediate-parents'; refuses with a
structured ERROR when SUBJECT has zero parents, when PARENT is
supplied but not in the candidate set, or when SUBJECT has more
than one parent and PARENT is omitted.  On success defers
entirely to `elot-gptel-tool-rename-resource' with
SOURCE=SUBJECT and TARGET=PARENT (or the auto-picked sole
parent).  Appends a NOTE to the OK envelope pointing the
caller at `elot_delete_resource' as the natural follow-up.

DRY_RUN snapshots the file bytes and the buffer contents,
forces `elot-gptel-allow-side-effects' to t for the duration
of the inner call, runs the rename for its revalidate verdict,
and then restores both disk and buffer regardless of the
outcome -- so the LLM gets the same OK / FAIL envelope a real
commit would have produced while nothing on disk changes.
DRY_RUN bypasses the side-effects gate (read-only by
construction).

Returns either:
  OK: renamed SUBJECT -> PARENT (... rename envelope ...)
  ...
  NOTE: SUBJECT folded into PARENT
or an `ERROR:' line on refusal / failure."
  (condition-case err
      (progn
        (unless (and (stringp subject) (not (string-empty-p subject)))
          (user-error "elot-gptel: subject must be a non-empty CURIE string"))
        (require 'elot-id-rename)
        (let* ((true-file (elot-gptel--resolve-file file))
               (buf (or (find-buffer-visiting true-file)
                        (let ((inhibit-message t))
                          (find-file-noselect true-file 'nowarn))))
               candidates chosen)
          (unless (buffer-live-p buf)
            (user-error "elot-gptel: cannot open %s" file))
          (with-current-buffer buf
            (unless (derived-mode-p 'org-mode)
              (let ((org-inhibit-startup t) (org-mode-hook nil))
                (org-mode)))
            (setq candidates
                  (elot-gptel--replace--immediate-parents subject)))
          (cond
           ((null candidates)
            (format
             "ERROR: %s has no immediate parents; nothing to weaken to"
             subject))
           ((and (stringp parent) (not (string-empty-p parent))
                 (not (member parent candidates)))
            (format
             "ERROR: %s is not an immediate parent of %s; candidates: %s"
             parent subject (mapconcat #'identity candidates " | ")))
           ((and (or (null parent)
                     (and (stringp parent) (string-empty-p parent)))
                 (> (length candidates) 1))
            (format
             "ERROR: %s has %d immediate parents; supply parent= to pick one; candidates: %s"
             subject (length candidates)
             (mapconcat #'identity candidates " | ")))
           (t
            (setq chosen
                  (if (and (stringp parent) (not (string-empty-p parent)))
                      parent
                    (car candidates)))
            (let* ((note (concat
                          "\n\nNOTE: " subject
                          " folded into " chosen))
                   (note-dry (concat note
                                     "  (dry_run: file unchanged on disk)"))
                   (run-rename
                    (lambda ()
                      (elot-gptel-tool-rename-resource
                       file subject chosen nil nil nil "merge"))))
              (if dry-run
                  (let* ((buf-bytes
                          (with-current-buffer buf
                            (buffer-substring-no-properties
                             (point-min) (point-max))))
                         (buf-mod (with-current-buffer buf
                                    (buffer-modified-p)))
                         (elot-gptel-allow-side-effects t)
                         (result (funcall run-rename)))
                    (unwind-protect
                        (if (string-prefix-p "OK:" result)
                            (concat result note-dry)
                          result)
                      ;; Restore both disk and buffer to baseline via
                      ;; the visiting buffer's own save path.  The
                      ;; rename body (or, in tests, a stub) has
                      ;; advanced the disk mtime; calling
                      ;; `set-visited-file-modtime' first aligns the
                      ;; buffer to whatever disk currently holds so
                      ;; the subsequent `erase-buffer' does not trip
                      ;; `ask-user-about-supersession-threat' (which
                      ;; signals "Cannot resolve conflict in batch
                      ;; mode" under --batch).  `save-buffer' then
                      ;; writes baseline bytes back to disk and
                      ;; refreshes modtime in one step, avoiding the
                      ;; file-lock contention that a parallel
                      ;; `with-temp-file true-file' would hit while
                      ;; this buffer still visits the file.
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (when (file-exists-p true-file)
                            (ignore-errors
                              (set-visited-file-modtime)))
                          (let ((inhibit-read-only t))
                            (widen)
                            (erase-buffer)
                            (insert buf-bytes))
                          (set-buffer-modified-p t)
                          (let ((inhibit-message t)
                                (make-backup-files nil)
                                (create-lockfiles nil))
                            (save-buffer))
                          (set-buffer-modified-p buf-mod)))))
                (let ((result (funcall run-rename)))
                  (if (string-prefix-p "OK:" result)
                      (concat result note)
                    result))))))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))


(defun elot-gptel--replace--immediate-parents (subject)
  "Return ordered list of immediate-parent CURIEs of SUBJECT.

Two sources, combined in this order then deduplicated:

  1. The heading-nesting parent of SUBJECT, IF that parent is a
     resource heading (carries `:uri' and is not an ontology
     heading or section root).  Heading nesting carries
     SubClassOf / SubPropertyOf, so a resource heading enclosing
     SUBJECT is an implicit immediate parent.

  2. The bare-CURIE values on SUBJECT's `SubClassOf' /
     `SubPropertyOf' description-list rows.  Class expressions
     are conservatively rejected -- only plain CURIEs (matched
     against `elot-gptel--replace-parent-bare-curie-re') count.

Returns nil when SUBJECT is not declared in the buffer, when it
has no resource heading-nesting parent, and when it carries no
plain-CURIE SubClassOf / SubPropertyOf row.

Calls `elot-headline-hierarchy-ensure-fresh' up front (F8
belt-and-braces) so freshly inserted / renamed headings are
visible.  Pure read; never mutates the buffer."
  (when (fboundp 'elot-headline-hierarchy-ensure-fresh)
    (ignore-errors (elot-headline-hierarchy-ensure-fresh)))
  (let* ((np (and subject
                  (not (string-empty-p subject))
                  (elot-gptel--replace--find-with-parent subject)))
         (node (car np))
         (parent (cdr np)))
    (when node
      (let* ((heading-parent
              (and (elot-gptel--replace--resource-parent-p parent)
                   (elot-gptel--delete--node-curie parent)))
             (row-parents
              (elot-gptel--replace--subclass-row-curies node))
             (acc (if heading-parent (list heading-parent) nil)))
        (dolist (p row-parents)
          (cl-pushnew p acc :test #'string=))
        (nreverse acc)))))


;;; ---------------------------------------------------------------------------
;;; Tool registry
;;; ---------------------------------------------------------------------------

(defconst elot-gptel--tool-specs
  `(("elot_conventions"
     :function elot-gptel-tool-conventions
     :description
     "Return the ELOT authoring-conventions cheat sheet as Markdown.

ELOT is a literate ontology authoring format: an Org-mode document
*is* the ontology source.  Heading nesting carries SubClassOf /
SubPropertyOf; description-list `- key :: value' rows carry
annotations and OMN axioms; reuse of external terms goes via a
heading plus `rdfs:isDefinedBy'.

Call this tool once at the start of any LLM-driven authoring
session before composing edits to an ELOT .org file.  The
returned document covers the cardinal idioms (heading nesting,
heading shape `Label (curie)', description-list keys, file
skeleton, reuse via `rdfs:isDefinedBy', the often-misunderstood
`:nodeclare:' tag, default-prefix mechanics) and embeds a self-
contained worked exemplar (a small `pets' ontology in a fenced
`org' code block) demonstrating every idiom inline.

Read-only; takes no arguments."
     :args ())
    ("elot_check"
     :function elot-gptel-tool-check
     :description
     "Composite lint -> OMN parse -> consistency -> unsatisfiable
check for an ELOT .org file, in one tool call.

Pipeline (fail-fast):
  1. LINT          - runs `elot_lint' on FILE (or on CONTENT
                     when supplied); any lint *error* aborts.
  2. OMN PARSE     - runs `elot_omn_validate' (optionally with
                     PROFILE); a failed parse aborts.
  3. CONSISTENCY   - runs `elot_consistency' with REASONER; an
                     inconsistent ontology aborts.
  4. UNSATISFIABLE - runs `elot_unsatisfiable' only when stage
                     3 reported `consistent but has unsatisfiable
                     classes'.

Lint *warnings* never block.  When ROBOT is not configured,
stages 2-4 are reported SKIPPED and the pipeline still returns a
clean verdict if lint was clean.

Returns a multi-block plain-text report with stage headers
\(`== LABEL ==') and a final `== SUMMARY ==' block.  The
SUMMARY's first line is one of:
  OK: all checks pass (lint + OMN parse + ... + consistency)
  OK: lint clean (ROBOT-backed stages SKIPPED)
  FAIL at stage: <stage> (<reason>)

Use this as the default \"is my ontology OK?\" tool; reach for
the atomic tools (`elot_lint', `elot_omn_validate', etc.) only
when you need a single stage in isolation.  Read-only."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "content"
             :type string
             :optional t
             :description
             "Optional ELOT .org draft string.  When supplied, the \
bytes are checked in place of the on-disk contents of FILE.  \
Same semantics as the `content' arg on `elot_lint' / \
`elot_omn_validate'.")
      (:name "profile"
             :type string
             :optional t
             :enum ["DL" "EL" "QL" "RL" "Full"]
             :description
             "Optional OWL 2 profile for the OMN-parse stage.")
      (:name "reasoner"
             :type string
             :optional t
             :enum ["hermit" "whelk"]
             :description
             "OWL reasoner for the consistency / unsatisfiable \
stages (default `hermit').")))
    ("elot_lint"
     :function elot-gptel-tool-lint
     :description
     "Lint an ELOT .org ontology source file.  Runs the standard
org-lint checkers plus ELOT-specific checkers (prefix table,
ontology header, required sections, description-list CURIEs,
OWL Manchester syntax, etc.).  Returns a plain-text report;
each issue is one line `LINE:COL [CATEGORY/TRUST] MESSAGE',
followed by a final `Summary: N errors, M warnings'.  Returns
`OK: no lint issues' when the file is clean."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "severity"
             :type string
             :optional t
             :description
             "Filter: `error', `warning', or `all' (default).")
      (:name "categories"
             :type array
             :items (:type string)
             :optional t
             :description
             "Restrict to a subset of org-lint checker names \
(e.g. [\"elot/prefix-table\", \"elot/omn-syntax\"]).")
      (:name "content"
             :type string
             :optional t
             :description
             "Optional ELOT .org draft string.  When supplied, the \
bytes are linted in place of the on-disk contents of FILE.  Lets an \
LLM lint its in-flight edits without asking the user to save first.  \
FILE is still required and the project-traversal guard still \
applies, but FILE need not exist on disk.")))
    ("elot_omn_validate"
     :function elot-gptel-tool-omn-validate
     :description
     "Validate the OMN export of an ELOT .org file using ROBOT.

Tangles each ontology node in the file to a temporary OMN file
and runs `robot convert', which exercises the full OWLAPI parser.
This catches whole-file errors the per-axiom ELOT lint cannot
\(e.g. an object property used in class position, a datatype
property declared with a class as its range).  Optionally also
runs `robot validate-profile' for an OWL 2 profile.

Returns `OK: ...' when all ontology nodes parse successfully,
or a multi-line error report otherwise.  Requires ROBOT to be
configured (`elot-robot-jar-path' or a `robot' executable on
PATH).  Read-only -- writes only into a temporary workspace."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "profile"
             :type string
             :optional t
             :enum ["DL" "EL" "QL" "RL" "Full"]
             :description
             "Optional OWL 2 profile to validate against.")
      (:name "content"
             :type string
             :optional t
             :description
             "Optional ELOT .org draft string.  When supplied, the \
bytes are validated in place of the on-disk contents of FILE.  \
Same semantics as the `content' arg on `elot_lint'.")))
    ("elot_omn_report"
     :function elot-gptel-tool-omn-report
     :description
     "Run ROBOT's `report' against an ELOT .org file and return the report.

Tangles each ontology node in FILE to a temporary OMN file and
runs `robot report --fail-on none' against each, capturing the
report body verbatim.  Which queries fire is ROBOT's business,
not ELOT's: defer to the catalogue documented at
https://robot.obolibrary.org/report_queries/.  ELOT only
provides the integration seam (tangle + invoke + frame output).

Returns either
  OK: N ontology node(s) report clean (no violations)
when ROBOT's report comes back empty (or header-only TSV/CSV)
for every node, or a per-node block carrying the report body.
ROBOT errors are surfaced via the same classifier as
`elot_omn_validate' (`robot/syntax-error', `robot/io-error',
etc.).

CONTENT, when supplied, replaces the on-disk contents of FILE
in the same way as on `elot_lint' / `elot_omn_validate'.

Read-only -- writes only into a temporary workspace.  Requires
ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "format"
             :type string
             :optional t
             :enum ["tsv" "csv" "yaml" "json" "html" "md"]
             :description
             "ROBOT report output format (default `tsv').")
      (:name "content"
             :type string
             :optional t
             :description
             "Optional ELOT .org draft string; same semantics as on \
`elot_lint' / `elot_omn_validate'.")))
    ("elot_diff"
     :function elot-gptel-tool-diff
     :description
     "Show the structural diff between two ontologies using ROBOT.

Runs `robot diff --left BASELINE --right FILE --labels true' so
the LLM can interpret an OMN/RDF-level diff that humans
typically find hard to scan in ROBOT's raw output.  Each
ELOT .org input is tangled to OMN inside a temporary workspace
before ROBOT sees it; RDF inputs ROBOT can read directly
\(.ttl, .owl, .ofn, .omn, .rdf, ...) are passed verbatim.

Use this as the verification half of an edit/verify cycle:
snapshot the file before editing, edit, then run `elot_diff'
to confirm only the intended axioms changed.  Pairs naturally
with `elot_edit_axiom' and `elot_check'.  ELOT does NOT
re-rank, re-group, or filter ROBOT's output -- defer to ROBOT
for the structural diff, defer to the LLM for the human-
friendly retelling.

Returns either
  OK: no structural differences (baseline=..., file=...)
when the two inputs are structurally identical, or a
`Diff: baseline=... file=... format=...' block carrying
ROBOT's diff body verbatim.

Read-only -- writes only into a temporary workspace.  Requires
ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Current ontology -- ELOT .org file (project-relative) \
or an RDF file ROBOT can read directly.")
      (:name "baseline"
             :type string
             :description
             "Baseline ontology to diff against (an earlier snapshot \
of the same ontology).  Same accepted extensions as FILE.")
      (:name "format"
             :type string
             :optional t
             :enum ["plain" "pretty" "markdown" "html"]
             :description
             "ROBOT diff output format (default `plain').")))
    ("elot_sparql"
     :function elot-gptel-tool-sparql
     :description
     "Execute a SPARQL query against an ontology and return the results.

FILE may be an ELOT .org file (which is tangled to OMN inside a
temporary workspace before the query runs) or an RDF file ROBOT
can read directly (.ttl, .owl, .ofn, .omn, .rdf, .nt, .nq,
.jsonld, .obo, .rdfxml).

Use this tool to inspect what an ontology already contains
before introducing new terms (preventing hallucinated CURIEs),
to pull targeted subgraphs that would not otherwise fit in
context, and to verify edits by re-running the same query
before and after.

Returns the result rows as plain text in the chosen FORMAT
(\"tsv\" by default).  When LIMIT is exceeded the surplus
rows are dropped and a `... N more rows omitted' trailer is
appended.  Mutating SPARQL (INSERT / DELETE / ...) is refused
unless `elot-gptel-allow-side-effects' is non-nil.  Requires
ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file or an RDF file ROBOT can read.")
      (:name "query"
             :type string
             :description "SPARQL query text.")
      (:name "format"
             :type string
             :optional t
             :enum ["tsv" "csv" "json" "table"]
             :description
             "Result format (default tsv).  `table' renders an Org table.")
      (:name "limit"
             :type integer
             :optional t
             :description
             "Maximum number of data rows to return (default 200, max 5000).")))
    ("elot_sparql_select"
     :function elot-gptel-tool-sparql-select
     :description
     "Run a read-only SPARQL SELECT or ASK query against an ontology.

Strictly refuses CONSTRUCT, DESCRIBE, and any SPARQL Update
form (INSERT / DELETE / LOAD / CLEAR / CREATE / DROP / COPY /
MOVE / ADD) -- even when `elot-gptel-allow-side-effects' is
enabled.  Intended for confirm-free agent loops that need a
hard guarantee no mutating keyword can reach ROBOT.

Arguments and return shape are identical to `elot_sparql'.
Requires ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file or an RDF file ROBOT can read.")
      (:name "query"
             :type string
             :description "SPARQL SELECT or ASK query text.")
      (:name "format"
             :type string
             :optional t
             :enum ["tsv" "csv" "json" "table"]
             :description
             "Result format (default tsv).  `table' renders an Org table.")
      (:name "limit"
             :type integer
             :optional t
             :description
             "Maximum number of data rows to return (default 200, max 5000).")))
    ("elot_unsatisfiable"
     :function elot-gptel-tool-unsatisfiable
     :description
     "Report unsatisfiable classes in an ontology using ROBOT's reasoner.

Tangles each ontology node in FILE (an ELOT .org file) to OMN
inside a temporary workspace, then runs
`robot reason --reasoner R' to detect classes with no possible
instances under the open-world semantics.

Returns `OK: no unsatisfiable classes ...' when the reasoner
finds none, or a per-node block listing each offending IRI
(with `rdfs:label' resolved via the ELOT DB when available).

If the ontology is *inconsistent* (every class is trivially
unsatisfiable), this tool reports `INCONSISTENT' and points the
caller at `elot_consistency' for a clearer explanation.

REASONER is one of `hermit' (default) or `whelk'.
Requires ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "reasoner"
             :type string
             :optional t
             :enum ["hermit" "whelk"]
             :description
             "OWL reasoner to use (default `hermit').")))
    ("elot_consistency"
     :function elot-gptel-tool-consistency
     :description
     "Check whether an ontology is consistent using ROBOT's reasoner.

Tangles each ontology node in FILE (an ELOT .org file) to OMN
inside a temporary workspace, then runs
`robot reason --reasoner R' to determine satisfiability of the
ontology as a whole.

Returns one of:
  - `OK: ontology is consistent ...' when the reasoner finds a
    model.
  - `OK: ontology is consistent but has unsatisfiable classes
    ...' when the ontology has a model but some classes are
    necessarily empty (run `elot_unsatisfiable' for details).
  - `INCONSISTENT: <explanation>' when the ontology has no
    model.  Typical causes: an individual asserted to be in two
    disjoint classes, or a functional property with two distinct
    values for the same subject.

REASONER is one of `hermit' (default) or `whelk'.
Requires ROBOT to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "reasoner"
             :type string
             :optional t
             :enum ["hermit" "whelk"]
             :description
             "OWL reasoner to use (default `hermit').")))
    ("elot_explain"
     :function elot-gptel-tool-explain
     :description
     "Explain why an OWL axiom is entailed by an ontology.

Tangles each ontology node in FILE (an ELOT .org file) to OMN
inside a temporary workspace, then runs `robot explain' to
produce a justification (a minimal subset of axioms that
entails AXIOM).  Useful for diagnosing surprising inferences,
inconsistencies (explain `owl:Thing SubClassOf: owl:Nothing'),
and unsatisfiable classes (explain `Foo SubClassOf:
owl:Nothing').

AXIOM is a single Manchester Syntax axiom.  ROBOT's `--axiom'
parser resolves names through the ontology's *label-based*
short-form provider: it does NOT accept bare CURIEs or
angle-bracket IRIs as class/property names.  This tool rewrites
the axiom on the fly -- any CURIE (e.g. `ex:dog') or IRI it can
resolve to a known `rdfs:label' is substituted with that label
(single-quoted when needed) before being passed to ROBOT, and
the substitution is reported as a NOTE in the result.  Tokens
without a known label are passed through unchanged so ROBOT's
own diagnostic remains informative.  All three of these forms
work:

  \"Dog SubClassOf: Animal\"
  \"ex:dog SubClassOf: ex:animal\"
  \"<http://example.org/dog> SubClassOf: <http://example.org/animal>\"

`owl:Thing' and `owl:Nothing' are passed through as-is.

REASONER is one of `hermit' (default), `whelk', or `elk' --
passed through to ROBOT.  MAX caps the number of distinct
justifications returned (default 1).

Returns the Markdown explanation produced by ROBOT.  When ROBOT
reports \"No explanations found.\" the tool appends a HINT
pointing at `elot_consistency' / `elot_unsatisfiable' (the axiom
is either not entailed, or trivially entailed because the
ontology is inconsistent).  Aborts with an `ERROR:' line if the
call exceeds `elot-gptel-explain-timeout' (default 60 s).
Read-only with respect to your project files.  Requires ROBOT
to be configured."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file, relative to the project root.")
      (:name "axiom"
             :type string
             :description
             "Manchester Syntax axiom to explain.  Class and property \
names can be given as rdfs:labels (preferred -- ROBOT's native form), \
as CURIEs like `ex:dog', or as <full IRIs>; the tool auto-translates \
CURIEs and IRIs to labels via `elot-db-get-label-any' before calling \
ROBOT.  Examples: \"Dog SubClassOf: Animal\", \
\"ex:dog SubClassOf: ex:animal\", \
\"owl:Thing SubClassOf: owl:Nothing\".")
      (:name "reasoner"
             :type string
             :optional t
             :enum ["hermit" "whelk" "elk"]
             :description
             "OWL reasoner to use (default `hermit').")
      (:name "max"
             :type integer
             :optional t
             :description
             "Maximum number of distinct justifications to return \
(default 1).")))
    ("elot_rename_resource"
     :function elot-gptel-tool-rename-resource
     :description
     "Rename a declared resource CURIE everywhere it appears in an ELOT .org file.

Rewrites every CURIE occurrence of SOURCE -- in heading
parentheticals, description-list rows (tag and value positions
including OMN axiom bodies like `Domain ::', `Types ::',
`SubClassOf:'), and src-block bodies whose language is `omn'
or `sparql' -- to TARGET.  Full-IRI occurrences (`<...>' or
bare in annotation values) that expand to SOURCE's IRI under
the file's prefix table are rewritten to the full-IRI form of
TARGET, preserving the angle-vs-bare shape.  Prefix-table rows,
other src-block languages, and running prose are left alone;
the count of prose lines containing SOURCE is reported.

When TARGET's prefix is not declared in the file's prefix
table, TARGET_IRI must be supplied; the resulting prefix row is
added to the prefix table inside the same atomic group as the
CURIE rewrite.  When TARGET_IRI is omitted in that case, this
tool refuses with a structured `ERROR:' line listing the
`elot-db' candidates for that prefix (format:
`ERROR: undeclared prefix `p:'; candidates: <IRI1> | <IRI2>;
retry with target_iri=<IRI>'); empty candidate list -> 
`candidates: (none)' with a hint to supply a brand-new
target_iri.

Gated by `elot-gptel-allow-side-effects': refuses when the flag
is nil.  After the rewrite the file is saved and re-linted
(plus OMN-parsed when ROBOT is configured); revalidation
failure rolls the buffer back to its pre-rewrite contents and
returns the failing diagnostics.

On success returns:
  OK: renamed SOURCE -> TARGET (N CURIE, M IRI; K prose mention(s) skipped[ -- audit recommended][; (declared prefix p: -> <IRI>)])
  followed by the lint (and OMN-parse) reports.

Optionally rewrites the resource heading's rdfs:label in the
same atomic operation: pass NEW_LABEL to change `Label (curie)'
to `NEW_LABEL (curie)' while preserving trailing statistics
cookies and tags.  Omit NEW_LABEL to leave the label untouched.

Out of scope: description-list label edits beyond the heading
title, cross-file rewrites, and rewrites of external projects
that import this ontology."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "source"
             :type string
             :description
             "CURIE currently declared on a resource heading in FILE.")
      (:name "target"
             :type string
             :description
             "New CURIE.  Prefix should be declared in the file's \
prefix table; otherwise supply `target_iri'.")
      (:name "ontology"
             :type string
             :optional t
             :description
             "Disambiguates when FILE holds more than one ontology \
declaring the same SOURCE CURIE.  Reserved -- the v1 \
implementation operates on the whole buffer.")
      (:name "target_iri"
             :type string
             :optional t
             :description
             "Full IRI for TARGET's prefix when that prefix is not \
declared in the file.  When supplied, the rename adds the new \
prefix row to the prefix table inside the same atomic group as \
the CURIE rewrite.  Ignored when the prefix is already \
declared (so the LLM can pass it defensively).")
      (:name "new_label"
             :type string
             :optional t
             :description
             "Replacement rdfs:label for the resource-declaring \
heading.  When supplied (and non-empty), the heading's title \
text before the `(CURIE)' parenthetical is rewritten to this \
value inside the same atomic group as the CURIE rewrite; \
trailing statistics cookies and tags are preserved verbatim.  \
Omit when only the identifier should change.")
      (:name "op"
             :type string
             :optional t
             :enum ["rename" "merge"]
             :description
             "Rewrite mode.  `rename' (default) refuses when TARGET \
is already declared in the file (the canonical rewrite-and-declare \
path).  `merge' requires TARGET to be already declared: every \
reference to SOURCE is rewritten to TARGET, SOURCE's heading is \
removed, and SOURCE's heading-nested children are promoted one \
outline level.  Use `merge' to fold one resource into an existing \
co-extensive one (the workflow behind `elot_replace_with_parent').")))
    ("elot_move_resource"
     :function elot-gptel-tool-move-resource
     :description
     "Move a resource heading within an ELOT .org file.

Relocates the resource heading declared by SOURCE -- with its
whole subtree -- to a new parent (or section root) within the
same ontology, within FILE.  The CURIE itself is not rewritten;
this is a pure subtree relocation.  Use `elot_rename_resource'
when the CURIE needs to change.

TARGET is either the CURIE of an existing resource heading
inside FILE, or the literal string `top' meaning `place
SOURCE directly under the section root of its kind' (the
level-2 section heading: Classes / Object properties / Data
properties / Annotation properties / Datatypes / Individuals).
AS is `child' (default; SOURCE becomes a child of TARGET) or
`sibling' (SOURCE is placed right after TARGET's subtree, at
TARGET's level).

The move is rejected when SOURCE's section kind disagrees with
TARGET's section kind (a class can only move under another
class / under the Classes section root; same shape for the
other kinds), when SOURCE is declared in more than one ontology
in FILE, when the move is a no-op (TARGET already SOURCE's
parent), or when it would place SOURCE inside its own subtree.
Under Datatypes / Individuals, `as=child' with a level-3+
TARGET is refused -- those sections have no inherent
sub-relationship, so siblings or `target=top' are the only
legal placements.

Gated by `elot-gptel-allow-side-effects': refuses when the
flag is nil.  After the move the file is saved and re-linted
(plus OMN-parsed when ROBOT is configured); revalidation
failure rolls the buffer back to its pre-move contents and
returns the failing diagnostics.

On success returns:
  OK: moved SOURCE from FROM-PARENT to TO-PARENT (as AS)
  followed by the lint (and OMN-parse) reports.

Out of scope: cross-ontology moves in the same buffer (the
plan's Step 12.2), cross-file moves (Step 12.3), and CURIE
rewrites that need to accompany a move (delegate to
`elot_rename_resource')."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "source"
             :type string
             :description
             "CURIE of the resource heading to move \
(e.g. `ex:dog').")
      (:name "target"
             :type string
             :description
             "CURIE of the target heading (e.g. `ex:mammal'), or \
the literal string `top' to place SOURCE directly under the \
section root of its kind.")
      (:name "as"
             :type string
             :optional t
             :enum ["child" "sibling"]
             :description
             "Placement relative to TARGET: `child' (default; \
SOURCE becomes a child of TARGET) or `sibling' (SOURCE is \
inserted right after TARGET's whole subtree, at TARGET's \
level).  Ignored when TARGET is `top' -- the section-root \
placement is always equivalent to a child of the section \
heading.")))
    ("elot_delete_resource"
     :function elot-gptel-tool-delete-resource
     :confirm t
     :description
     "Remove a declared resource heading from an ELOT .org file.

SUBJECT identifies the heading to delete -- CURIE preferred
(e.g. `ex:dog'), or an unambiguous rdfs:label.

The tool runs a pre-flight reference scan before touching the
buffer.  Any axiom-row or OMN/SPARQL src-block reference to
SUBJECT on a *different* subject is grounds for refusal: the
diagnostic enumerates the dangling rows (file line + subject +
keyword + fragment) so the caller can repair them via
`elot_edit_axiom' / `elot_edit_axioms' or fold SUBJECT into
one of its parents via `elot_replace_with_parent' first.  This
tool deliberately does NOT auto-rewrite dependents -- that is
the closest-replacement-class problem, deferred elsewhere.

CASCADE governs the handling of SUBJECT's semantic dependents
-- outline children (heading-nesting carries SubClassOf /
SubPropertyOf) plus any other subject whose SubClassOf /
SubPropertyOf row names SUBJECT:

  `refuse'   (default)  Refuse with the dependent list when
                         SUBJECT has any semantic dependents;
                         the caller must opt into one of the
                         explicit cascades.
  `reparent'             Promote SUBJECT's direct children one
                         outline level so they become siblings
                         of SUBJECT's former position; SubClassOf
                         is preserved via the new heading nesting.
  `delete'               Remove SUBJECT and its entire subtree.

DRY_RUN runs the full pipeline (apply + revalidate via the
read-only `content=' code path) and restores the buffer
afterwards, so the LLM gets the same OK / FAIL envelope a
real commit would produce while nothing on disk changes.
Bypasses the side-effects gate (read-only by construction).

Gated by `elot-gptel-allow-side-effects' unless DRY_RUN is
true.  On a successful delete the file is saved, lint and
OMN-validate are re-run; revalidation failure rolls the
buffer back to its pre-delete contents and returns the
diagnostics.

On success returns:
  OK: deleted SUBJECT (N prose mention(s) skipped[ -- audit
  recommended][; K child heading(s) promoted | M descendant
  heading(s) removed with subtree])
  == LINT ==
  ...
  [== OMN PARSE ==
   ...]

Out of scope: cross-file deletion, prose-mention rewriting,
auto-archival of the deleted subtree to a `deprecated'
section (a separate first-class authoring move)."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "subject"
             :type string
             :description
             "CURIE (preferred) or unambiguous label of the \
resource heading to delete.")
      (:name "cascade"
             :type string
             :optional t
             :enum ["refuse" "reparent" "delete"]
             :description
             "How to handle SUBJECT's semantic dependents \
(outline children, plus other subjects whose SubClassOf / \
SubPropertyOf row names SUBJECT).  `refuse' (default) refuses \
if any exist; `reparent' promotes direct outline children one \
level; `delete' removes the whole subtree.")
      (:name "dry_run"
             :type boolean
             :optional t
             :description
             "When true, run the full pipeline but restore the \
buffer afterwards.  Same OK / FAIL envelope; file unchanged on \
disk.  Bypasses the side-effects gate (read-only by \
construction).")))
    ("elot_replace_with_parent"
     :function elot-gptel-tool-replace-with-parent
     :confirm t
     :description
     "Replace every use of SUBJECT with one of its immediate parents,
then optionally delete SUBJECT.

A pragmatic answer to the `closest replacement class' problem:
when the right replacement for a class one wants to delete is
one of its immediate parents, rewriting every use of SUBJECT
to that PARENT is a safe semantic weakening (anything
previously asserted as SUBJECT is still true under PARENT, by
the SubClassOf semantics).  This tool is a thin composite
over `elot_rename_resource' with a parent-guard -- the guard
is the whole value the wrapper adds over a bare rename.

Pipeline:
  1. Enumerate SUBJECT's immediate parents from two sources:
     the heading-nesting parent (when it is a resource
     heading), and the bare-CURIE values on SUBJECT's
     `SubClassOf' / `SubPropertyOf' description-list rows.
     Class expressions on those rows are conservatively
     rejected -- only plain CURIEs count.
  2. 0 candidates -> ERROR (nothing to weaken to).
     1 candidate, PARENT omitted -> auto-pick.
     >1 candidates, PARENT omitted -> refuse with the
       candidate list.
     PARENT supplied but not in the candidate set -> refuse
       with the candidate list.
  3. Defer to `elot_rename_resource' with source=SUBJECT and
     target=PARENT.  Inherits the full M9 write-back
     contract: gated by `elot-gptel-allow-side-effects',
     atomic apply with re-lint + OMN-parse, rollback on
     revalidation failure.
  4. Append a NOTE pointing at `elot_delete_resource' as the
     natural follow-up (advisory only).

DRY_RUN runs the full pipeline (rename + revalidate) and
restores both the file on disk and the in-memory buffer
afterwards, so the LLM gets the same OK / FAIL envelope a
real commit would have produced while nothing on disk
changes.  Bypasses the side-effects gate.

Out of scope: walking up more than one level, picking among
parents by a heuristic, replacing a property with its
inverse, and the deletion itself (call `elot_delete_resource'
in a follow-up call)."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "subject"
             :type string
             :description
             "CURIE of the resource whose uses are to be \
reattributed to its parent.")
      (:name "parent"
             :type string
             :optional t
             :description
             "CURIE of one of SUBJECT's immediate parents.  \
Required when SUBJECT has more than one immediate parent; \
otherwise auto-picked from the sole candidate.  Refused when \
not an immediate parent of SUBJECT.")
      (:name "dry_run"
             :type boolean
             :optional t
             :description
             "When true, run the full pipeline but restore the \
file on disk and the in-memory buffer afterwards.  Same \
OK / FAIL envelope; file unchanged.  Bypasses the side-effects \
gate (read-only by construction).")))
    ("elot_insert_sibling_resource"
     :function elot-gptel-tool-insert-sibling-resource
     :confirm t
     :description
     "Insert one or more new resource headings as siblings of an
existing heading in an ELOT .org file.

ANCHOR identifies the existing heading (CURIE preferred -- e.g.
`ex:dog' -- or an unambiguous label).  LABELS is a non-empty
array of plain rdfs:label strings; one new heading is inserted per
label.  Do not pass full heading titles of the form `Label (curie)'
here: these tools always mint fresh identifiers.  Reuse an existing
CURIE with the borrow/direct-declaration workflow instead.  The new
headings are placed AFTER ANCHOR's whole subtree, at ANCHOR's outline
level.

Identifiers are minted under the ontology heading's
`:ELOT-id-scheme:' via `elot-id-mint-batch' (collision-aware
against every CURIE already declared in the file).  The
description-list keys carried by ANCHOR are inherited
blank-valued (checkbox cookies reset to `[ ]'); nested
annotation rows are NOT replicated.

Missing `:ELOT-id-scheme:' returns an ERROR: pointing at
`elot-id-set-scheme' -- gptel tools cannot prompt
interactively, so the user must add the property before
retrying.

Gated by `elot-gptel-allow-side-effects'.  After insertion the
file is saved, lint and OMN-validate are re-run; revalidation
failure rolls the buffer back to its pre-insert contents.  On
success returns:

  OK: inserted N sibling(s) under ANCHOR
  Minted CURIEs:
    - ex:foo -- "Foo"
    - ex:bar -- "Bar"
    ...
  == LINT ==
  ...
  [== OMN PARSE ==
   ...]"
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "anchor"
             :type string
             :description
             "CURIE (preferred), heading title, or section :ID: / \
:CUSTOM_ID: of the heading the new sibling(s) follow.")
      (:name "labels"
             :type array
             :items (:type string)
             :description
             "Array of plain rdfs:label strings (not `Label (curie)' headings); length = batch size.")))
    ("elot_insert_child_resource"
     :function elot-gptel-tool-insert-child-resource
     :confirm t
     :description
     "Insert one or more new resource headings as first children of an
existing heading in an ELOT .org file.

Same argument shape as `elot_insert_sibling_resource'.  The new
headings land at one outline level deeper than ANCHOR, before
any existing children.

Refusal contract from the underlying interactive command:
ANCHOR at level 3+ under Datatypes or Individuals returns an
ERROR: -- those kinds have no inherent subtype / sub-individual
relation in OWL.  ANCHOR on the level-2 section heading itself
is always allowed (seeds the first resource of an empty
section).

Same write-back contract as `elot_insert_sibling_resource':
gated by `elot-gptel-allow-side-effects', auto-revalidate,
rollback on failure."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "anchor"
             :type string
             :description
             "CURIE (preferred), heading title, or section :ID: / \
:CUSTOM_ID: of the parent heading.  Use the section :ID: \
(e.g. `my-ont-object-property-hierarchy') to seed the first \
resource of an empty resource section.")
      (:name "labels"
             :type array
             :items (:type string)
             :description
             "Array of plain rdfs:label strings (not `Label (curie)' headings); length = batch size.")))
    ("elot_insert_resource_tree"
     :function elot-gptel-tool-insert-resource-tree
     :confirm t
     :description
     "Insert a forest of new resource headings under (or after) an
existing heading in an ELOT .org file.

TREE is a JSON array of nodes.  Each node is either a plain
rdfs:label string (a leaf -- no children) or an array [LABEL,
CHILD, ...] whose tail is recursively the same shape.  Do not pass
full heading titles of the form `Label (curie)' here: this tool
always mints fresh identifiers; reuse existing CURIEs via the
borrow/direct-declaration workflow.  Example:

  [[\"Mammal\", \"Mouse\", \"Whale\"], \"Bird\"]

inserts a `Mammal' branch with two leaf children plus a
`Bird' leaf.

AS controls how the top-level nodes are placed relative to
ANCHOR: `sibling' (default) places them after ANCHOR's whole
subtree at the same level; `child' places them as first
children of ANCHOR.  Every nested level is always inserted as
children of its parent.

Identifier minting, description-list inheritance, and kind
restrictions are inherited from `elot_insert_sibling_resource'
/ `elot_insert_child_resource' -- a deeply-nested Datatype or
Individual subtree may surface an ERROR at the offending
recursive call.

Same write-back contract.  On success returns:

  OK: inserted N heading(s) under ANCHOR (as AS)
  Minted CURIEs:
    - ex:... -- "Label" (level order: top-level siblings
      first, then each branch's children level by level)
    ...
  == LINT ==
  ...

CURIEs are listed in level order (not strict pre-order DFS)
so the caller can address them in a follow-up call.  Each
entry pairs the minted CURIE with its input label (F13) so
the caller can unambiguously route follow-up renames."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "anchor"
             :type string
             :description
             "CURIE (preferred), heading title, or section :ID: / \
:CUSTOM_ID: of the heading the tree attaches to.")
      (:name "tree"
             :type array
             :items (:type string)
             :description
             "Forest of nodes; each node is a LABEL string or a \
[LABEL, CHILD, ...] array (recursive).")
      (:name "as"
             :type string
             :optional t
             :enum ["sibling" "child"]
             :description
             "Placement of the top-level nodes relative to ANCHOR \
(default `sibling').")))
    ("elot_db_query"
     :function elot-gptel-tool-db-query
     :description
     "Run a read-only SQL query against the ELOT label database.

The database is the cross-session cache of every ontology
source the user has ever registered (see `elot-db.el').  It is
the right place to ask `what already exists?' before minting
new identifiers, to enumerate labels/types across sources, and
to look up provenance.  Use `elot_db_schema' to fetch the
authoritative table layout (DDL + current `schema_version'
row); the core tables are `sources', `entities', `attributes',
`prefixes', and `global_prefixes'.

SQL must be a SELECT or WITH ... SELECT statement.  Anything
mutating (INSERT, UPDATE, DELETE, DROP, PRAGMA, ...) is refused
at parse time, and the connection is additionally pinned to
`PRAGMA query_only = 1' for the duration of the call as a
belt-and-braces safeguard.

Returns a TSV-formatted result with a header row.  When the
result exceeds LIMIT (default 200, max 5000) the surplus rows
are dropped and a `... N more rows omitted' trailer is
appended."
     :args
     ((:name "sql"
             :type string
             :description
             "A SELECT or `WITH ... SELECT' SQL statement.")
      (:name "limit"
             :type integer
             :optional t
             :description
             "Maximum number of data rows to return \
(default 200, max 5000).")))
    ("elot_db_schema"
     :function elot-gptel-tool-db-schema
     :description
     "Return the ELOT label DB schema (DDL + version).

No arguments.  Returns a Markdown report containing the
code-side `elot-db-schema-version' constant, the stored
`schema_version' row read from the open database, and the full
DDL from the canonical `schema.sql' file as a fenced ```sql```
code block.

Call this once at the start of a session to let the model
compose correct `elot_db_query' statements without having the
schema hard-coded in the system prompt.  Read-only."
     :args ())
    ("elot_db_get_label"
     :function elot-gptel-tool-db-get-label
     :description
     "Return the best rdfs:label the ELOT DB knows for TOKEN.

TOKEN may be a CURIE (e.g. `ex:dog'), a full IRI -- bare or
in angle-brackets (`<http://example.org/dog>') -- or a literal
`entities.id' string.  Lookup consults the buffer's active
label sources first, then the cross-source prefix-rewrite
passes (CURIE<->IRI).

Returns the label on one line, or `(no label)' when nothing is
known.  Use this before introducing a new term: if the DB
already labels the IRI you were about to mint, prefer reuse
(see `elot_db_borrow_term', Milestone 6 Step 6.5).  Read-only."
     :args
     ((:name "token"
             :type string
             :description
             "A CURIE, full IRI (with or without angle brackets), \
or literal entity id to look up.")))
    ("elot_db_list_sources"
     :function elot-gptel-tool-db-list-sources
     :description
     "List every source currently registered in the ELOT label DB.

Returns a TSV with columns `source', `data_source', `type',
`last_modified', `last_updated' (all sources, not just the
buffer's active ones).  Useful as the first step of a reuse
check: which ontologies has the user already encountered?
Read-only."
     :args ())
    ("elot_db_activate_source"
     :function elot-gptel-tool-db-activate-source
     :description
     "Register-if-needed and activate FILE as a label source.

Closes the LLM label-lookup loop: if FILE has never been
ingested into the ELOT label DB, this parses it and writes the
rows; if FILE is registered but stale (mtime advanced), this
re-ingests; either way, the source is moved to the head of the
active-source list so subsequent gptel tool calls in the same
session resolve labels for FILE's entities (avoiding the silent
CURIE-passthrough mode where `elot_explain' fails with
\"Encountered ex:cat -- expected: Class name\").

Read-only with respect to the =.org= source -- mutates only the
label DB and the active-source list -- and NOT gated by
`elot-gptel-allow-side-effects' (think of it as session setup,
the same category as `set-variable').  Idempotent: a second
call on an already-fresh, already-active source returns
`status=already-active'.

Returns a single line:
  `OK: activated FILE (status=STATUS, N entities)'
where STATUS is one of `registered', `refreshed',
`already-up-to-date', or `already-active'."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (or any source ELOT's \
parser dispatcher knows: .csv, .tsv, .json, .ttl, .rq), \
project-relative.")))
    ("elot_db_remove_source"
     :function elot-gptel-tool-db-remove-source
     :confirm t
     :description
     "Remove a registered source (and its cascade) from the ELOT label DB.

Deletes the matching row(s) in the `sources' table; the FK
`ON DELETE CASCADE' declared on `entities', `attributes', and
`prefixes' removes the dependent rows in the same operation.
The shared `global_prefixes' table is untouched.

Exact mode (LIKE absent / false):
  SOURCE and DATA_SOURCE are matched verbatim.  DATA_SOURCE
  defaults to the empty-string sentinel ELOT uses for non-SPARQL
  sources.  At most one row can match.

Pattern mode (LIKE true):
  SOURCE and DATA_SOURCE are SQL LIKE patterns (`%' matches any
  sequence; `_' matches one character).  DATA_SOURCE defaults to
  `%' (match any).  Any number of rows may match.  As a safety
  guard the tool refuses SOURCE=`' or SOURCE=`%' unless
  ALLOW_ALL is also true -- which would otherwise nuke the whole
  table.

DRY_RUN, when true, lists the rows that would be deleted (TSV
with columns `source', `data_source', `type', `last_modified',
`last_updated' in pattern mode; a one-line existence check in
exact mode) without touching the DB.  Bypasses the side-effects
gate (read-only by construction).

Gated by `elot-gptel-allow-side-effects' for the real delete;
DRY_RUN does not require the gate.  Returns `OK: removed N
source(s) (...)' on success, or an `ERROR:' line on refusal /
failure.  Use `elot_db_list_sources' first to inspect the
relevant rows."
     :args
     ((:name "source"
             :type string
             :description
             "Source identifier (exact, by default) or SQL LIKE \
pattern when `like' is true.  Use `elot_db_list_sources' to \
discover candidate values.")
      (:name "data_source"
             :type string
             :optional t
             :description
             "Secondary key (SPARQL endpoint / data file).  In \
exact mode defaults to the empty-string sentinel; in pattern \
mode defaults to `%' (match any).")
      (:name "like"
             :type boolean
             :optional t
             :description
             "When true, treat SOURCE and DATA_SOURCE as SQL \
LIKE patterns (multi-row delete).")
      (:name "allow_all"
             :type boolean
             :optional t
             :description
             "When true in pattern mode, permits SOURCE=`' or \
`%' (which would otherwise be refused as accidental \
whole-table deletion).  Ignored in exact mode.")
      (:name "dry_run"
             :type boolean
             :optional t
             :description
             "When true, preview the rows that would be \
removed without performing the DELETE.  Bypasses the \
side-effects gate.")))
    ("elot_db_expand_curie"
     :function elot-gptel-tool-db-expand-curie
     :description
     "Expand CURIE (prefix:local) to a full IRI using the ELOT DB.

Consults the active sources' prefix tables first, then the
shared `global_prefixes' table.  Default-prefix form (`:foo')
is supported via the empty-prefix row.  Returns the IRI, or an
`ERROR:' line when the prefix is not known.  Symmetric to
`elot_db_get_label' for the IRI side of the identifier
triangle.  Read-only."
     :args
     ((:name "curie"
             :type string
             :description
             "A CURIE of the form `prefix:local' (the prefix may \
be empty for the default-prefix case).")))
    ("elot_db_get_attributes"
     :function elot-gptel-tool-db-get-attributes
     :description
     "List all (prop, value, lang, source) rows for an entity ID.

ID is the form ELOT stores in `entities.id' -- either a CURIE
(`ex:dog') or a full IRI, matching how the source ingested it.
When SOURCE is supplied, restrict the listing to that single
source row; otherwise return rows from every source.

Returns a TSV with columns `prop', `value', `lang', `source',
`data_source', or `OK: no rows' when nothing is stored.  This
is the natural way to ask \"what does the DB know about
`ex:dog'?\" without composing SQL.  Read-only."
     :args
     ((:name "id"
             :type string
             :description
             "Entity id (CURIE or full IRI) to inspect.")
      (:name "source"
             :type string
             :optional t
             :description
             "Optional source name; restrict to a single source's rows.")))
    ("elot_db_supertypes"
     :function elot-gptel-tool-db-supertypes
     :description
     "Return the direct supertypes asserted for ID.

Selects `attributes.value' where `prop' is one of
`rdfs:subClassOf', `rdfs:subPropertyOf' (RDF spellings used
by TTL ingest) or `SubClassOf', `SubPropertyOf' (OMN spellings
used by ELOT `.org' tangle ingest).  Both spellings are
queried so the result is uniform regardless of how a given
source was ingested.

Returns a TSV with columns `value', `prop', `source',
`data_source', or `OK: no rows' when ID has no asserted
supertype.  Useful for taxonomy walking (\"what is `ex:dog'
a kind of?\").  Read-only."
     :args
     ((:name "id"
             :type string
             :description
             "Entity id (CURIE or full IRI) to look up.")
      (:name "source"
             :type string
             :optional t
             :description
             "Optional source name; restrict to a single source's rows.")))
    ("elot_db_individual_types"
     :function elot-gptel-tool-db-individual-types
     :description
     "Return the asserted types of an individual ID.

Selects `attributes.value' where `prop' is `rdf:type' (RDF
spelling) or `Types' (OMN spelling), excluding the
`owl:NamedIndividual' marker value.  Restricted to ids that
are actually individuals -- the SELECT only returns rows when
ID has a `Types' row (OMN frame-keyword indicator) or a
`rdf:type owl:NamedIndividual' attestation in the same source.
This guard prevents class- or property-level `rdf:type' rows
(e.g. `rdf:type owl:Class') from being reported.

Returns a TSV with columns `value', `prop', `source',
`data_source', or `OK: no rows' when ID is not an individual
or has no asserted type.  Read-only."
     :args
     ((:name "id"
             :type string
             :description
             "Individual id (CURIE or full IRI).")
      (:name "source"
             :type string
             :optional t
             :description
             "Optional source name; restrict to a single source's rows.")))
    ("elot_db_search_label"
     :function elot-gptel-tool-db-search-label
     :description
     "Search the entire ELOT label database for entities matching QUERY.

Use this BEFORE minting a fresh identifier (M10's
`elot_mint_identifier'): if the cache already contains a term
labelled `Snake' from some past project, reuse that identifier
(and cite the source ontology via `rdfs:isDefinedBy') rather
than introducing a parallel one.  The search spans every
source the user has ever registered, not just the buffer's
active label sources.

QUERY matches as a case-insensitive substring against both
`entities.label' and `entities.id'.  Pass a string already
containing `%' wildcards for explicit LIKE semantics.

Optional filters:
- KIND restricts to one of: class, object-property, data-property,
  annotation-property, individual, datatype, ontology.
- SOURCE restricts to a single registered source name.
- LANG requires at least one `rdfs:label' row in that language.

Returns a TSV with columns: id, label, kind (the asserted
`rdf:type' CURIE), ontology_iri (the citation target for
`rdfs:isDefinedBy'), source, data_source, via.  The `via'
column marks how each row was found: `exact' (id equals the
query), `label-substring' (LIKE-substring hit), or
`local-name' (M11.1 cross-prefix fallback -- QUERY looked
like `prefix:local' but the entity was found under a
different prefix; inspect before adopting as a reuse).

Empty result -> a single `OK: no rows' line; use that as the
signal to fall through to `elot_mint_identifier'.  Read-only."
     :args
     ((:name "query"
             :type string
             :description
             "Substring (or LIKE pattern) to match against labels/ids.")
      (:name "kind"
             :type string
             :optional t
             :enum ["class" "object-property" "data-property"
                    "annotation-property" "individual" "datatype"
                    "ontology"]
             :description
             "Restrict to entities of this RDF type.")
      (:name "source"
             :type string
             :optional t
             :description
             "Restrict to a single registered source name.")
      (:name "lang"
             :type string
             :optional t
             :description
             "Require at least one `rdfs:label' row with this language tag.")
      (:name "limit"
             :type integer
             :optional t
             :description
             "Cap on returned rows (default 50, max 500).")
      (:name "exact_only"
             :type boolean
             :optional t
             :description
             "When true, suppress the M11.1 cross-prefix local-name fallback and require strict id/label matching.")))
    ("elot_db_borrow_term"
     :function elot-gptel-tool-db-borrow-term
     :description
     "Return an ELOT description-list snippet for reusing a term.

Given an entity TOKEN (CURIE like `ex:dog', or a full IRI --
bare or angle-bracketed) known to the ELOT label DB, render
an ELOT heading + description list ready to embed under the
target ontology.  The snippet ALWAYS contains an
`rdfs:isDefinedBy' back-pointer to the source ontology IRI
(or to a `(source: NAME)' fallback when no `owl:Ontology'
declaration is recorded), and a `skos:definition' line when
the DB has cached one.

The leading `*' in the heading is a placeholder -- callers
re-level it to match the target ontology's structure
(typically under a `:resourcedefs: yes' heading or as a
child of an existing resource heading).

Use this AFTER `elot_db_search_label' has produced a
matching CURIE, BEFORE `elot_add_term' (M9.3) embeds the
borrowed term into the target ontology.  Read-only."
     :args
     ((:name "token"
             :type string
             :description
             "CURIE, IRI, or angle-bracketed IRI known to the ELOT DB.")))
    ("elot_borrow_term"
     :function elot-gptel-tool-borrow-term
     :description
     "Composite reuse-before-mint: search the ELOT label DB and \
borrow the unique match in one call.

Pipeline:
- Zero candidates -> a single line
    OK: no candidates -- fall through to elot_mint_identifier (M10)
  Stable signal that no reuse is possible.
- Exactly one candidate -> auto-borrow: a one-line provenance
  preface (kind / source / via) followed by the same ELOT
  description-list snippet `elot_db_borrow_term' produces,
  including the `rdfs:isDefinedBy' back-pointer and any
  contingent NOTEs (default-prefix warning, missing-label
  placeholder, heading-nesting kind note).
- Multiple candidates -> a ranked candidate TSV (same columns
  as `elot_db_search_label': id, label, kind, ontology_iri,
  source, data_source, via) followed by a
    SELECT: N candidates match `LABEL' ...
  block telling the caller how to disambiguate: re-call with
  `source' (and optionally `lang') narrowed to one row, or
  call `elot_db_borrow_term' directly with the chosen id.

LABEL is matched case-insensitively as a substring against
both `entities.label' and `entities.id'.  KIND defaults to
`class' (the common authoring case); pass another kind to
borrow a property / individual / datatype.  SOURCE restricts
to a single registered source; LANG requires a label in the
given language tag.

The composite never silently picks among candidates --
disambiguation stays with the caller (LLM or human).  Use this
as the default `borrow a term' entry point; reach for the
atomic tools (`elot_db_search_label', `elot_db_borrow_term')
only when you need an open-ended substring search,
cross-source comparison, or a non-class kind with idiosyncratic
shaping.  Read-only."
     :args
     ((:name "label"
             :type string
             :description
             "Label or local-name to search for.")
      (:name "kind"
             :type string
             :optional t
             :enum ["class" "object-property" "data-property"
                    "annotation-property" "individual" "datatype"]
             :description
             "Entity kind (default `class').")
      (:name "source"
             :type string
             :optional t
             :description
             "Restrict to a single registered source name.")
      (:name "lang"
             :type string
             :optional t
             :description
             "Require a label in this language tag.")))
    ("elot_mint_identifier"
     :function elot-gptel-tool-mint-identifier
     :description
     "Mint a fresh identifier (CURIE) conforming to this project's
identifier scheme.

The scheme is configured by the user (`elot-id-default-scheme'
defcustom, the buffer-local `elot-id-scheme' variable, the Org
keyword `#+ELOT_ID_SCHEME', or the per-ontology Org property
`:elot-id-scheme:').  ELOT ships four built-in schemes -- `uuid',
`slug', `counter', `acme' (default, timestamp + random +
checksum) -- and users may register custom schemes via
`elot-id-register-scheme'.

SCHEME may be either a bare name (e.g. `acme') or a name
followed by scheme-specific tokens.  Two tokens are honoured
out of the box:
  - `counter GO_0000000' -- the literal template defines the
    alpha prefix (leading non-digit run) and the pad width
    (length of the trailing digit run).  Example output for
    that spec: `ex:GO_0000001'.
  - `acme slug:t' -- include a 5-char label slug in the
    identifier (default is the slugless 11-char form, which
    is shorter but reads as opaque).

Different projects use *radically* different identifier
conventions.  When the user has not specified which scheme
this project uses, ASK before minting -- do not guess.

FILE is the ELOT .org file being edited; its
`:ELOT-default-prefix:' property and existing declarations seed
the minting context (so collisions are avoided and the right
prefix is used).  The file need not exist on disk yet.  LABEL
is the `rdfs:label' the new identifier will carry; some schemes
(notably `slug' and `acme') derive part of the local-name from
it.  KIND is optional; some schemes encode it in the produced
identifier.

Returns a single line:
  OK: minted CURIE (scheme=NAME, label=...)
or an `ERROR:' line.  Read-only with respect to the .org file --
this tool only chooses a string.  Compose with `elot_add_term'
(Milestone 9) to actually introduce the term."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (scopes the minting context).")
      (:name "label"
             :type string
             :description
             "The rdfs:label the identifier will carry.")
      (:name "scheme"
             :type string
             :optional t
             :description
             "Override the configured scheme by name or spec.  Bare \
names: `acme', `uuid', `slug', `counter'.  Specs: `counter GO_0000000' \
(OBO-style template, defines alpha prefix and pad width), `acme slug:t' \
(include label slug; default is the slugless 11-char form).")
      (:name "kind"
             :type string
             :optional t
             :enum ["class" "object-property" "data-property"
                    "annotation-property" "individual" "datatype"]
             :description
             "Entity kind; helps schemes that distinguish (e.g. `acme' \
encodes a single TYPE letter).")))
    ("elot_verify_identifier"
     :function elot-gptel-tool-verify-identifier
     :description
     "Verify that CURIE conforms to the project's identifier scheme.

Useful before adopting an external identifier into the project,
and as a CI check that newly introduced identifiers match the
local convention.  Returns one of:

  OK:   'CURIE' is valid under scheme 'NAME'.
  FAIL: 'CURIE' does not conform to scheme 'NAME'.
  ERROR: <reason>

FILE locates the containing ontology heading and supplies the
default scheme via its `:ELOT-id-scheme:' property.  When SCHEME
is supplied explicitly it overrides that default.  Read-only."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (scopes the verification context).")
      (:name "curie"
             :type string
             :description
             "The CURIE to verify.")
      (:name "scheme"
             :type string
             :optional t
             :description
             "Override the configured scheme by name.")))
    ("elot_axiom_keywords"
     :function elot-gptel-tool-axiom-keywords
     :description
     "Return the legal OMN frame keywords for SUBJECT in FILE.

Read-only authoring helper (Milestone 9 Step 9.2.a).  Resolves
SUBJECT against the buffer's `elot-slurp' (CURIE preferred,
label fallback), reads its `rdf:type', and returns a structured
report with four sections:

  == Frame keywords ==   legal frame keys for the kind
                         (Class: SubClassOf / EquivalentTo /
                         DisjointWith / DisjointUnionOf / HasKey;
                         ObjectProperty: Domain / Range /
                         SubPropertyOf / EquivalentTo /
                         DisjointWith / InverseOf /
                         Characteristics / SubPropertyChain;
                         DataProperty: Domain / Range /
                         SubPropertyOf / Characteristics
                         (Functional only); AnnotationProperty:
                         Domain / Range / SubPropertyOf;
                         Individual: Types / Facts / SameAs /
                         DifferentFrom; Datatype: EquivalentTo)
                         with a one-line shape hint each.
  == Universal annotation rows ==
                         every `owl:AnnotationProperty' declared
                         in the file -- the APs the LLM may use
                         on any subject (`rdfs:comment',
                         `skos:example',
                         `iof-av:naturalLanguageDefinition', ...).
  == Buffer signature ==
                         per-kind CURIE -> label index drawn from
                         `elot-slurp' -- the identifiers the LLM
                         may reference in a fragment without
                         violating the Manchester explicit-
                         signature rule.
  == Existing rows ==    current description-list contents of
                         SUBJECT, so a follow-up edit can target
                         them unambiguously.

Returns an `OK:'-prefixed multi-line report on success, an
`ERROR:'-prefixed single line otherwise.  Never mutates FILE.
Pairs with the future `elot_axiom_check' (9.2.b) and
`elot_edit_axiom' (9.2.c)."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "subject"
             :type string
             :description
             "CURIE (preferred) or rdfs:label of a resource \
declared in FILE.")))
    ("elot_axiom_check"
     :function elot-gptel-tool-axiom-check
     :description
     "Pre-flight validate a candidate `KEYWORD :: FRAGMENT' row on SUBJECT.

Read-only authoring helper (Milestone 9 Step 9.2.b).  The cheap
iteration loop that confirms a proposed description-list row is
well-formed against the buffer's signature and ROBOT's OMN
parser *before* committing it via `elot_edit_axiom' (9.2.c).

Pipeline (fail-fast):
  1. Keyword legality -- KEYWORD must be a frame keyword for
     SUBJECT's `rdf:type' (see `elot_axiom_keywords') OR an
     annotation property declared in FILE.
  2. Synthesise `- KEYWORD :: FRAGMENT' as the last row of
     SUBJECT's description list in an in-memory copy of FILE.
  3. Static checkers (`elot/omn-keyword-appropriateness',
     `elot/axiom-value-curies', `elot/axiom-keyword-range') run
     against the draft and filtered to the synthesised row.
  4. Optional consistency probe (enable via `consistency=true').

No ROBOT call by default: whole-file OMN parse is the
post-commit responsibility of `elot_edit_axiom' (9.2.c) /
`elot_check', so 9.2.b stays a sub-second Emacs-local loop.
Reasoning has no static substitute, so the optional consistency
probe is the only ROBOT-gated stage.

Returns `OK: SUBJECT KEYWORD :: FRAGMENT validates' on full
pass with the list of stages run, `FAIL:' with an actionable
next step on any blocker, or `ERROR:' for malformed input
(unknown subject, etc.).  Never mutates FILE."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "subject"
             :type string
             :description
             "CURIE (preferred) or rdfs:label of a resource \
declared in FILE.")
      (:name "keyword"
             :type string
             :description
             "OMN frame keyword (`SubClassOf', `Domain', `Types', \
...) or a declared annotation property CURIE (`rdfs:comment', \
`skos:definition', ...).")
      (:name "fragment"
             :type string
             :description
             "Proposed value as it would appear on the right of \
`::'.  No leading `- KEYWORD ::' -- just the value text.")
      (:name "consistency"
             :type boolean
             :optional t
             :description
             "When true, also run an in-memory `elot_consistency' \
probe on the synthesised draft.  Costs one ROBOT run; off by \
default to keep the loop cheap.")))
    ("elot_edit_axiom"
     :function elot-gptel-tool-edit-axiom
     :confirm t
     :description
     "Commit a single description-list row `- KEYWORD :: FRAGMENT' on SUBJECT.

Writer counterpart of `elot_axiom_check' (Milestone 9 Step
9.2.c).  Mutates FILE: gated by `elot-gptel-allow-side-effects'.
Caller is expected to have run `elot_axiom_check' first; this
tool trusts its input and does not re-run those static checks.

OP selects the operation:
  - `add' (default) appends `- KEYWORD :: FRAGMENT' as the last
    top-level row of SUBJECT's description list.
  - `replace' rewrites the unique row whose left-hand keyword
    equals KEYWORD (matcher: `match_fragment' wins; otherwise
    the keyword must be unique on SUBJECT) to the new FRAGMENT.
    Nested axiom-annotation children of the row are preserved
    verbatim.
  - `delete' removes the unique row (matcher: `match_fragment'
    when supplied, else FRAGMENT) together with its nested
    annotation children.  FRAGMENT may be omitted.
  - `delete-empty' sweeps every row on SUBJECT whose value is
    empty or whitespace-only and removes them in one atomic
    edit.  When KEYWORD is supplied the sweep is restricted to
    rows with that keyword; pass the empty string (or omit
    KEYWORD) to sweep every keyword on SUBJECT.  FRAGMENT and
    `match_fragment' are ignored.  Zero matches is success
    (idempotent no-op; the file is not touched).

Match resolution refuses on 0 hits (`no row matches') or >1
hits (`ambiguous; N rows match'); the LLM should supply
`match_fragment' to disambiguate.  Targeting an empty-value
row: pass `match_fragment: \"\"' (empty string) on `replace' /
`delete' to match rows whose value is empty or whitespace-only.

Post-commit auto-revalidate runs the standard lint pipeline
(plus OMN parse / consistency when ROBOT is configured).  On
failure the file is rolled back to its pre-edit contents and
the response carries `FAIL:' plus the diagnostic; on success
the response carries `OK:' plus the revalidation report.

Out of scope: editing nested axiom-annotation rows (future
9.2.d), batch / multi-row edits in one call, cross-file
writes."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "subject"
             :type string
             :description
             "CURIE (preferred) or rdfs:label of a resource \
declared in FILE.")
      (:name "keyword"
             :type string
             :description
             "OMN frame keyword (`SubClassOf', `Domain', `Types', \
...) or a declared annotation property CURIE (`rdfs:comment', \
`skos:definition', ...).  Required for `add' / `replace' / \
`delete'; optional for `delete-empty' (empty / omitted = sweep \
all keywords).")
      (:name "fragment"
             :type string
             :optional t
             :description
             "New value (no leading `- KEYWORD ::').  Required \
for `add' / `replace'; optional for `delete' (when supplied, \
also serves as the matcher when `match_fragment' is omitted); \
ignored for `delete-empty'.")
      (:name "op"
             :type string
             :optional t
             :enum ["add" "replace" "delete" "delete-empty"]
             :description
             "Operation to perform (default `add').")
      (:name "match_fragment"
             :type string
             :optional t
             :description
             "For `replace' / `delete': existing-value text of \
the row to target.  Match is ASCII-normalised (whitespace \
runs collapsed, ends trimmed).  Pass the empty string to \
target a row whose value is empty / whitespace-only.  Omit \
when KEYWORD alone is unique on SUBJECT.  Ignored for \
`delete-empty'.")))
    ("elot_edit_axioms"
     :function elot-gptel-tool-edit-axioms
     :confirm t
     :description
     "Commit a BATCH of description-list-row edits to FILE atomically.

Batch counterpart of `elot_edit_axiom' (Milestone 9 Step 9.2.c.1).
Motivating case: two independent OMN axiom errors in the same
file that block each other under the singleton tool (fixing one
in isolation triggers a revalidate rollback because the other
is still broken).  A batch fixes both in the same revalidate
window.

EDITS is an ordered array.  Each element is an object with the
same fields as `elot_edit_axiom' minus `file':
  - `subject'         (required)
  - `op'              (optional, default `add'; one of `add' /
                       `replace' / `delete' / `delete-empty')
  - `keyword'         (required for add/replace/delete; optional
                       for delete-empty -- empty/omitted sweeps
                       every keyword on the subject)
  - `fragment'        (required for `add' / `replace';
                       ignored for `delete-empty')
  - `match_fragment'  (optional matcher for `replace' / `delete';
                       pass \"\" to target an empty-value row;
                       ignored for `delete-empty')

Semantics:
  - Atomic: snapshot -> apply each edit in order to the running
    in-memory draft -> save once -> single post-commit revalidate
    -> rollback on failure.  No per-edit save / revalidate.
  - Per-edit match resolution sees the buffer as the prior
    edits left it, so `replace' in edit 2 resolves against the
    bytes edit 1 produced.
  - Failure at edit K (unknown subject, no match, ambiguous)
    aborts with `FAIL at edits[K]:' and leaves FILE byte-identical
    to its pre-call state.
  - DRY_RUN, when true, runs the full pipeline (apply + lint +
    OMN parse via the read-only `content=' code path) but skips
    the actual `save-buffer'.  Same OK / FAIL envelope; the file
    on disk is unchanged.  Useful for a `try-before-commit' loop.

Mutating tool: gated by `elot-gptel-allow-side-effects' (the
gate is bypassed when DRY_RUN is true).  Out of scope: cross-file
batches, partial-success / continue-on-error, reordering edits."
     :args
     ((:name "file"
             :type string
             :description
             "Path to an ELOT .org file (project-relative).")
      (:name "edits"
             :type array
             :items (:type object)
             :description
             "Ordered array of edit objects.  Each object: \
{subject, op?, keyword, fragment?, match_fragment?}.  Same \
shape as `elot_edit_axiom' minus `file'.")
      (:name "dry_run"
             :type boolean
             :optional t
             :description
             "When true, run the full pipeline (apply + revalidate \
via `content=') but skip the actual save.  File on disk \
unchanged; useful for try-before-commit.  Default false."))))
"Each entry is (NAME . PLIST) where PLIST is forwarded to
`gptel-make-tool' after light translation.")

(defun elot-gptel--tool-thunk (fn)
  "Return a lambda dispatching gptel's positional args to FN.
gptel calls the tool function with positional arguments in the
order declared by `:args' (see `gptel-make-tool' docs).  Map the
known tool symbols to a lambda with the matching arity."
  (pcase fn
    ('elot-gptel-tool-conventions
     (lambda () (elot-gptel-tool-conventions)))
    ('elot-gptel-tool-check
     (lambda (file &optional content profile reasoner)
       (elot-gptel-tool-check file content profile reasoner)))
    ('elot-gptel-tool-lint
     (lambda (file &optional severity categories content)
       (elot-gptel-tool-lint file severity categories content)))
    ('elot-gptel-tool-omn-validate
     (lambda (file &optional profile content)
       (elot-gptel-tool-omn-validate file profile content)))
    ('elot-gptel-tool-omn-report
     (lambda (file &optional format content)
       (elot-gptel-tool-omn-report file format content)))
    ('elot-gptel-tool-diff
     (lambda (file baseline &optional format)
       (elot-gptel-tool-diff file baseline format)))
    ('elot-gptel-tool-sparql
     (lambda (file query &optional format limit)
       (elot-gptel-tool-sparql file query format limit)))
    ('elot-gptel-tool-sparql-select
     (lambda (file query &optional format limit)
       (elot-gptel-tool-sparql-select file query format limit)))
    ('elot-gptel-tool-unsatisfiable
     (lambda (file &optional reasoner)
       (elot-gptel-tool-unsatisfiable file reasoner)))
    ('elot-gptel-tool-consistency
     (lambda (file &optional reasoner)
       (elot-gptel-tool-consistency file reasoner)))
    ('elot-gptel-tool-explain
     (lambda (file axiom &optional reasoner max)
       (elot-gptel-tool-explain file axiom reasoner max)))
    ('elot-gptel-tool-db-query
     (lambda (sql &optional limit)
       (elot-gptel-tool-db-query sql limit)))
    ('elot-gptel-tool-db-schema
     (lambda () (elot-gptel-tool-db-schema)))
    ('elot-gptel-tool-db-get-label
     (lambda (token) (elot-gptel-tool-db-get-label token)))
    ('elot-gptel-tool-db-list-sources
     (lambda () (elot-gptel-tool-db-list-sources)))
    ('elot-gptel-tool-db-activate-source
     (lambda (file) (elot-gptel-tool-db-activate-source file)))
    ('elot-gptel-tool-db-expand-curie
     (lambda (curie) (elot-gptel-tool-db-expand-curie curie)))
    ('elot-gptel-tool-db-remove-source
     (lambda (source &optional data-source like allow-all dry-run)
       (elot-gptel-tool-db-remove-source
        source data-source like allow-all dry-run)))
    ('elot-gptel-tool-db-get-attributes
     (lambda (id &optional source)
       (elot-gptel-tool-db-get-attributes id source)))
    ('elot-gptel-tool-db-supertypes
     (lambda (id &optional source)
       (elot-gptel-tool-db-supertypes id source)))
    ('elot-gptel-tool-db-individual-types
     (lambda (id &optional source)
       (elot-gptel-tool-db-individual-types id source)))
    ('elot-gptel-tool-db-search-label
     (lambda (query &optional kind source lang limit exact-only)
       (elot-gptel-tool-db-search-label
        query kind source lang limit exact-only)))
    ('elot-gptel-tool-db-borrow-term
     (lambda (token) (elot-gptel-tool-db-borrow-term token)))
    ('elot-gptel-tool-borrow-term
     (lambda (label &optional kind source lang)
       (elot-gptel-tool-borrow-term label kind source lang)))
    ('elot-gptel-tool-mint-identifier
     (lambda (file label &optional scheme kind)
       (elot-gptel-tool-mint-identifier file label scheme kind)))
    ('elot-gptel-tool-verify-identifier
     (lambda (file curie &optional scheme)
       (elot-gptel-tool-verify-identifier file curie scheme)))
    ('elot-gptel-tool-axiom-keywords
     (lambda (file subject)
       (elot-gptel-tool-axiom-keywords file subject)))
    ('elot-gptel-tool-axiom-check
     (lambda (file subject keyword fragment &optional consistency)
       (elot-gptel-tool-axiom-check file subject keyword fragment
                                    consistency)))
    ('elot-gptel-tool-edit-axiom
     (lambda (file subject keyword &optional fragment op match-fragment)
       (elot-gptel-tool-edit-axiom file subject keyword fragment
                                   op match-fragment)))
    ('elot-gptel-tool-edit-axioms
     (lambda (file edits &optional dry-run)
       (elot-gptel-tool-edit-axioms file edits dry-run)))
    ('elot-gptel-tool-rename-resource
     (lambda (file source target &optional ontology target-iri new-label op)
       (elot-gptel-tool-rename-resource
        file source target ontology target-iri new-label op)))
    ('elot-gptel-tool-move-resource
     (lambda (file source target &optional as)
       (elot-gptel-tool-move-resource file source target as)))
    ('elot-gptel-tool-replace-with-parent
     (lambda (file subject &optional parent dry-run)
       (elot-gptel-tool-replace-with-parent file subject parent dry-run)))
    ('elot-gptel-tool-delete-resource
     (lambda (file subject &optional cascade dry-run)
       (elot-gptel-tool-delete-resource file subject cascade dry-run)))
    ('elot-gptel-tool-insert-sibling-resource
     (lambda (file anchor labels)
       (elot-gptel-tool-insert-sibling-resource file anchor labels)))
    ('elot-gptel-tool-insert-child-resource
     (lambda (file anchor labels)
       (elot-gptel-tool-insert-child-resource file anchor labels)))
    ('elot-gptel-tool-insert-resource-tree
     (lambda (file anchor tree &optional as)
       (elot-gptel-tool-insert-resource-tree file anchor tree as)))
    (_ (error "elot-gptel: no dispatcher for %S" fn))))

(defun elot-gptel--confirm-effective-p (spec-confirm)
  "Return non-nil if the gptel `:confirm t' flag should be set.
SPEC-CONFIRM is the value of `:confirm' read from the tool
spec.  Consults `elot-gptel-confirm-mutations' to decide whether
the spec-level value is honoured, suppressed, or honoured
conditionally on `elot-gptel-allow-side-effects'.  Specs that
omit `:confirm' (i.e. SPEC-CONFIRM is nil) always remain
non-confirming -- this helper only ever /downgrades/ a
spec-level `:confirm t', never adds one."
  (and spec-confirm
       (pcase elot-gptel-confirm-mutations
         ('always     t)
         ('never      nil)
         ('once-armed (not elot-gptel-allow-side-effects))
         (_           t))))

(defun elot-gptel--register-one (spec)
  "Register a single SPEC via `gptel-make-tool' and return (NAME . TOOL)."
  (unless (featurep 'gptel)
    (user-error "elot-gptel: gptel is not loaded"))
  (let* ((name (car spec))
         (plist (cdr spec))
         (fn (plist-get plist :function))
         (description (plist-get plist :description))
         (args (plist-get plist :args))
         (confirm (elot-gptel--confirm-effective-p
                   (plist-get plist :confirm)))
         (tool
          (apply #'gptel-make-tool
                 :name        name
                 :description description
                 :args        args
                 :category    "elot"
                 :function    (elot-gptel--tool-thunk fn)
                 (append (and confirm (list :confirm t)) nil))))
    (cons name tool)))

(defun elot-gptel--refresh-active-selection ()
  "Replace any stale ELOT tool objects currently in `gptel-tools'.
`gptel-make-tool' updates the global registry, but `gptel-tools'
\(the per-session selection driven by `gptel-menu') still points
at the previous tool struct.  Walk `gptel-tools' and swap any
ELOT entry for the freshly-registered one of the same name, so
re-registration takes effect without the user toggling the tool
in `gptel-menu'."
  (when (and (boundp 'gptel-tools) (listp gptel-tools))
    (setq gptel-tools
          (mapcar
           (lambda (tool)
             (let* ((name (ignore-errors (gptel-tool-name tool)))
                    (fresh (and name
                                (cdr (assoc name elot-gptel--tools)))))
               (or fresh tool)))
           gptel-tools))))

;;;###autoload
(defun elot-gptel-register-tools ()
  "Register ELOT's gptel tools.
Errors with a helpful message when gptel is not available.  Safe
to call repeatedly: re-registering replaces any previously
registered ELOT tools, and refreshes any ELOT entries already
selected in `gptel-tools' so the new definitions take effect
without toggling in `gptel-menu'."
  (interactive)
  (unless (require 'gptel nil 'noerror)
    (user-error
     "elot-gptel: gptel is not installed; M-x package-install RET gptel"))
  (elot-gptel-unregister-tools)
  (setq elot-gptel--tools
        (mapcar #'elot-gptel--register-one elot-gptel--tool-specs))
  (elot-gptel--refresh-active-selection)
  (when (called-interactively-p 'any)
    (message "elot-gptel: registered %d tool%s"
             (length elot-gptel--tools)
             (if (= (length elot-gptel--tools) 1) "" "s")))
  elot-gptel--tools)

(defun elot-gptel-unregister-tools ()
  "Remove ELOT's gptel tools from the registry.
Idempotent.  Primarily used by the test suite and by
`elot-gptel-register-tools' before a re-registration."
  (interactive)
  (when (and (featurep 'gptel) elot-gptel--tools)
    (dolist (entry elot-gptel--tools)
      (let ((name (car entry)))
        ;; gptel does not (yet) export a stable removal API; clear our
        ;; cache and let the next registration overwrite by name.
        (ignore name))))
  (setq elot-gptel--tools nil))

(provide 'elot-gptel)
;;; elot-gptel.el ends here
