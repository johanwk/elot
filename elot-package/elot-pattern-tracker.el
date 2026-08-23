;;; elot-pattern-tracker.el --- Pattern instantiation progress tracker  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ELOT contributors

;; Author: ELOT
;; Keywords: ontology, org

;; This file is part of ELOT.

;;; Commentary:

;; While an LLM or a human applies one ELOT modelling pattern to a
;; target ontology file, a small progress outline is kept inside that
;; file.  This file implements the first part of that feature: turning
;; a pattern file into the trimmed outline text that the tracker uses.
;;
;; Trimming works on the parsed headline hierarchy that
;; `elot-update-headline-hierarchy' produces, never on regular
;; expressions over the pattern text.  The rules are:
;;
;; 1. Take the pattern's single ontology outline.
;; 2. Drop every `:nodeclare:' heading and its own contents, but keep
;;    its subheadings at their original outline levels.
;; 3. Drop every description-list row.  The tracker is a checklist,
;;    not a copy of the pattern: a reader must consult the pattern
;;    file itself for the rules, and a copied row would go stale.
;; 4. Keep only resource headings that carry a `pattern:action' row,
;;    plus any heading needed to hold such a resource, with its
;;    original nesting.  Resources without an action are part of the
;;    pattern file's own bookkeeping, not work to be tracked.
;; 5. Keep standard section headings, but omit their copied `ID'
;;    properties.
;;
;; The prefix table heading and the ontology declaration heading are
;; not copied: neither is a resource to be applied, and a prefix table
;; cannot be represented in the parsed hierarchy.
;;
;; Tracker insertion, status and state changes are added later; this
;; file only produces the trimmed outline.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)

(declare-function elot-update-headline-hierarchy "elot-tangle" ())
(defvar elot-headline-hierarchy)

(defconst elot-pattern-tracker-root-level 1
  "Outline level of the tracker root heading.")


;;; Reading a pattern file

(defun elot-pattern-tracker--file-hierarchy (file)
  "Return a freshly parsed headline hierarchy for pattern FILE.
FILE is visited in a temporary buffer, so no user buffer state is
touched and no stale cache can be used."
  (unless (file-readable-p file)
    (error "Pattern file not readable: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    ;; `elot-update-headline-hierarchy' and the ELOT parsing helpers
    ;; expect a real Org buffer with a file name.
    (setq buffer-file-name (expand-file-name file))
    (unwind-protect
        (progn
          (let ((org-inhibit-startup t))
            (org-mode))
          (elot-update-headline-hierarchy)
          elot-headline-hierarchy)
      (setq buffer-file-name nil))))

(defun elot-pattern-tracker--ontology-node (hierarchy)
  "Return the single ontology node of HIERARCHY.
Signals an error unless exactly one top-level node declares
`ELOT-context-type' equal to \"ontology\"."
  (let ((found (seq-filter
                (lambda (node)
                  (equal (plist-get node :elot-context-type) "ontology"))
                (plist-get hierarchy :children))))
    (cond
     ((null found)
      (error "No ontology outline found in pattern"))
     ((cdr found)
      (error "Pattern has %d ontology outlines; exactly one is required"
             (length found)))
     (t (car found)))))


;;; Trimming

(defun elot-pattern-tracker--nodeclare-p (node)
  "Return non-nil when NODE is tagged `:nodeclare:'."
  (and (member "nodeclare" (plist-get node :tags)) t))

(defun elot-pattern-tracker--skip-p (node)
  "Return non-nil when NODE is copied neither as section nor resource.
This covers the prefix table heading and the ontology declaration
heading, which are part of the pattern file but not part of the
work to be tracked."
  (let ((id (plist-get node :id)))
    (or (plist-get node :prefixdefs)
        (and (stringp id)
             (string-suffix-p "-ontology-declaration" id)))))

(defun elot-pattern-tracker--action-p (node)
  "Return non-nil when NODE carries a `pattern:action' row."
  (and (assoc "pattern:action" (plist-get node :descriptions)) t))

(defun elot-pattern-tracker--trim-node (node)
  "Return the trimmed copy of NODE as a list of tracker nodes.
A `:nodeclare:' heading contributes no node of its own; its
trimmed children are returned instead, keeping their original
outline levels."
  (let ((children (apply #'append
                         (mapcar #'elot-pattern-tracker--trim-node
                                 (plist-get node :children)))))
    (cond
     ((elot-pattern-tracker--nodeclare-p node) children)
     ((elot-pattern-tracker--skip-p node) children)
     ;; A resource heading is tracked only when it says what to do.
     ;; Keep an untracked heading only when it holds tracked ones.
     ((and (plist-get node :uri)
           (not (elot-pattern-tracker--action-p node)))
      children)
     ((and (null (plist-get node :uri)) (null children)) nil)
     (t
      (list (list :level (plist-get node :level)
                  :title (plist-get node :title)
                  :uri (plist-get node :uri)
                  :tags (plist-get node :tags)
                  :resource (and (plist-get node :uri) t)
                  :children children))))))

(defun elot-pattern-tracker-trim (ontology-node)
  "Return the trimmed child nodes of ONTOLOGY-NODE.
The ontology root itself is not copied: the caller replaces it
with the tracker root heading."
  (apply #'append
         (mapcar #'elot-pattern-tracker--trim-node
                 (plist-get ontology-node :children))))


;;; Rendering

(defun elot-pattern-tracker--render-node (node states)
  "Return NODE and its subtree as tracker outline text.
STATES is an alist mapping a node URI to its TODO state string, or
nil for no state.  Headings without a URI never receive a state."
  (let* ((uri (plist-get node :uri))
         (state (and (plist-get node :resource)
                     (cdr (assoc uri states))))
         (tags (seq-remove (lambda (tag) (equal tag "nodeclare"))
                           (plist-get node :tags))))
    (concat (make-string (plist-get node :level) ?*)
            " "
            (if state (concat state " ") "")
            (plist-get node :title)
            (if tags (concat "  :" (string-join tags ":") ":") "")
            "\n"
            (mapconcat (lambda (child)
                         (elot-pattern-tracker--render-node child states))
                       (plist-get node :children)
                       ""))))

(defun elot-pattern-tracker-render (nodes states name target pattern-file)
  "Return the whole tracker outline text.
NODES are trimmed nodes, STATES an alist of URI to state string,
NAME the tracker title, TARGET the target ontology `ID', and
PATTERN-FILE the pattern's file name as it should be recorded."
  (concat (make-string elot-pattern-tracker-root-level ?*)
          " Pattern tracker: " name "\n"
          ":PROPERTIES:\n"
          ":ELOT-context-type: pattern-tracker\n"
          ":ELOT-pattern-target: " target "\n"
          ":ELOT-pattern-file: " pattern-file "\n"
          ":header-args:omn: :tangle no\n"
          ":END:\n"
          elot-pattern-tracker-keyword-line "\n\n"
          (mapconcat (lambda (node)
                       (elot-pattern-tracker--render-node node states))
                     nodes
                     "")))

;;; Progress states

(defconst elot-pattern-tracker-cancelled-tag "cancelled"
  "Tag put on the top heading of a leg that is not applied.")

(defconst elot-pattern-tracker-keyword-line "#+TODO: TODO STARTED | DONE"
  "Keyword line written inside the tracker subtree.
STARTED means the heading exists but its axioms are still
missing.  The line sits inside the tracker, so removing the
tracker leaves no trace of it in the file.")

(defun elot-pattern-tracker--assign-states (nodes cancelled &optional present)
  "Return an alist mapping resource URI to its TODO state.
NODES are trimmed tracker nodes.  CANCELLED is a list of resource
URIs that are not applied; they get `DONE'.  PRESENT is an alist
mapping a resource URI already declared in the target file to
non-nil when that declaration also carries an `rdfs:isDefinedBy'
row.  A present and cited resource gets `DONE', a present but
uncited one `STARTED', and everything else `TODO'.  Cancelling
wins over being present.

Cancelling does not cascade: outline nesting is the pattern's own
class or property hierarchy, not a work breakdown.  A leg may well
need a subclass while its superclass is not wanted, so each
resource is judged on its own."
  (let (states)
    (cl-labels
        ((walk (node)
               (let ((uri (plist-get node :uri)))
                 (when (plist-get node :resource)
                   (push (cons uri
                               (cond
                                ((member uri cancelled) "DONE")
                                ((assoc uri present)
                                 (if (cdr (assoc uri present))
                                     "DONE" "STARTED"))
                                (t "TODO")))
                         states))
                 (dolist (child (plist-get node :children))
                   (walk child)))))
      (dolist (node nodes) (walk node)))
    (nreverse states)))

(defun elot-pattern-tracker--tag-cancelled (nodes cancelled)
  "Return NODES with the cancelled tag added to each URI in CANCELLED.
Only the named resources are tagged; descendants are untouched."
  (mapcar
   (lambda (node)
     (let ((tags (plist-get node :tags)))
       (append
        (list :level (plist-get node :level)
              :title (plist-get node :title)
              :uri (plist-get node :uri)
              :tags (if (and (plist-get node :resource)
                             (member (plist-get node :uri) cancelled))
                        (append tags (list elot-pattern-tracker-cancelled-tag))
                      tags)
              :resource (plist-get node :resource)
              :children (elot-pattern-tracker--tag-cancelled
                         (plist-get node :children) cancelled)))))
   nodes))


;;; Building the tracker text for a target file

(defun elot-pattern-tracker--pattern-name (file)
  "Return the tracker title for pattern FILE."
  (file-name-base file))

(defun elot-pattern-tracker--ontology-id (file)
  "Return the ontology heading `ID' of ELOT target FILE.
Falls back to the ontology local name, then to the file base name."
  (let* ((hierarchy (elot-pattern-tracker--file-hierarchy file))
         (ontology (ignore-errors
                     (elot-pattern-tracker--ontology-node hierarchy))))
    (or (and ontology
             (seq-some (lambda (child) (plist-get child :id))
                       (plist-get ontology :children)))
        (and ontology (plist-get ontology :elot-context-localname))
        (file-name-base file))))

(defun elot-pattern-tracker--target-declared (target-file)
  "Return an alist of resource URI to cited flag for TARGET-FILE.
Every resource heading declared in TARGET-FILE contributes one
entry; the value is non-nil when the heading carries an
`rdfs:isDefinedBy' row.  Returns nil when the file cannot be
read or parsed."
  (condition-case nil
      (let ((hierarchy (elot-pattern-tracker--file-hierarchy target-file))
            found)
        (cl-labels
            ((walk (node)
                   (let ((uri (plist-get node :uri)))
                     (when uri
                       (push (cons uri
                                   (and (assoc "rdfs:isDefinedBy"
                                               (plist-get node :descriptions))
                                        t))
                             found)))
                   (dolist (child (plist-get node :children))
                     (walk child))))
          (dolist (child (plist-get hierarchy :children)) (walk child)))
        (nreverse found))
    (error nil)))

(defun elot-pattern-tracker-text (pattern-file target-file &optional cancelled target-id)
  "Return the tracker outline text for PATTERN-FILE and TARGET-FILE.
CANCELLED is a list of resource URIs that are not applied.
TARGET-ID overrides the target ontology heading `ID'.

A pattern resource whose CURIE is already declared in TARGET-FILE
starts at `DONE' when that declaration carries an
`rdfs:isDefinedBy' row, and at `STARTED' when it does not."
  (let* ((hierarchy (elot-pattern-tracker--file-hierarchy pattern-file))
         (ontology (elot-pattern-tracker--ontology-node hierarchy))
         (nodes (elot-pattern-tracker-trim ontology))
         (present (and (file-readable-p target-file)
                       (elot-pattern-tracker--target-declared target-file)))
         (states (elot-pattern-tracker--assign-states nodes cancelled present))
         (tagged (elot-pattern-tracker--tag-cancelled nodes cancelled)))
    (elot-pattern-tracker-render
     tagged states
     (elot-pattern-tracker--pattern-name pattern-file)
     (or target-id (elot-pattern-tracker--ontology-id target-file))
     (file-relative-name pattern-file (file-name-directory target-file)))))

(defun elot-pattern-tracker-resource-uris (pattern-file)
  "Return the resource URIs a tracker for PATTERN-FILE would contain."
  (let* ((hierarchy (elot-pattern-tracker--file-hierarchy pattern-file))
         (ontology (elot-pattern-tracker--ontology-node hierarchy))
         (nodes (elot-pattern-tracker-trim ontology)))
    (mapcar #'car (elot-pattern-tracker--assign-states nodes nil))))


;;; Finding a tracker in a target buffer

(defconst elot-pattern-tracker--marker ":ELOT-context-type: pattern-tracker"
  "Property line that identifies a tracker heading.")

(defun elot-pattern-tracker--find-in-buffer ()
  "Return the position of the tracker heading, or nil when absent.
Point is not moved."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward elot-pattern-tracker--marker nil t)
      (org-back-to-heading t)
      (point))))

(defun elot-pattern-tracker--first-heading-position ()
  "Return the position of the first Org heading, or `point-max'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\*+ " nil t)
        (line-beginning-position)
      (point-max))))


;;; Commands

;;;###autoload
(defun elot-pattern-tracker-insert (pattern-file &optional target-file cancelled target-id)
  "Insert a progress tracker for PATTERN-FILE into TARGET-FILE.
TARGET-FILE defaults to the file of the current buffer.  CANCELLED
is a list of resource URIs that are not applied; each of them is
marked DONE and tagged `:cancelled:'.  Cancelling one resource
does not cancel the resources nested under it.

The tracker is put immediately before the first Org heading, so
file-wide keywords and other preamble text stay above it.  The
file is saved at once.  Refuses when the file already holds a
tracker."
  (interactive
   (list (read-file-name "Pattern file: " nil nil t)
         (or (buffer-file-name)
             (read-file-name "Target ELOT file: " nil nil t))
         nil))
  (let* ((target (expand-file-name (or target-file (buffer-file-name))))
         (buffer (find-file-noselect target)))
    (with-current-buffer buffer
      (when (elot-pattern-tracker--find-in-buffer)
        (user-error "%s already holds a pattern tracker" (file-name-nondirectory target)))
      (let ((text (elot-pattern-tracker-text (expand-file-name pattern-file)
                                             target cancelled target-id)))
        (save-excursion
          (goto-char (elot-pattern-tracker--first-heading-position))
          (insert text "\n")))
      (save-buffer))
    (when (called-interactively-p 'any)
      (message "Pattern tracker inserted in %s" (file-name-nondirectory target)))
    target))

;;;###autoload
(defun elot-pattern-tracker-delete (&optional target-file)
  "Remove the pattern tracker outline from TARGET-FILE and save.
TARGET-FILE defaults to the file of the current buffer.  Refuses
when the file holds no tracker."
  (interactive (list (or (buffer-file-name)
                         (read-file-name "Target ELOT file: " nil nil t))))
  (let* ((target (expand-file-name (or target-file (buffer-file-name))))
         (buffer (find-file-noselect target)))
    (with-current-buffer buffer
      (let ((start (elot-pattern-tracker--find-in-buffer)))
        (unless start
          (user-error "%s holds no pattern tracker" (file-name-nondirectory target)))
        (save-excursion
          (goto-char start)
          (let* ((level (save-excursion
                          (goto-char start)
                          (skip-chars-forward "*")
                          (- (point) start)))
                 (end (save-excursion
                        (goto-char start)
                        (forward-line 1)
                        (if (re-search-forward
                             (format "^\\*\\{1,%d\\}[ \t]" level) nil t)
                            (line-beginning-position)
                          (point-max)))))
            (delete-region start end)
            ;; Leave no extra blank lines behind.
            (goto-char start)
            (while (looking-at "^[ \t]*$")
              (delete-region (point) (min (point-max) (1+ (line-end-position)))))))
        (save-buffer)))
    (when (called-interactively-p 'any)
      (message "Pattern tracker removed from %s" (file-name-nondirectory target)))
    target))

;;; Reading the states of an embedded tracker

(defun elot-pattern-tracker--heading-uri (title)
  "Return the resource URI carried by heading TITLE, or nil.
A resource heading is either `Label (curie)' or a bare CURIE."
  (cond
   ((null title) nil)
   ((string-match "(\\([^()]+\\))[ \t]*\\'" title)
    (match-string 1 title))
   ((string-match-p "\\`[^ \t]+:[^ \t]+\\'" title) title)
   (t nil)))

(defun elot-pattern-tracker--tracker-end (start)
  "Return the end position of the tracker subtree beginning at START."
  (save-excursion
    (goto-char start)
    (let ((level (progn (skip-chars-forward "*") (- (point) start))))
      (forward-line 1)
      (if (re-search-forward (format "^\\*\\{1,%d\\}[ \t]" level) nil t)
          (line-beginning-position)
        (point-max)))))

(defun elot-pattern-tracker--entries ()
  "Return the resource entries of the tracker in the current buffer.
Each entry is a plist with `:uri', `:title', `:state', `:tags' and
`:pos'.  Returns nil when the buffer holds no tracker."
  (let ((start (elot-pattern-tracker--find-in-buffer)))
    (when start
      (let ((end (elot-pattern-tracker--tracker-end start))
            entries)
        (save-excursion
          (goto-char start)
          (forward-line 1)
          (while (re-search-forward "^\\*+[ \t]" end t)
            (let* ((pos (line-beginning-position))
                   (line (buffer-substring-no-properties
                          pos (line-end-position)))
                   state tags title)
              (when (string-match
                     "\\`\\(\\*+\\)[ \t]+\\(?:\\(TODO\\|STARTED\\|DONE\\)[ \t]+\\)?\\(.*?\\)\\(?:[ \t]+:\\([[:alnum:]_@#%:]+\\):\\)?[ \t]*\\'"
                     line)
                (setq state (match-string 2 line)
                      title (match-string 3 line)
                      tags (let ((s (match-string 4 line)))
                             (and s (split-string s ":" t)))))
              (let ((uri (elot-pattern-tracker--heading-uri title)))
                (when uri
                  (push (list :uri uri :title title :state state
                              :tags tags :pos pos)
                        entries)))))
          (nreverse entries))))))

(defun elot-pattern-tracker--buffer (target-file)
  "Return a buffer visiting TARGET-FILE."
  (find-file-noselect
   (expand-file-name (or target-file (buffer-file-name)))))

(defun elot-pattern-tracker--root-property (name)
  "Return property NAME of the tracker root, or nil."
  (let ((start (elot-pattern-tracker--find-in-buffer)))
    (when start
      (save-excursion
        (goto-char start)
        (org-entry-get (point) name)))))

;;;###autoload
(defun elot-pattern-tracker-status (&optional target-file)
  "Return the state of the tracker in TARGET-FILE as a plist.
The plist has `:pattern', `:target', `:todo', `:started',
`:done' and `:cancelled', the last four being lists of resource
URIs.  STARTED means the heading exists but its axioms are still
missing, so it counts as outstanding work.
Returns nil when TARGET-FILE holds no tracker."
  (interactive (list (or (buffer-file-name)
                         (read-file-name "Target ELOT file: " nil nil t))))
  (with-current-buffer (elot-pattern-tracker--buffer target-file)
    (let ((entries (elot-pattern-tracker--entries)))
      (when entries
        (let (todo started done cancelled)
          (dolist (entry entries)
            (let ((uri (plist-get entry :uri))
                  (state (plist-get entry :state)))
              (cond
               ((member elot-pattern-tracker-cancelled-tag
                        (plist-get entry :tags))
                (push uri cancelled))
               ((equal state "DONE") (push uri done))
               ((equal state "STARTED") (push uri started))
               ((equal state "TODO") (push uri todo)))))
          (let ((result
                 (list :pattern (elot-pattern-tracker--root-property
                                 "ELOT-pattern-file")
                       :target (elot-pattern-tracker--root-property
                                "ELOT-pattern-target")
                       :todo (nreverse todo)
                       :started (nreverse started)
                       :done (nreverse done)
                       :cancelled (nreverse cancelled))))
            (when (called-interactively-p 'any)
              (message "Tracker %s: %d TODO, %d STARTED, %d DONE, %d cancelled"
                       (plist-get result :target)
                       (length (plist-get result :todo))
                       (length (plist-get result :started))
                       (length (plist-get result :done))
                       (length (plist-get result :cancelled))))
            result))))))

(defun elot-pattern-tracker-todo-count (&optional target-file)
  "Return the number of headings still open in TARGET-FILE's tracker.
TODO and STARTED headings are counted together, so a caller has
only one number to read.  Returns nil when there is no tracker."
  (let ((status (elot-pattern-tracker-status target-file)))
    (and status (+ (length (plist-get status :todo))
                   (length (plist-get status :started))))))


;;; Changing states

(defun elot-pattern-tracker--normalize-specs (specs state)
  "Return SPECS as an alist of URI to state.
SPECS is a list of URIs, or a list of (URI . STATE) pairs, or a
list of (URI STATE) lists.  STATE is the state used for bare
URIs."
  (mapcar
   (lambda (spec)
     (cond
      ((stringp spec)
       (unless state
         (error "No state given for %s" spec))
       (cons spec state))
      ((and (consp spec) (stringp (car spec)) (stringp (cdr spec))) spec)
      ((and (consp spec) (stringp (car spec)) (consp (cdr spec)))
       (cons (car spec) (cadr spec)))
      (t (error "Bad resource specification: %S" spec))))
   specs))

(defconst elot-pattern-tracker-states
  '("TODO" "STARTED" "DONE" "DONE :cancelled:")
  "States a tracker heading may be given.")

(defun elot-pattern-tracker--split-state (state)
  "Return (KEYWORD . CANCELLED) for STATE.
STATE is \"TODO\", \"STARTED\", \"DONE\", \"DONE :cancelled:\" or
\"cancelled\"."
  (cond
   ((equal state "TODO") (cons "TODO" nil))
   ((equal state "STARTED") (cons "STARTED" nil))
   ((equal state "DONE") (cons "DONE" nil))
   ((member state '("DONE :cancelled:" "cancelled" ":cancelled:"))
    (cons "DONE" t))
   (t (error "Unknown tracker state: %s" state))))

(defun elot-pattern-tracker--write-heading (entry keyword cancelled)
  "Rewrite the heading of ENTRY with KEYWORD and the cancelled tag.
CANCELLED non-nil adds the tag, nil removes it."
  (save-excursion
    (goto-char (plist-get entry :pos))
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
           (stars (progn (string-match "\\`\\*+" line)
                         (match-string 0 line)))
           (tags (seq-remove
                  (lambda (tag) (equal tag elot-pattern-tracker-cancelled-tag))
                  (plist-get entry :tags)))
           (tags (if cancelled
                     (append tags (list elot-pattern-tracker-cancelled-tag))
                   tags))
           (new (concat stars " "
                        (if keyword (concat keyword " ") "")
                        (plist-get entry :title)
                        (if tags
                            (concat "  :" (string-join tags ":") ":")
                          ""))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert new))))

;;;###autoload
(defun elot-pattern-tracker-set-states (specs &optional state target-file)
  "Set the states of several tracker headings in TARGET-FILE at once.
SPECS is a list of resource URIs, in which case STATE is used for
all of them, or a list of (URI . STATE) pairs.

All changes are applied or none: when any URI is absent from the
tracker, the whole operation is refused and the missing URIs are
reported.  A state is \"TODO\", \"STARTED\", \"DONE\" or
\"DONE :cancelled:\".  The file is saved once, after all changes.

Returns a list of (URI OLD NEW) for the headings that changed."
  (let* ((pairs (elot-pattern-tracker--normalize-specs specs state))
         (buffer (elot-pattern-tracker--buffer target-file)))
    (with-current-buffer buffer
      (let ((entries (elot-pattern-tracker--entries)))
        (unless entries
          (user-error "%s holds no pattern tracker"
                      (file-name-nondirectory (buffer-file-name))))
        (let ((missing (seq-remove
                        (lambda (pair)
                          (seq-find (lambda (e)
                                      (equal (plist-get e :uri) (car pair)))
                                    entries))
                        pairs)))
          (when missing
            (user-error "Not in the tracker: %s"
                        (string-join (mapcar #'car missing) ", "))))
        (let (changed)
          ;; Work from the end of the buffer, so earlier positions stay valid.
          (dolist (pair (sort (copy-sequence pairs)
                              (lambda (a b)
                                (> (plist-get
                                    (seq-find (lambda (e)
                                                (equal (plist-get e :uri) (car a)))
                                              entries)
                                    :pos)
                                   (plist-get
                                    (seq-find (lambda (e)
                                                (equal (plist-get e :uri) (car b)))
                                              entries)
                                    :pos)))))
            (let* ((entry (seq-find (lambda (e)
                                      (equal (plist-get e :uri) (car pair)))
                                    entries))
                   (split (elot-pattern-tracker--split-state (cdr pair)))
                   (old (concat (or (plist-get entry :state) "")
                                (if (member elot-pattern-tracker-cancelled-tag
                                            (plist-get entry :tags))
                                    " :cancelled:" "")))
                   (new (concat (car split)
                                (if (cdr split) " :cancelled:" ""))))
              (elot-pattern-tracker--write-heading entry (car split) (cdr split))
              (unless (equal (string-trim old) (string-trim new))
                (push (list (car pair) (string-trim old) new) changed))))
          (save-buffer)
          (nreverse changed))))))

;;; Notes on tracker headings

(defun elot-pattern-tracker-add-note (uri note &optional target-file)
  "Add NOTE as plain text under the tracker heading for URI.
The note is put at the end of that heading's own body, before any
subheading.  Returns non-nil when a note was written."
  (with-current-buffer (elot-pattern-tracker--buffer target-file)
    (let* ((entries (elot-pattern-tracker--entries))
           (entry (seq-find (lambda (e) (equal (plist-get e :uri) uri))
                            entries)))
      (when (and entry note (not (string-empty-p (string-trim note))))
        (save-excursion
          (goto-char (plist-get entry :pos))
          (forward-line 1)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\*+[ \t]" nil t)
                           (line-beginning-position)
                         (point-max)))))
            (goto-char end)
            ;; Step back over trailing blank lines.
            (skip-chars-backward " \t\n")
            (unless (bolp) (forward-line 1))
            (insert (string-trim note) "\n")))
        t))))

(provide 'elot-pattern-tracker)

;;; elot-pattern-tracker.el ends here
