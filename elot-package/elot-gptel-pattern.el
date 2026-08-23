;;; elot-gptel-pattern.el --- gptel tool for the ELOT pattern tracker  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ELOT contributors

;; Author: ELOT
;; Keywords: ontology, org, tools

;; This file is part of ELOT.

;;; Commentary:

;; One public gptel tool, `elot_pattern_tracker', with four actions:
;; `create', `status', `set_state' and `delete'.  The actions are small
;; internal functions behind a thin dispatcher; only the one tool is
;; registered.
;;
;; The tool never lets a signal escape: every result is a plain string
;; beginning with `OK:', `FAIL:' or `ERROR:'.  When the caller's intent
;; already holds (delete with no tracker, create with the same tracker
;; already there) the result is `OK:' with a note that nothing was
;; done.  A conflict that would lose work or mix two patterns is
;; `FAIL:'.  A genuine fault is `ERROR:'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'elot-pattern-tracker)

(declare-function elot-gptel--resolve-file "elot-gptel")
(declare-function elot-gptel--resolve-file-path "elot-gptel")
(declare-function elot-gptel--truthy "elot-gptel")
(defvar elot-gptel-allow-side-effects)
(defvar elot-gptel--arg-file)


;;;; Helpers

(defun elot-gptel-pattern--seq (v)
  "Return V as a list of strings, accepting a vector, list, or string."
  (cond
   ((null v) nil)
   ((stringp v) (list v))
   ((vectorp v) (append v nil))
   ((listp v) v)
   (t nil)))

(defun elot-gptel-pattern--require-gate ()
  "Signal a `user-error' when side effects are not allowed."
  (unless elot-gptel-allow-side-effects
    (user-error
     "side effects are disabled (set `elot-gptel-allow-side-effects' to t)")))

(defun elot-gptel-pattern--short (file)
  "Return FILE without its directory."
  (file-name-nondirectory file))

(defun elot-gptel-pattern--open (status)
  "Return the headings of STATUS that are still open.
TODO and STARTED count together; a STARTED heading is marked as
such, since its axioms are still missing."
  (append (plist-get status :todo)
          (mapcar (lambda (uri) (concat uri " (STARTED)"))
                  (plist-get status :started))))

(defun elot-gptel-pattern--todo-lines (status)
  "Return a report of the headings still TODO in STATUS."
  (let ((open (elot-gptel-pattern--open status)))
    (if (null open)
        "No headings still TODO."
      (concat (format "%d heading%s still TODO:\n"
                      (length open) (if (= (length open) 1) "" "s"))
              (mapconcat (lambda (uri) (concat "  - " uri)) open "\n")))))


;;;; Actions

(defun elot-gptel-pattern--create (target pattern-file pattern-target dropped)
  "Create a tracker for PATTERN-FILE in TARGET.
PATTERN-TARGET overrides the target ontology ID.  DROPPED is a
list of resource URIs to mark DONE and tag `:cancelled:'."
  (unless (and pattern-file (stringp pattern-file)
               (not (string-empty-p pattern-file)))
    (user-error "action `create' needs `pattern_file'"))
  (let* ((pattern (elot-gptel--resolve-file pattern-file))
         (existing (elot-pattern-tracker-status target)))
    (cond
     ((and existing
           (equal (file-name-base (or (plist-get existing :pattern) ""))
                  (file-name-base pattern)))
      (format "OK: %s already holds a tracker for %s (nothing to do)\n%s"
              (elot-gptel-pattern--short target)
              (file-name-base pattern)
              (elot-gptel-pattern--todo-lines existing)))
     (existing
      (format "FAIL: %s already holds a tracker for %s; \
finish or delete it before creating one for %s"
              (elot-gptel-pattern--short target)
              (file-name-base (or (plist-get existing :pattern) "another pattern"))
              (file-name-base pattern)))
     (t
      (elot-gptel-pattern--require-gate)
      (let ((known (elot-pattern-tracker-resource-uris pattern)))
        (dolist (uri dropped)
          (unless (member uri known)
            (user-error "not a resource of this pattern: %s" uri))))
      (elot-pattern-tracker-insert pattern target dropped pattern-target)
      (let* ((status (elot-pattern-tracker-status target))
             (cited (length (plist-get status :done)))
             (uncited (length (plist-get status :started)))
             (notes (delq nil
                          (list (and (> cited 0)
                                     (format "%d heading%s already present, marked DONE"
                                             cited (if (= cited 1) "" "s")))
                                (and (> uncited 0)
                                     (format "%d present without provenance, marked STARTED"
                                             uncited))))))
        (format "OK: tracker created in %s for pattern %s (ontology %s)\n%s%s"
                (elot-gptel-pattern--short target)
                (file-name-base pattern)
                (or (plist-get status :target) "?")
                (if notes (concat (string-join notes "; ") "\n") "")
                (elot-gptel-pattern--todo-lines status)))))))

(defun elot-gptel-pattern--status (target)
  "Report the tracker in TARGET."
  (let ((status (elot-pattern-tracker-status target)))
    (if (null status)
        (format "OK: %s holds no pattern tracker"
                (elot-gptel-pattern--short target))
      (let* ((todo (plist-get status :todo))
             (started (plist-get status :started))
             (done (plist-get status :done))
             (cancelled (plist-get status :cancelled))
             (open (+ (length todo) (length started))))
        (concat
         (if (> open 0)
             (format "INCOMPLETE: %d heading%s still TODO.\n"
                     open (if (= open 1) "" "s"))
           "OK: no headings still TODO.\n")
         (format "pattern: %s\nontology: %s\n"
                 (or (plist-get status :pattern) "?")
                 (or (plist-get status :target) "?"))
         (format "TODO (%d):%s\n" (length todo)
                 (if todo (concat "\n" (mapconcat (lambda (u) (concat "  - " u))
                                                  todo "\n"))
                   " none"))
         (format "STARTED (%d):%s\n" (length started)
                 (if started (concat "\n" (mapconcat (lambda (u) (concat "  - " u))
                                                     started "\n"))
                   " none"))
         (format "DONE (%d):%s\n" (length done)
                 (if done (concat "\n" (mapconcat (lambda (u) (concat "  - " u))
                                                  done "\n"))
                   " none"))
         (format "cancelled (%d):%s" (length cancelled)
                 (if cancelled
                     (concat "\n" (mapconcat (lambda (u) (concat "  - " u))
                                             cancelled "\n"))
                   " none")))))))

(defun elot-gptel-pattern--specs (todo done cancelled &optional started)
  "Return a list of (CURIE . STATE) from the plain string arrays.
TODO, STARTED, DONE and CANCELLED each name tracker headings.  A
CURIE named in more than one array is an error."
  (let (specs)
    (dolist (pair (list (cons todo "TODO")
                        (cons started "STARTED")
                        (cons done "DONE")
                        (cons cancelled "DONE :cancelled:")))
      (dolist (uri (elot-gptel-pattern--seq (car pair)))
        (unless (and (stringp uri) (not (string-empty-p (string-trim uri))))
          (user-error "`%s' must hold CURIE strings only" (cdr pair)))
        (setq uri (string-trim uri))
        (when (assoc uri specs)
          (user-error "%s is named in more than one state array" uri))
        (push (cons uri (cdr pair)) specs)))
    (nreverse specs)))

(defun elot-gptel-pattern--set-state (target todo done cancelled &optional started)
  "Set tracker states in TARGET from the state arrays.
TODO, STARTED, DONE and CANCELLED each name tracker headings."
  (let* ((specs (elot-gptel-pattern--specs todo done cancelled started))
         (status (elot-pattern-tracker-status target)))
    (unless specs
      (user-error "action `set_state' needs at least one of \
`todo', `started', `done' or `cancelled'"))
    (unless status
      (user-error "%s holds no pattern tracker"
                  (elot-gptel-pattern--short target)))
    (elot-gptel-pattern--require-gate)
    (let ((changed (elot-pattern-tracker-set-states specs nil target)))
      (let ((after (elot-pattern-tracker-status target)))
        (concat
         (format "OK: %d heading%s changed in %s\n"
                 (length changed) (if (= (length changed) 1) "" "s")
                 (elot-gptel-pattern--short target))
         (if changed
             (concat (mapconcat
                      (lambda (c)
                        (format "  - %s: %s -> %s"
                                (nth 0 c)
                                (if (string-empty-p (nth 1 c)) "(none)" (nth 1 c))
                                (nth 2 c)))
                      changed "\n")
                     "\n")
           "")
         (elot-gptel-pattern--todo-lines after))))))

(defun elot-gptel-pattern--delete (target force)
  "Remove the tracker from TARGET.  FORCE allows an incomplete one."
  (let ((status (elot-pattern-tracker-status target)))
    (cond
     ((null status)
      (format "OK: %s holds no pattern tracker (nothing to remove)"
              (elot-gptel-pattern--short target)))
     ((and (elot-gptel-pattern--open status) (not force))
      (format "FAIL: tracker in %s is incomplete\n%s\nSet the remaining \
headings DONE with action=set_state, or repeat with force=true."
              (elot-gptel-pattern--short target)
              (elot-gptel-pattern--todo-lines status)))
     (t
      (elot-gptel-pattern--require-gate)
      (let ((remaining (length (elot-gptel-pattern--open status))))
        (elot-pattern-tracker-delete target)
        (if (> remaining 0)
            (format "OK: tracker removed from %s; %d unfinished heading%s discarded"
                    (elot-gptel-pattern--short target) remaining
                    (if (= remaining 1) "" "s"))
          (format "OK: tracker removed from %s"
                  (elot-gptel-pattern--short target))))))))


;;;; Moving tracker headings on from a declaration or an axiom batch

(defun elot-gptel-pattern--flip (file curies state)
  "Set the tracker headings CURIES in FILE to STATE.
STATE is \"STARTED\" or \"DONE\".  A heading that is already at,
or past, STATE is left alone and reported.  Returns a plain-text
note to append to the caller's `OK:' line, or nil when there is
nothing to say."
  (let* ((curies (delete-dups (mapcar #'string-trim
                                      (elot-gptel-pattern--seq curies))))
         (status (and curies (elot-pattern-tracker-status file))))
    (when status
      (let* ((todo (plist-get status :todo))
             (started (plist-get status :started))
             (done (plist-get status :done))
             (cancelled (plist-get status :cancelled))
             flip notes)
        (dolist (uri curies)
          (cond
           ((member uri todo) (push uri flip))
           ((member uri started)
            (if (equal state "DONE")
                (push uri flip)
              (push (format "NOTE: tracker heading %s was already STARTED." uri)
                    notes)))
           ((member uri cancelled)
            (push (format "NOTE: tracker heading %s was cancelled; \
it is left as it is." uri) notes))
           ((member uri done)
            (push (format "NOTE: tracker heading %s was already DONE." uri)
                  notes))
           (t
            (push (format "NOTE: %s is not a heading of the tracker in %s."
                          uri (elot-gptel-pattern--short file))
                  notes))))
        (setq flip (nreverse flip) notes (nreverse notes))
        (when flip
          (elot-pattern-tracker-set-states flip state file)
          (setq notes
                (append notes
                        (list (format "TRACKER: marked %s: %s"
                                      state (string-join flip ", "))))))
        (let ((left (or (elot-pattern-tracker-todo-count file) 0)))
          (when (or flip notes)
            (string-join
             (append notes
                     (list (format "TRACKER: %d heading%s still TODO for \
ontology %s in %s."
                                   left (if (= left 1) "" "s")
                                   (or (plist-get status :target) "?")
                                   (elot-gptel-pattern--short file))))
             "\n")))))))

(defun elot-gptel-pattern-declare-flip (file curie extra &optional provenance)
  "Move tracker headings on in FILE after a declaration.
CURIE is the declared identifier; it matches a tracker heading
for a fixed pattern constant.  EXTRA names further tracker
headings the declaration satisfies, for a variable resource whose
minted CURIE cannot be matched by name.

PROVENANCE non-nil means the same declaration also wrote an
`rdfs:isDefinedBy' row, so nothing further is owed to the
pattern and the headings go straight to DONE.  Without it the
heading exists but its axioms are still missing, so it is set
STARTED and the axiom writer finishes it.

Returns a plain-text note to append to the caller's `OK:' line,
or nil when there is nothing to say.  Never signals: a tracker
problem must not affect the declaration."
  (condition-case nil
      (let* ((extra (elot-gptel-pattern--seq extra))
             (status (elot-pattern-tracker-status file)))
        (cond
         ((null status)
          (when extra
            (format "NOTE: %s holds no pattern tracker; \
tracker_started / tracker_done was ignored."
                    (elot-gptel-pattern--short file))))
         (t
          (let* ((known (append (plist-get status :todo)
                                (plist-get status :started)
                                (plist-get status :done)
                                (plist-get status :cancelled)))
                 (wanted (append (and (member curie known) (list curie))
                                 (mapcar #'string-trim extra))))
            (elot-gptel-pattern--flip
             file wanted (if provenance "DONE" "STARTED"))))))
    (error nil)))

(defun elot-gptel-pattern-axiom-flip (file curies)
  "Mark tracker headings CURIES DONE in FILE after an axiom batch.
Called only once the batch has been committed and revalidated, so
a rolled-back edit changes no tracker state.  Returns a note to
append to the caller's reply, or nil.  Never signals: a tracker
problem must not affect the edit."
  (condition-case nil
      (let ((curies (elot-gptel-pattern--seq curies)))
        (when curies
          (if (null (elot-pattern-tracker-status file))
              (format "NOTE: %s holds no pattern tracker; \
tracker_done was ignored." (elot-gptel-pattern--short file))
            (elot-gptel-pattern--flip file curies "DONE"))))
    (error nil)))


;;;; The public tool function

(defun elot-gptel-tool-pattern-tracker
    (action target_file &optional pattern_file pattern_target dropped_legs
            todo done cancelled force started)
  "Implementation of the `elot_pattern_tracker' tool.

ACTION is `create', `status', `set_state' or `delete'.
TARGET_FILE is the ELOT file holding, or to hold, the tracker.
PATTERN_FILE, PATTERN_TARGET and DROPPED_LEGS belong to `create';
TODO, STARTED, DONE and CANCELLED to `set_state'; FORCE to
`delete'.

Returns one plain-text string starting with `OK:', `FAIL:' or
`ERROR:'.  No signal escapes."
  (condition-case err
      (let ((target (elot-gptel--resolve-file target_file))
            (act (and (stringp action) (downcase (string-trim action)))))
        (pcase act
          ("create"
           (elot-gptel-pattern--create
            target pattern_file pattern_target
            (elot-gptel-pattern--seq dropped_legs)))
          ("status" (elot-gptel-pattern--status target))
          ("set_state"
           (elot-gptel-pattern--set-state target todo done cancelled started))
          ("delete"
           (elot-gptel-pattern--delete target (elot-gptel--truthy force)))
          (_ (format "ERROR: unknown action `%s'; \
use create, status, set_state or delete" (or action "")))))
    (user-error (format "ERROR: %s" (error-message-string err)))
    (error      (format "ERROR: %s" (error-message-string err)))))


;;;; Tool spec

(defconst elot-gptel--spec-pattern-tracker
  `("elot_pattern_tracker"
    :function elot-gptel-tool-pattern-tracker
    :confirm t
    :description
    "Keep a small progress outline inside a target ELOT file while \
one modelling pattern is applied to it.  The outline lists the \
pattern's resource headings, each TODO, STARTED, DONE, or DONE \
with the `:cancelled:' tag, so work can be resumed after an \
interruption.  STARTED means the heading exists but its axioms \
are still missing, so it counts as outstanding work.

ACTION selects what to do:
  - `create'    trim the pattern named by `pattern_file' and insert
                the tracker before the first heading of `target_file',
                then save.  `pattern_target' is the target ontology
                heading's ID; when omitted it is read from the file.
                Resource headings named in `dropped_legs' are marked
                DONE and tagged `:cancelled:'; all others are TODO.
                Cancelling does not cascade, so a subclass of a
                cancelled resource stays TODO.  A pattern resource
                whose CURIE is already declared in `target_file'
                does not start at TODO: it starts DONE when that
                declaration carries an `rdfs:isDefinedBy' row, and
                STARTED when it does not, so repeat application of
                a pattern begins from an honest worklist.
  - `status'    report the pattern, the target ontology, and the
                headings grouped by state.  Read-only.  When work
                remains the first line is `INCOMPLETE: N headings
                still TODO.', where N counts TODO and STARTED
                together.
  - `set_state' change several headings in one call, which is the
                normal case because a pattern leg is added as a
                whole.  Give one plain array of CURIEs per state:
                `todo', `started', `done', `cancelled' (the last
                means DONE with the `:cancelled:' tag).  At least
                one array is needed; a CURIE may appear in only one
                of them.  All changes are applied or none; an
                unknown CURIE makes the whole call an ERROR and
                nothing is written.
  - `delete'    remove the tracker and save.  Refuses while TODO or
                STARTED headings remain unless `force' is true; a
                forced deletion reports how many were discarded.

Returns one plain-text string.  `OK:' when the action succeeded or
its intent already held (delete with no tracker, create when the
same tracker is already there); `FAIL:' for a conflict that would
lose work or mix two patterns; `ERROR:' for a fault.  Mutating
actions are gated by `elot-gptel-allow-side-effects' and write to
disk at once.

The tracker is inert: it carries `ELOT-context-type: pattern-tracker'
and `:tangle no', so it is not part of the ontology.  It is a
checklist only -- consult the pattern file itself for the rules.

Out of scope: no lint, no OMN parse, no ROBOT stage; more than one
tracker per file; guessing DONE from changes made to the ontology."
    :args
    ((:name "action"
            :type string
            :enum ["create" "status" "set_state" "delete"]
            :description
            "What to do: create, status, set_state or delete.")
     (:name "target_file"
            :type string
            :description
            "Path to the ELOT .org file holding, or to hold, the \
tracker, relative to the project root.")
     (:name "pattern_file"
            :type string
            :optional t
            :description
            "For `create': path to the pattern .org file, relative \
to the project root.")
     (:name "pattern_target"
            :type string
            :optional t
            :description
            "For `create': the target ontology heading's ID.  When \
omitted it is read from the target file.")
     (:name "dropped_legs"
            :type array
            :items (:type string)
            :optional t
            :description
            "For `create': CURIEs of pattern resources that are not \
applied.  Each is marked DONE and tagged `:cancelled:'.  \
Cancelling does not cascade.")
     (:name "todo"
            :type array
            :items (:type string)
            :optional t
            :description
            "For `set_state': CURIEs of tracker headings to set \
TODO.")
     (:name "done"
            :type array
            :items (:type string)
            :optional t
            :description
            "For `set_state': CURIEs of tracker headings to set \
DONE.")
     (:name "cancelled"
            :type array
            :items (:type string)
            :optional t
            :description
            "For `set_state': CURIEs of tracker headings to set \
DONE with the `:cancelled:' tag.")
     (:name "force"
            :type boolean
            :optional t
            :description
            "For `delete': when true, remove a tracker that still \
has TODO or STARTED headings.  Default false.")
     (:name "started"
            :type array
            :items (:type string)
            :optional t
            :description
            "For `set_state': CURIEs of tracker headings to set \
STARTED, meaning the heading exists but its axioms are still \
missing."))))

(provide 'elot-gptel-pattern)

;;; elot-gptel-pattern.el ends here
