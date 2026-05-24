;;; elot-robot.el --- Shared ROBOT process layer for ELOT  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; Version: 2.0.0
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Shared process-layer wrapper for the ROBOT command-line tool
;; (https://robot.obolibrary.org/).  Used by:
;;
;;   - `elot-tangle.el'  — post-tangle OMN -> Turtle conversion
;;   - `elot-sources.el' — SPARQL queries against Turtle sources
;;   - `elot-gptel.el'   — LLM-callable validation, SPARQL and
;;                          reasoning tools (Milestones 3-5)
;;
;; *ROBOT is optional for ELOT.*  Manchester Syntax (OMN) output
;; from ELOT itself is pure Emacs Lisp.  ROBOT is only needed for:
;;
;;   1. Converting OMN -> Turtle / OFN / other RDF serialisations.
;;   2. Executing SPARQL queries via ROBOT's `query' subcommand.
;;   3. Reasoning (consistency checks, unsatisfiable classes) via
;;      ROBOT's `reason' and `explain' subcommands.
;;
;; Users who never need these operations can ignore this module
;; entirely; nothing in core ELOT depends on ROBOT.
;;
;; Two configuration paths are supported:
;;
;;   a. `elot-robot-jar-path' names a `robot.jar' file; ROBOT is
;;      invoked as `java -jar PATH ...'.  This is the canonical
;;      setup for the project and the form used in the manual.
;;
;;   b. A `robot' executable (typically a shell shim) is available
;;      on `exec-path'; used as a fallback when the jar is absent.
;;
;; All process invocations use `make-process' / `call-process' with
;; an explicit argv list — never `shell-command' with a concatenated
;; string.  This eliminates an entire category of bugs around quoting,
;; spaces in paths, and shell injection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; --------------------------------------------------------------
;;;; Configuration
;;;; --------------------------------------------------------------

(defgroup elot-robot nil
  "Shared ROBOT process layer for ELOT.
ROBOT is an optional external dependency used for OMN -> Turtle
conversion, SPARQL execution against Turtle, and OWL reasoning."
  :group 'elot
  :prefix "elot-robot-")

(defcustom elot-robot-jar-path (expand-file-name "~/bin/robot.jar")
  "Path to the robot.jar file.
When this names an existing file ROBOT is invoked as
`java -jar PATH'.  When it is empty or missing, ELOT falls back
to a `robot' executable on `exec-path' if available.

Set this variable, or install a `robot' shim, only if you intend
to use the ROBOT-backed features (OMN -> TTL conversion, SPARQL
over TTL, reasoning).  Core ELOT works without it."
  :group 'elot-robot
  :type 'string)

(defcustom elot-robot-jvm-args nil
  "Extra JVM flags passed to ROBOT when invoked via `java -jar'.
A list of strings inserted *before* `-jar' in the argv.  Use this
to raise heap size for large ontologies, e.g. \\='(\"-Xmx4G\")."
  :group 'elot-robot
  :type '(repeat string))

(defcustom elot-robot-default-timeout nil
  "Default wall-clock timeout for ROBOT invocations, in seconds.
When nil (the default), `elot-robot-run' waits indefinitely --
matching the historical behaviour.  When a positive number, every
`elot-robot-run' call that does not pass an explicit `:timeout'
will be aborted after that many seconds: ROBOT receives SIGTERM,
then SIGKILL after a short grace period.  The result plist then
has `:exit' nil and `:timed-out' t, and `elot-robot-classify'
returns the kind `:timeout'.

Per-call `:timeout' on `elot-robot-run' overrides this default
(including the explicit value 0, which disables the timeout for
that one call)."
  :group 'elot-robot
  :type '(choice (const :tag "No timeout" nil)
                 (number :tag "Seconds")))

(defcustom elot-robot-timeout-grace 2.0
  "Seconds to wait between SIGTERM and SIGKILL after a timeout.
After `elot-robot-run' decides a call has overrun its timeout it
sends SIGTERM to the ROBOT process and waits this many seconds
for a clean shutdown; if the process is still alive at that
point, SIGKILL is sent."
  :group 'elot-robot
  :type 'number)

;; Backwards-compatible alias.  Some user init files have
;;     (setq elot-robot-command-str ...)
;; Keep it defined and synchronised with `elot-robot-jar-path' so
;; old configurations keep working.
(defvar elot-robot-command-str
  (concat "java -jar " elot-robot-jar-path)
  "Shell-style ROBOT invocation string, derived from `elot-robot-jar-path'.
Prefer the argv-based `elot-robot-run' in new code.  This variable
is preserved for backwards compatibility with existing callers
and user customisations.")

;;;; --------------------------------------------------------------
;;;; Availability probe (cached, cheap)
;;;; --------------------------------------------------------------

(defvar elot-robot--available-cache 'unset
  "Cached result of `elot-robot-available-p'.
The symbol `unset' means \"not probed yet\"; other values are
the probe result (t or nil).")

(defvar elot-robot--invocation-cache nil
  "Cached argv prefix for invoking ROBOT.
Either (\"java\" \"-jar\" JAR) when the jar form is configured,
or (\"robot\") when the executable shim is on `exec-path',
or nil when ROBOT is not available.")

(defun elot-robot--compute-invocation ()
  "Compute the argv prefix for invoking ROBOT.
Returns a list of strings, or nil if no configuration works.
Prefers `elot-robot-jar-path' when it names an existing file;
falls back to a `robot' executable on `exec-path'."
  (cond
   ((and (stringp elot-robot-jar-path)
         (not (string-empty-p elot-robot-jar-path))
         (file-exists-p elot-robot-jar-path))
    (append (list "java") elot-robot-jvm-args
            (list "-jar" elot-robot-jar-path)))
   ((executable-find "robot")
    (list (executable-find "robot")))
   (t nil)))

(defun elot-robot-available-p (&optional refresh)
  "Return non-nil if ROBOT is configured for invocation.
The result is cached for the rest of the Emacs session.  Pass a
non-nil REFRESH to force re-evaluation, e.g. after the user has
just set `elot-robot-jar-path' via `customize'.

This is a *cheap* probe — it only checks the filesystem and
`exec-path'.  It does *not* spawn a JVM.  Use `elot-robot-version'
when you need to verify ROBOT actually runs."
  (when (or refresh (eq elot-robot--available-cache 'unset))
    (setq elot-robot--invocation-cache (elot-robot--compute-invocation))
    (setq elot-robot--available-cache
          (and elot-robot--invocation-cache t)))
  elot-robot--available-cache)

(defun elot-robot-invocation ()
  "Return the cached argv prefix for invoking ROBOT, or nil."
  (elot-robot-available-p)
  elot-robot--invocation-cache)

(defun elot-robot-reset-cache ()
  "Forget the cached ROBOT availability / invocation.
Call this after changing `elot-robot-jar-path' or `exec-path'."
  (interactive)
  (setq elot-robot--available-cache 'unset
        elot-robot--invocation-cache nil
        elot-robot--version-cache 'unset
        elot-robot-command-str
        (concat "java -jar " (or elot-robot-jar-path ""))))

;;;; --------------------------------------------------------------
;;;; The synchronous primitive
;;;; --------------------------------------------------------------

(defun elot-robot--effective-timeout (timeout)
  "Resolve TIMEOUT against `elot-robot-default-timeout'.
Returns either a positive number of seconds or nil.  An explicit
TIMEOUT (including 0) overrides the default; 0 means \"no
timeout for this call\"."
  (cond
   ((eq timeout nil)
    (let ((d elot-robot-default-timeout))
      (and (numberp d) (> d 0) d)))
   ((and (numberp timeout) (> timeout 0)) timeout)
   (t nil)))

(defun elot-robot--strip-process-status-lines ()
  "Strip Emacs process-sentinel status lines from the current buffer.
Belt-and-braces complement to `:sentinel #\\='ignore' on the ROBOT
processes: removes any trailing line of the form
\"Process elot-robot[-stderr] <status>\" that a future Emacs (or
a non-default sentinel chain) might re-introduce."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^Process elot-robot\\(?:-stderr\\)?[^\n]*\n?" nil t)
      (replace-match "" t t))))

(defun elot-robot--run-with-timeout
    (program rest input stdout-file stderr-file stdout-buf timeout)
  "Run PROGRAM REST under a wall-clock TIMEOUT (positive seconds).

INPUT, if non-nil, is sent to the process's stdin.  STDOUT goes
to STDOUT-FILE when set, otherwise into STDOUT-BUF.  STDERR is
redirected to STDERR-FILE.

Returns a cons (EXIT . TIMED-OUT).  EXIT is the integer exit code
when the process completed naturally, or nil when killed by the
timeout watchdog; TIMED-OUT is t in the latter case."
  (let* ((capture-buf (if stdout-file
                          (generate-new-buffer " *elot-robot-stdout-tmp*")
                        stdout-buf))
         (stderr-buf (generate-new-buffer " *elot-robot-stderr-tmp*"))
         (stderr-proc (make-pipe-process
                       :name "elot-robot-stderr"
                       :buffer stderr-buf
                       :noquery t
                       ;; Suppress Emacs's default "Process ... finished"
                       ;; sentinel insertion, which would otherwise be
                       ;; written into STDERR-BUF and confuse classifiers.
                       :sentinel #'ignore))
         (proc (make-process
                :name "elot-robot"
                :command (cons program rest)
                :buffer capture-buf
                :stderr stderr-proc
                :connection-type 'pipe
                :noquery t
                ;; Same rationale as above: keep CAPTURE-BUF free of
                ;; sentinel-injected status lines so stdout reads back
                ;; verbatim from ROBOT.
                :sentinel #'ignore))
         (timed-out nil)
         (kill-timer nil)
         (timer
          (run-at-time
           timeout nil
           (lambda ()
             (when (process-live-p proc)
               (setq timed-out t)
               (ignore-errors (interrupt-process proc))
               (setq kill-timer
                     (run-at-time
                      elot-robot-timeout-grace nil
                      (lambda ()
                        (when (process-live-p proc)
                          (ignore-errors (kill-process proc)))))))))))
    (unwind-protect
        (progn
          (when input
            (ignore-errors (process-send-string proc input))
            (ignore-errors (process-send-eof proc)))
          (while (process-live-p proc)
            (accept-process-output proc 0.1))
          ;; Drain the stderr pipe.  Loop until the pipe process is
          ;; dead *and* a final poll returns nil: a single pass can
          ;; miss the last chunk on slow pipes.
          (while (or (process-live-p stderr-proc)
                     (accept-process-output stderr-proc 0.05))))
      (ignore-errors (cancel-timer timer))
      (when kill-timer (ignore-errors (cancel-timer kill-timer)))
      (when (process-live-p proc)
        (ignore-errors (kill-process proc)))
      (when (process-live-p stderr-proc)
        (ignore-errors (delete-process stderr-proc)))
      ;; Flush stderr buffer to the file the outer caller reads.
      ;; Defensively strip any trailing "Process elot-robot..." status
      ;; line in case a future Emacs reintroduces sentinel-injected
      ;; messages despite `:sentinel #'ignore' above.
      (when (buffer-live-p stderr-buf)
        (with-current-buffer stderr-buf
          (elot-robot--strip-process-status-lines)
          (write-region (point-min) (point-max) stderr-file nil 'silent))
        (kill-buffer stderr-buf))
      ;; If a stdout file was requested, flush the capture buffer to it.
      (when (and stdout-file (buffer-live-p capture-buf))
        (with-current-buffer capture-buf
          (elot-robot--strip-process-status-lines)
          (write-region (point-min) (point-max) stdout-file nil 'silent))
        (kill-buffer capture-buf)))
    (cons (and (not timed-out) (process-exit-status proc))
          timed-out)))

(cl-defun elot-robot-run (args &key input stdout-file working-directory timeout)
  "Run ROBOT synchronously with ARGS, a list of subcommand strings.

ARGS supplies *only* the ROBOT subcommand and its options
(e.g. (\"convert\" \"--input\" IN \"--output\" OUT)).  The argv
prefix selecting `java -jar JAR' or the `robot' shim is provided
automatically from `elot-robot-invocation'.

Keyword arguments:

  :INPUT             string written to ROBOT's stdin, or nil.
  :STDOUT-FILE       file to receive stdout verbatim, or nil (in
                      which case stdout is captured in the returned
                      plist).
  :WORKING-DIRECTORY directory in which to run the process; defaults
                      to `default-directory'.
  :TIMEOUT           wall-clock seconds before the call is aborted.
                      Overrides `elot-robot-default-timeout' for
                      this call; pass 0 to disable the timeout
                      explicitly.  When ROBOT is killed by the
                      watchdog the returned plist has `:exit' nil
                      and `:timed-out' t.

Returns a plist:

  (:exit N :stdout S :stderr S :duration SECS :argv (...) :timed-out BOOL)

Signals `user-error' only when ROBOT is not available.  Non-zero
exit codes (and timeouts) are *returned* in the plist, so callers
can classify them (see `elot-robot-classify')."
  (unless (elot-robot-available-p)
    (user-error
     "ROBOT not available — set `elot-robot-jar-path' or install `robot' on PATH"))
  (let* ((prefix    (elot-robot-invocation))
         (full-argv (append prefix args))
         (program   (car full-argv))
         (rest      (cdr full-argv))
         (stderr-file (make-temp-file "elot-robot-stderr-"))
         (stdout-buf  (and (not stdout-file)
                           (generate-new-buffer " *elot-robot-stdout*")))
         (default-directory (or working-directory default-directory))
         (effective-timeout (elot-robot--effective-timeout timeout))
         (t0 (current-time))
         exit timed-out stdout stderr)
    (unwind-protect
        (progn
          (if effective-timeout
              (let ((r (elot-robot--run-with-timeout
                        program rest input stdout-file stderr-file
                        stdout-buf effective-timeout)))
                (setq exit      (car r)
                      timed-out (cdr r)))
            (let ((destination (if stdout-file
                                   (list :file stdout-file)
                                 (list stdout-buf stderr-file))))
              (setq exit
                    (cond
                     (input
                      (with-temp-buffer
                        (insert input)
                        (apply #'call-process-region
                               (point-min) (point-max)
                               program nil destination nil rest)))
                     (t
                      (apply #'call-process program nil
                             destination nil rest))))))
          (setq stdout
                (cond (stdout-file "")     ; written to file, not captured
                      ((buffer-live-p stdout-buf)
                       (with-current-buffer stdout-buf
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
                      (t "")))
          (setq stderr
                (with-temp-buffer
                  (when (file-readable-p stderr-file)
                    (insert-file-contents stderr-file))
                  (buffer-substring-no-properties (point-min) (point-max)))))
      (when (buffer-live-p stdout-buf) (kill-buffer stdout-buf))
      (when (file-exists-p stderr-file) (ignore-errors (delete-file stderr-file))))
    (list :exit      exit
          :stdout    stdout
          :stderr    stderr
          :duration  (float-time (time-subtract (current-time) t0))
          :argv      full-argv
          :timed-out (and timed-out t))))

;;;; --------------------------------------------------------------
;;;; Version probe (live, cached)
;;;; --------------------------------------------------------------

(defvar elot-robot--version-cache 'unset
  "Cached value of `elot-robot-version' (a string or nil).")

(defun elot-robot-version (&optional refresh)
  "Return the ROBOT version string, e.g. \"1.9.6\", or nil.
Spawns a JVM the first time it is called in a session; the result
is cached.  Pass a non-nil REFRESH to re-probe."
  (when (or refresh (eq elot-robot--version-cache 'unset))
    (setq elot-robot--version-cache
          (and (elot-robot-available-p)
               (condition-case _err
                   (let* ((res (elot-robot-run '("--version")))
                          (out (concat (plist-get res :stdout)
                                       (plist-get res :stderr))))
                     (when (string-match
                            "ROBOT[^\n0-9]*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)"
                            out)
                       (match-string 1 out)))
                 (error nil)))))
  elot-robot--version-cache)

;;;; --------------------------------------------------------------
;;;; Stderr classifier
;;;; --------------------------------------------------------------
;;
;; ROBOT reports a handful of stable diagnostic shapes on stderr.
;; The classifier turns those strings into structured data so that
;; tangle, sources, the gptel tools and any future caller can react
;; uniformly instead of each grepping stderr for their own substrings.
;;
;; The classifier is pure: it depends only on its STDERR and EXIT
;; arguments.  Add new patterns by extending `elot-robot--classifiers'.

(defconst elot-robot--classifiers
  '(elot-robot--match-syntax-error
    elot-robot--match-unsatisfiable
    elot-robot--match-inconsistent
    elot-robot--match-reasoner-error
    elot-robot--match-io-error)
  "Ordered list of classifier functions tried by `elot-robot-classify'.
Each function receives STDERR (string) and returns either nil
(no match) or a cons (KIND . PAYLOAD-PLIST).  The first matching
classifier wins, so put more specific patterns first.")

(defun elot-robot--match-syntax-error (stderr)
  "Detect a Manchester/OMN parser error in STDERR.
Returns (:syntax-error :file F :line L :column C :message M) or nil."
  (when (or (string-match-p
             "ManchesterOWLSyntaxOntologyParser" stderr)
            (string-match-p "Parser:.*OWLParser" stderr)
            (string-match-p "OWLOntologyCreationException" stderr))
    (let ((line nil) (col nil) (file nil) (msg nil))
      ;; Canonical ROBOT/OWLAPI form.
      (when (string-match
             "Line[ \t]+\\([0-9]+\\)[ \t]+column[ \t]+\\([0-9]+\\)"
             stderr)
        (setq line (string-to-number (match-string 1 stderr))
              col  (string-to-number (match-string 2 stderr))))
      ;; Some ROBOT versions emit "at line N, column M".
      (when (and (not line)
                 (string-match
                  "at line[ \t]+\\([0-9]+\\)[, \t]+column[ \t]+\\([0-9]+\\)"
                  stderr))
        (setq line (string-to-number (match-string 1 stderr))
              col  (string-to-number (match-string 2 stderr))))
      (when (string-match "Input:[ \t]*\\(.+\\)$" stderr)
        (setq file (string-trim (match-string 1 stderr))))
      (when (string-match "Encountered[ \t]+\\(.*\\)" stderr)
        (setq msg (string-trim (match-string 1 stderr))))
      (cons :syntax-error
            (list :file file :line line :column col
                  :message (or msg "Manchester syntax parse error"))))))

(defun elot-robot--match-unsatisfiable (stderr)
  "Detect unsatisfiable-class output in STDERR.
Returns (:unsatisfiable-classes :iris (...)) or nil."
  (when (or (string-match-p "UNSATISFIABLE CLASSES" stderr)
            (string-match-p "unsatisfiable class" stderr)
            (string-match-p "There are[ \t]+[0-9]+[ \t]+unsatisfiable"
                            stderr))
    (let ((iris '())
          (pos 0))
      (while (string-match
              "\\(?:- \\|unsatisfiable: \\)\\(<\\([^>]+\\)>\\|\\(https?://[^ \t\n<>\"]+\\)\\|\\([A-Za-z][A-Za-z0-9_]*:[A-Za-z0-9_-]+\\)\\)"
              stderr pos)
        (push (or (match-string 2 stderr)
                  (match-string 3 stderr)
                  (match-string 4 stderr))
              iris)
        (setq pos (match-end 0)))
      (cons :unsatisfiable-classes
            (list :iris (nreverse iris))))))

(defun elot-robot--match-inconsistent (stderr)
  "Detect inconsistent-ontology output in STDERR.
Returns (:inconsistent-ontology :explanation STR) or nil."
  (when (or (string-match-p "ONTOLOGY HAS INCONSISTENCIES" stderr)
            (string-match-p "is inconsistent" stderr)
            (string-match-p "INCONSISTENT ONTOLOGY" stderr))
    (let ((explanation
           (cond
            ((string-match
              "Explanation:[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)\\(?:\n\n\\|\\'\\)"
              stderr)
             (string-trim (match-string 1 stderr)))
            (t (string-trim stderr)))))
      (cons :inconsistent-ontology
            (list :explanation explanation)))))

(defun elot-robot--match-reasoner-error (stderr)
  "Detect a reasoner failure in STDERR.
Returns (:reasoner-error :reasoner NAME :message MSG) or nil."
  (when (or (string-match-p "REASONER ERROR" stderr)
            (string-match-p "Unknown reasoner" stderr)
            (string-match-p "not supported by reasoner" stderr))
    (let ((reasoner
           (when (string-match
                  "reasoner[: \t]+\\([A-Za-z][A-Za-z0-9-]*\\)" stderr)
             (match-string 1 stderr))))
      (cons :reasoner-error
            (list :reasoner reasoner
                  :message (string-trim stderr))))))

(defun elot-robot--match-io-error (stderr)
  "Detect a missing-file / permission error in STDERR.
Returns (:io-error :path P :message M) or nil."
  (when (or (string-match-p "FileNotFoundException" stderr)
            (string-match-p "NoSuchFileException" stderr)
            (string-match-p "MISSING FILE ERROR" stderr)
            (string-match-p "Permission denied" stderr)
            (string-match-p "AccessDeniedException" stderr))
    (let ((path
           (cond
            ((string-match
              "\\(?:FileNotFoundException\\|NoSuchFileException\\|AccessDeniedException\\):[ \t]*\\([^\n]+?\\)\\(?: (\\|\n\\|\\'\\)"
              stderr)
             (string-trim (match-string 1 stderr)))
            ((string-match "MISSING FILE ERROR[^\n]*\n[ \t]*\\([^\n]+\\)" stderr)
             (string-trim (match-string 1 stderr)))
            (t nil))))
      (cons :io-error
            (list :path path
                  :message (string-trim stderr))))))

(defun elot-robot-classify (stderr &optional exit)
  "Classify ROBOT's STDERR output (with optional EXIT code).
Returns a cons (KIND . PAYLOAD-PLIST) where KIND is one of:

  :ok                       success, nothing notable
  :syntax-error             Manchester/OMN parse failure
                            payload: (:file :line :column :message)
  :io-error                 missing file / permission denied
                            payload: (:path :message)
  :inconsistent-ontology    payload: (:explanation)
  :unsatisfiable-classes    payload: (:iris (...))
  :reasoner-error           payload: (:reasoner :message)
  :timeout                  ROBOT exceeded the wall-clock budget
                            payload: (:stderr STDERR)
                            (only produced via `elot-robot-classify-result';
                             this function alone has no notion of timeouts)
  :unknown                  payload: (:stderr STDERR)

EXIT defaults to nil; when supplied and zero with no recognised
diagnostic in STDERR, the result is `:ok'."
  (let ((stderr (or stderr "")))
    (or (cl-some (lambda (fn) (funcall fn stderr))
                 elot-robot--classifiers)
        (cond
         ((and (numberp exit) (zerop exit))
          (cons :ok nil))
         ((string-empty-p (string-trim stderr))
          (cons (if (and (numberp exit) (zerop exit)) :ok :unknown)
                (list :stderr stderr)))
         (t (cons :unknown (list :stderr stderr)))))))

(defun elot-robot-classify-result (result)
  "Classify a plist RESULT returned by `elot-robot-run'.
Recognises the `:timed-out' flag as kind `:timeout' so callers
can report wall-clock aborts uniformly."
  (if (plist-get result :timed-out)
      (cons :timeout
            (list :stderr   (or (plist-get result :stderr) "")
                  :duration (plist-get result :duration)))
    (elot-robot-classify (plist-get result :stderr)
                         (plist-get result :exit))))

;;;; --------------------------------------------------------------
;;;; Temp-workspace helper
;;;; --------------------------------------------------------------
;;
;; Multi-step ROBOT pipelines (write a query file -> run ROBOT ->
;; read the CSV result -> clean up) repeatedly invent the same
;; `make-temp-file' + `unwind-protect' dance.  The helper below
;; allocates a single fresh directory under `temporary-file-directory',
;; passes its path to a function, and deletes it recursively on exit
;; -- including non-local exit through an error or `throw'.
;;
;; Inside the callback, callers can name files relative to the
;; workspace path (e.g. `(expand-file-name "query.rq" ws)') and
;; trust that they will be cleaned up.

(defvar elot-robot-workspace-keep nil
  "When non-nil, do not delete the workspace directory on exit.
Useful for debugging a failed pipeline; leave nil in normal use.")

(defun elot-robot--make-workspace (&optional prefix)
  "Allocate a fresh workspace directory and return its absolute path.
PREFIX defaults to \"elot-robot-ws-\"."
  (file-name-as-directory
   (make-temp-file (or prefix "elot-robot-ws-") t)))

(defun elot-robot--delete-workspace (dir)
  "Recursively delete workspace directory DIR, ignoring errors.
No-op when `elot-robot-workspace-keep' is non-nil or DIR is nil
or does not name a directory."
  (when (and dir
             (not elot-robot-workspace-keep)
             (stringp dir)
             (file-directory-p dir))
    (ignore-errors (delete-directory dir t))))

(defun elot-robot-call-with-workspace (fn &optional prefix)
  "Call FN with one argument, the path to a fresh workspace directory.

A unique directory is created under `temporary-file-directory'
(name prefix PREFIX, default \"elot-robot-ws-\"); its absolute
path (with trailing slash) is passed to FN, and
`default-directory' is bound to the same path for the duration of
the call.  The directory is deleted recursively on exit, even
when FN exits non-locally through an error or `throw'.

Set `elot-robot-workspace-keep' non-nil to retain the directory
for post-mortem inspection."
  (let ((dir (elot-robot--make-workspace prefix)))
    (unwind-protect
        (let ((default-directory dir))
          (funcall fn dir))
      (elot-robot--delete-workspace dir))))

(provide 'elot-robot)
;;; elot-robot.el ends here
