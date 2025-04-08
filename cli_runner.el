;; cli_runner.el - Batch execution script for ELOT
;; Uses vendored dependencies from ./lib/ and ./elot-package/

;; --- Environment Setup ---

;; Get essential directory paths from environment variables
(defvar elot-cli-root-dir (getenv "ELOT_ROOT_DIR")
  "The root directory of the ELOT CLI project, from ELOT_ROOT_DIR env var.")
(defvar elot-cli-bin-dir (getenv "ELOT_BIN_DIR")
  "The directory containing binaries like robot.jar, from ELOT_BIN_DIR env var.")

;; Validate environment variables
(unless elot-cli-root-dir
  (error "Environment variable ELOT_ROOT_DIR is not set! Cannot proceed."))
(unless elot-cli-bin-dir
  (error "Environment variable ELOT_BIN_DIR is not set! Cannot locate robot.jar."))

;; Define key directories relative to the root
(defvar elot-cli-lisp-dir (expand-file-name "elot-package" elot-cli-root-dir)
  "Directory containing the core ELOT Lisp files.")
(defvar elot-cli-lib-dir (expand-file-name "lib" elot-cli-root-dir)
  "Directory containing downloaded Elisp dependencies.")

;; Validate key directories
(unless (file-directory-p elot-cli-lisp-dir)
  (error "ELOT Lisp directory not found at expected location: %s" elot-cli-lisp-dir))
(unless (file-directory-p elot-cli-lib-dir)
  (error "Dependency 'lib' directory not found at expected location: %s. Run setup script?" elot-cli-lib-dir))

;; --- Package and Load Path Initialization ---

;; Set package-user-dir to the 'lib' directory BEFORE initializing
(setq package-user-dir elot-cli-lib-dir)

;; Initialize the package system to recognize packages in 'lib/'
;; This automatically adds subdirs in 'lib/' (like lib/dash-X.Y.Z/) to load-path
(package-initialize)
(message "Initialized package system with package-user-dir: %s" package-user-dir)

;; Explicitly add the core ELOT Lisp directory to load-path
(add-to-list 'load-path elot-cli-lisp-dir)
(message "Added core ELOT directory to load-path: %s" elot-cli-lisp-dir)

;; --- Load Required Libraries ---
;; Load Org mode core features needed by the functions below
(message "Loading required base libraries (Org)...")
(require 'org)
(require 'ob-tangle)
(require 'ox-html)
;; Load ELOT itself. This should pull in its own dependencies (like dash, ht, hydra)
;; from the 'lib/' directory via the initialized package system.
(require 'elot)
;; (require 'elot-label-display) ;; Load explicitly if elot doesn't, but needed
(message "ELOT system loaded.")


;; --- Configure ROBOT Path ---
;; Use the logic established previously to find robot.jar via ELOT_BIN_DIR
;; This sets the 'elot-robot-jar-path' variable needed by elot-robot-omn-to-ttl etc.
(message "Locating robot.jar...")
(if (not elot-cli-bin-dir) ; Re-check just in case
    (error "ELOT_BIN_DIR environment variable not set. Cannot locate robot.jar.")
  (let* ((absolute-bin-dir (expand-file-name elot-cli-bin-dir))
         (potential-jar-path (expand-file-name "robot.jar" absolute-bin-dir)))
    (unless (file-directory-p absolute-bin-dir)
      (error "ELOT_BIN_DIR does not point to a valid directory: %s" absolute-bin-dir))
    (if (file-exists-p potential-jar-path)
        (progn
          ;; For interactive use, elot.el defines elot-robot-jar-path via defcustom
          (setq elot-robot-jar-path potential-jar-path)
          (message "ROBOT JAR located successfully at: %s" elot-robot-jar-path))
      (error "robot.jar was not found in the specified bin directory: %s (Expected: %s)"
             absolute-bin-dir potential-jar-path))))


;; --- ENABLE Debugging ---
;; (setq debug-on-error t)
;; (setq message-log-max (* 1024 1024)) ; Optional: Increase message log size

;; --- Ingest Core ELOT Library of Babel Definitions ---
(let ((elot-lob-path (concat (file-name-directory (locate-library "elot")) "elot-lob.org")))
  (if (file-exists-p elot-lob-path)
      (progn
        (message "Ingesting ELOT library definitions from: %s" elot-lob-path)
        (org-babel-lob-ingest elot-lob-path))
    (warn "Could not find ELOT library file to ingest: %s" elot-lob-path)))

;; --- Disable Interactive Prompts ---
(setq org-confirm-babel-evaluate nil) ; Never prompt for babel execution in batch

;; ============================================================================
;; --- Core Processing Functions (Copied from older script) ---
;; ============================================================================

(defun elot-batch-tangle-file (input-file)
  "Tanglke INPUT-FILE in batch mode.
Order: Change CWD, Open file, Load elot-defaults, Tangle buffer.
Assumes INPUT-FILE is an absolute path and exists.
Assumes '# -*- eval: ... -*-' line is removed from Org file."
  (message "[Tangle Function] Starting for: %s" input-file)
  (let* ((buffer-to-kill nil) ; Variable to hold the buffer
         (original-dir default-directory)
         (success nil)) ; Track if process completed without error
    (unwind-protect ; Ensure directory restoration & buffer kill
        (progn
          (cd (file-name-directory input-file)) ; Change CWD for relative paths
          (message "[Tangle Function] Changed working directory to: %s" default-directory)

          (condition-case err ; Catch errors during the main process
              (progn
                ;; 1. Open file normally, but don't display it
                (message "[Tangle Function] Opening file normally (noselect): %s" input-file)
                (setq buffer-to-kill (find-file-noselect input-file)) ; Returns the buffer

                ;; 2. Execute next steps within the context of the opened buffer
                (with-current-buffer buffer-to-kill
                  ;; 2a. Load elot-defaults NOW, while buffer is current
                  ;; Assumes elot-defaults.el is in elot-cli-lisp-dir added to load-path
                  (message "[Tangle Function] Loading elot-defaults library for buffer %s..." (buffer-name))
                  (load-library "elot-defaults")

                  ;; 2b. Tangle the buffer
                  (message "[Tangle Function] Tangling buffer %s..." (buffer-name))
                  (org-babel-tangle) ; Tangles the current buffer
                  (message "[Tangle Function] Tangling complete for %s." (buffer-name)))

                ;; If we got here without error, mark as success
                (setq success t)
                (message "[Tangle Function] Process completed successfully for %s." input-file))

            ;; Error handler for condition-case
            ;; Re-signal the error so batch mode exits non-zero
            (error (signal (car err) (cdr err))))) ; Pass original error type and data

      ;; Cleanup part of unwind-protect: Kill buffer and restore CWD
      (when (buffer-live-p buffer-to-kill)
        (message "[Tangle Function] Killing buffer %s." (buffer-name buffer-to-kill))
        (kill-buffer buffer-to-kill))
      (cd original-dir)
      (message "[Tangle Function] Restored working directory to: %s" default-directory))
    success)) ; Return success status


(defun elot-batch-export-html (input-file output-file)
  "Export INPUT-FILE to OUTPUT-FILE (HTML) in batch mode.
Assumes INPUT-FILE and OUTPUT-FILE are absolute paths.
Ensures output directory exists."
  (message "[Export Function] Starting HTML export for: %s -> %s" input-file output-file)
  (let* ((output-dir (file-name-directory output-file))
         (buffer-to-kill nil)
         (original-dir default-directory)
         (success nil))
    (unless (file-directory-p output-dir)
      (message "[Export Function] Creating output directory: %s" output-dir)
      (make-directory output-dir t))

    (unwind-protect ; Ensure directory restoration & buffer kill
        (progn
          (cd (file-name-directory input-file)) ; Change CWD for relative includes etc.
          (message "[Export Function] Changed working directory to: %s" default-directory)

          (condition-case err ; Catch errors during export
              (progn
                (setq buffer-to-kill (find-file-noselect input-file))
                (with-current-buffer buffer-to-kill
                  (message "[Export Function] Exporting buffer %s to HTML..." (buffer-name))
                  ;; --- Org Export Customizations can go here ---
                  (let ((org-html-validation-link nil)) ; Example
                    ;; The actual export command for the current buffer
                    (org-html-export-to-html nil nil nil output-file))
                   ;; --- End Customizations ---
                  ) ; End with-current-buffer
                (setq success t)
                (message "[Export Function] HTML export completed successfully for %s." input-file))

            ;; Error handler - Re-signal the error
            (error (signal (car err) (cdr err)))))

      ;; Cleanup
      (when (buffer-live-p buffer-to-kill)
        (message "[Export Function] Killing buffer %s." (buffer-name buffer-to-kill))
        (kill-buffer buffer-to-kill))
      (cd original-dir)
      (message "[Export Function] Restored working directory to: %s" default-directory))
    success))

;; ============================================================================
;; --- Command Dispatch ---
;; ============================================================================

(let* (;; Get args, handle "--"
       (raw-args command-line-args-left)
       (args-left (if (and raw-args (string= (car raw-args) "--"))
                      (cdr raw-args)
                    raw-args))
       (command (car-safe args-left))
       (command-args (cdr-safe args-left)))


  ;; --- Input Validation ---
  (unless command
     (error "No command provided. Usage: elot-run <command> [args...]"))
  (unless (member command '("tangle" "export-html")) ;; Add other valid commands here
    (error "Invalid command: '%s'. Supported: tangle, export-html" command))

  (message "Received command: '%s' with args: %S" command command-args)

  ;; --- Command Execution ---
  (cond
   ;; --- Tangle Command ---
   ((string= command "tangle")
    (unless (= (length command-args) 1)
      (error "Usage for tangle: tangle <input-org-file>. Got %d args: %S" (length command-args) command-args))
    (let* ((input-file-raw (car command-args))
           (input-file-abs (expand-file-name input-file-raw)))
      (unless (file-exists-p input-file-abs)
        (error "Input file not found: %s (from %s)" input-file-abs input-file-raw))
      (elot-batch-tangle-file input-file-abs)
      (message "Tangle command finished for: %s" input-file-abs)
      ))

   ;; --- Export HTML Command ---
   ((string= command "export-html")
    ;; **MODIFIED: Expect only 1 argument**
    (unless (= (length command-args) 1)
      (error "Usage for export-html: export-html <input-org-file>. Got %d args: %S" (length command-args) command-args))
    (let* ((input-file-raw (car command-args))
           (input-file-abs (expand-file-name input-file-raw))
           ;; **MODIFIED: Derive output file automatically**
           (output-file-abs (concat (file-name-directory input-file-abs) ; Same directory
                                    (file-name-sans-extension (file-name-nondirectory input-file-abs))
                                    ".html")))
      (unless (file-exists-p input-file-abs)
        (error "Input file not found: %s (from %s)" input-file-abs input-file-raw))
      ;; Call the export function with the derived output path
      (elot-batch-export-html input-file-abs output-file-abs)
      (message "Export command finished for: %s -> %s" input-file-abs output-file-abs) ; Log derived output
      ))

   ;; --- Unknown Command (Shouldn't happen due to validation above) ---
   (t (error "Internal error: Unknown command '%s' reached execution." command)))
 )

;; Emacs will exit automatically after running the script in batch mode
(message "ELOT processing finished.")
