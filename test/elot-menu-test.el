;;; elot-menu-test.el --- Tests for Step 1.10  -*- lexical-binding: t; -*-

;; Usage:  make -C test menu-test  (or)
;;         cd test && emacs --batch -L ../elot-package -L . \
;;            -l elot-menu-test.el \
;;            -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.10: the ELOT menu gains a `Labels & sources' submenu with
;; three groups: /Source registration/, /Active sources (per buffer
;; / project)/, and /Display/.  These tests exercise the menu
;; structure without opening a GUI.
;;
;; Implementation note: `easy-menu-define' stores the menu as a
;; keymap in the symbol's value slot (the descriptor is compiled
;; away).  So the tests walk the keymap.  Menu entries are of the
;; form
;;
;;     (menu-item NAME TARGET . PROPS)
;;
;; where TARGET is either a command symbol or a submenu keymap.
;; `:style toggle :selected EXPR' becomes `:button (:toggle . EXPR)'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'pcase)

(let* ((this-file (or load-file-name buffer-file-name))
       (repo-root (file-name-directory
                   (directory-file-name
                    (file-name-directory this-file)))))
  (add-to-list 'load-path (expand-file-name "elot-package" repo-root))
  (add-to-list 'load-path (file-name-directory this-file)))

(require 'easymenu)
(require 'elot-db)
(require 'elot-sources)
(require 'elot-label-display)
(require 'elot-mode)

;;; ---------------------------------------------------------------------------
;;; Helpers that walk the `elot-menu' keymap
;;; ---------------------------------------------------------------------------

(defun elot-menu-test--root ()
  "Return the ELOT menu as a keymap."
  (should (boundp 'elot-menu))
  (let ((v (symbol-value 'elot-menu)))
    (should (keymapp v))
    v))

(defun elot-menu-test--entries (km)
  "Return a list of (NAME TARGET PROPS) for each menu-item in keymap KM.
TARGET is either a command symbol or a sub-keymap.  Does not recurse."
  (let (acc)
    (map-keymap
     (lambda (_event binding)
       (when (and (consp binding) (eq (car binding) 'menu-item))
         (let ((name   (nth 1 binding))
               (target (nth 2 binding))
               (props  (nthcdr 3 binding)))
           (push (list name target props) acc))))
     km)
    (nreverse acc)))

(defun elot-menu-test--find-submenu (km name)
  "Return the sub-keymap named NAME under KM, or nil."
  (cl-loop for (n target _p) in (elot-menu-test--entries km)
           when (and (stringp n) (string= n name) (keymapp target))
           return target))

(defun elot-menu-test--all-command-entries (km)
  "Recursively collect (NAME COMMAND PROPS) for every command item under KM."
  (let (acc)
    (dolist (entry (elot-menu-test--entries km))
      (pcase-let ((`(,name ,target ,props) entry))
        (cond
         ((keymapp target)
          (setq acc (append acc (elot-menu-test--all-command-entries target))))
         ((symbolp target)
          (push (list name target props) acc)))))
    acc))

(defun elot-menu-test--find-item-by-command (km cmd)
  "Return (NAME COMMAND PROPS) for the first item in KM whose command is CMD."
  (cl-find-if (lambda (e) (eq (nth 1 e) cmd))
              (elot-menu-test--all-command-entries km)))

(defun elot-menu-test--prop (props kw)
  "Return the value of keyword KW in PROPS (a plist), or nil."
  (plist-get props kw))

;;; ---------------------------------------------------------------------------
;;; Tests
;;; ---------------------------------------------------------------------------

(ert-deftest test-elot-menu-has-labels-submenu ()
  "The /Labels & sources/ submenu exists with its three groups."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources")))
    (should labels)
    (should (elot-menu-test--find-submenu labels "Source registration"))
    (should (elot-menu-test--find-submenu
             labels "Active sources (per buffer / project)"))
    (should (elot-menu-test--find-submenu labels "Display"))))

(ert-deftest test-elot-menu-entry-points-resolve ()
  "Every command symbol referenced from the /Labels & sources/ submenu
is `fboundp'."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources"))
         (cmds   (mapcar (lambda (e) (nth 1 e))
                         (elot-menu-test--all-command-entries labels))))
    (should cmds)
    (dolist (c cmds)
      (should (symbolp c))
      (should (fboundp c)))))

(ert-deftest test-elot-menu-omits-set-active-sources ()
  "The Lisp-only bulk setter `elot-label-set-active-sources' is not
exposed in the menu (Step 1.10 spec)."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources")))
    (should-not
     (elot-menu-test--find-item-by-command
      labels 'elot-label-set-active-sources))))

(ert-deftest test-elot-menu-labels-submenu-commands-complete ()
  "All Step 1.10 commands are present in the submenu."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources"))
         (expected '(elot-label-register-current-buffer
                     elot-label-register-source
                     elot-label-unregister-source
                     elot-label-refresh-source
                     elot-label-refresh-all-sources
                     elot-label-list-sources
                     elot-label-activate-source
                     elot-label-deactivate-source
                     elot-label-list-active-sources
                     elot-label-move-source-up
                     elot-label-move-source-down
                     elot-global-label-display-mode
                     elot-global-label-display-setup
                     elot-toggle-label-display)))
    (dolist (c expected)
      (should (elot-menu-test--find-item-by-command labels c)))))

(ert-deftest test-elot-menu-enable-guard-respects-active-sources ()
  "The deactivate / move entries are disabled when
`elot-active-label-sources' is nil, and enabled when non-nil.
The `:enable' form is read from the menu-item properties and
evaluated directly (no GUI required)."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources"))
         (cmds   '(elot-label-deactivate-source
                   elot-label-move-source-up
                   elot-label-move-source-down))
         (saved  (and (boundp 'elot-active-label-sources)
                      elot-active-label-sources)))
    (unwind-protect
        (dolist (cmd cmds)
          (let* ((entry  (elot-menu-test--find-item-by-command labels cmd))
                 (enable (elot-menu-test--prop (nth 2 entry) :enable)))
            (should entry)
            (should enable)
            (let ((elot-active-label-sources nil))
              (should-not (eval enable t)))
            (let ((elot-active-label-sources '(("foo" nil))))
              (should (eval enable t)))))
      (setq elot-active-label-sources saved))))

(ert-deftest test-elot-menu-enable-guard-respects-registered-sources ()
  "The unregister / refresh-source / refresh-all entries are disabled
when `elot-db-list-sources' is empty and enabled when non-empty.
`elot-db-list-sources' is stubbed via `cl-letf' to avoid touching
a real DB."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources"))
         (cmds   '(elot-label-unregister-source
                   elot-label-refresh-source
                   elot-label-refresh-all-sources)))
    (dolist (cmd cmds)
      (let* ((entry  (elot-menu-test--find-item-by-command labels cmd))
             (enable (elot-menu-test--prop (nth 2 entry) :enable)))
        (should entry)
        (should enable)
        (cl-letf (((symbol-function 'elot-db-list-sources)
                   (lambda (&rest _) nil)))
          (should-not (eval enable t)))
        (cl-letf (((symbol-function 'elot-db-list-sources)
                   (lambda (&rest _) '(("/tmp/fake.ttl" nil)))))
          (should (eval enable t)))))))

(ert-deftest test-elot-menu-display-entries-are-toggles ()
  "The two display-state commands are toggle buttons.
In keymap form, `:style toggle :selected EXPR' appears as
`:button (:toggle . EXPR)'."
  (let* ((root   (elot-menu-test--root))
         (labels (elot-menu-test--find-submenu root "Labels & sources")))
    (dolist (cmd '(elot-global-label-display-mode
                   elot-toggle-label-display))
      (let* ((entry  (elot-menu-test--find-item-by-command labels cmd))
             (button (elot-menu-test--prop (nth 2 entry) :button)))
        (should entry)
        (should (consp button))
        (should (eq (car button) :toggle))
        (should (cdr button))))))

(provide 'elot-menu-test)
;;; elot-menu-test.el ends here
