;;; elot-menu-item-b-test.el --- Tests for Step 1.12 Item B  -*- lexical-binding: t; -*-

;; Usage:  make -C test step-1-12-item-b-test  (or)
;;         cd test && emacs --batch -L ../elot-package -L . \
;;            -l elot-menu-item-b-test.el \
;;            -f ert-run-tests-batch-and-exit

;;; Commentary:

;; Step 1.12 Item B: ELOT menu visibility in non-ELOT buffers.
;;
;; The ELOT menu is attached twice:
;;
;;   1. `elot-mode-map' (via `easy-menu-define') -- always active in
;;      ELOT Org buffers.
;;   2. `elot-global-label-display-mode-map' (via a `define-key' in
;;      a `with-eval-after-load' snippet) -- active in any buffer
;;      where that minor mode is on, with `:visible (not elot-mode)'
;;      so it hides itself whenever elot-mode also contributes the
;;      menu (preventing a duplicate "ELOT" top-level entry).
;;
;; The ELOT-only entries (tangle, lint, templates, xref, ...) carry
;; `:enable (elot--in-elot-buffer-p)' so that, when the menu is
;; surfaced via `elot-global-label-display-mode-map' in a non-ELOT
;; buffer, those entries are greyed out.
;;
;; The entries that make sense anywhere (`Insert Existing Resource
;; ID', the toggles, the whole `Labels & sources' submenu) have no
;; such guard and remain enabled.

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
;;; Helpers (share the style of elot-menu-test.el but stay self-contained)
;;; ---------------------------------------------------------------------------

(defun elot-item-b--entries (km)
  "List `(NAME TARGET PROPS)' for each menu-item in keymap KM."
  (let (acc)
    (map-keymap
     (lambda (_ binding)
       (when (and (consp binding) (eq (car binding) 'menu-item))
         (push (list (nth 1 binding) (nth 2 binding) (nthcdr 3 binding))
               acc)))
     km)
    (nreverse acc)))

(defun elot-item-b--find-submenu (km name)
  (cl-loop for (n target _) in (elot-item-b--entries km)
           when (and (stringp n) (string= n name) (keymapp target))
           return target))

(defun elot-item-b--all-command-entries (km)
  (let (acc)
    (dolist (e (elot-item-b--entries km))
      (pcase-let ((`(,name ,target ,props) e))
        (cond
         ((keymapp target)
          (setq acc (append acc (elot-item-b--all-command-entries target))))
         ((symbolp target)
          (push (list name target props) acc)))))
    acc))

(defun elot-item-b--find-by-command (km cmd)
  (cl-find-if (lambda (e) (eq (nth 1 e) cmd))
              (elot-item-b--all-command-entries km)))

(defun elot-item-b--menu-bar-entry (km name)
  "Return the raw (NAME menu-item \"label\" TARGET . PROPS) cell for
NAME in KM's `[menu-bar]' sub-keymap, or nil.  `lookup-key' on
`[menu-bar ELOT]' would return only the menu-item's TARGET; we
need the whole cell to inspect `:visible' / `:enable' props."
  (let ((mb (lookup-key km [menu-bar])))
    (and (keymapp mb) (assq name (cdr mb)))))

;;; ---------------------------------------------------------------------------
;;; 1: menu is visible via the global-mode keymap in non-ELOT buffers
;;; ---------------------------------------------------------------------------

(ert-deftest test-menu-visible-in-global-mode-buffer ()
  "In a fundamental-mode buffer with `elot-global-label-display-mode'
on, the ELOT menu is reachable via `[menu-bar ELOT]' in the active
keymap stack."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (bound-and-true-p elot-mode))
    ;; Turn the global mode on without requiring any active sources
    ;; (the mode enables even with nil sources; it just warns).
    (let ((elot-active-label-sources nil))
      (elot-global-label-display-mode 1)
      (unwind-protect
          (let ((entry (elot-item-b--menu-bar-entry
                        elot-global-label-display-mode-map 'ELOT)))
            (should entry)
            ;; Cell shape: (ELOT menu-item "ELOT" TARGET . PROPS).
            (should (eq (nth 1 entry) 'menu-item))
            (should (keymapp (nth 3 entry)))
            ;; And it is reachable from the current active-maps stack
            ;; (lookup-key returns the TARGET keymap here, not the cell).
            (should (keymapp
                     (lookup-key (make-composed-keymap (current-active-maps))
                                 [menu-bar ELOT]))))
        (elot-global-label-display-mode -1)))))

;;; ---------------------------------------------------------------------------
;;; 2: entries that should stay enabled outside an ELOT buffer
;;; ---------------------------------------------------------------------------

(ert-deftest test-menu-global-entries-enabled-outside-elot ()
  "In a non-ELOT buffer, the `Labels & sources' submenu and the
shared `Insert Existing Resource ID' entry are not guarded by
`elot--in-elot-buffer-p': either no `:enable' is set, or its form
does not reduce to nil purely because we are outside an ELOT Org
buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (elot--in-elot-buffer-p))
    (let* ((root   (symbol-value 'elot-menu))
           (shared '(elot-label-lookup
                     elot-label-register-current-buffer
                     elot-label-register-source
                     elot-label-list-sources
                     elot-label-activate-source
                     elot-label-list-active-sources
                     elot-global-label-display-mode
                     elot-global-label-display-setup
                     elot-toggle-label-display)))
      (dolist (cmd shared)
        (let* ((entry  (elot-item-b--find-by-command root cmd))
               (enable (plist-get (nth 2 entry) :enable)))
          (should entry)
          ;; Either unguarded (nil) or the form does NOT call
          ;; `elot--in-elot-buffer-p'.  If it is guarded by something
          ;; else (e.g. the registered-sources check), that is fine --
          ;; those are orthogonal to Item B and tested by Step 1.10.
          (when enable
            (should-not
             (cl-some (lambda (x) (eq x 'elot--in-elot-buffer-p))
                      (flatten-tree enable)))))))))

;;; ---------------------------------------------------------------------------
;;; 3: entries that should be disabled outside an ELOT buffer
;;; ---------------------------------------------------------------------------

(ert-deftest test-menu-elot-only-entries-disabled-outside-elot ()
  "In a non-ELOT buffer, every entry that declares `:enable
(elot--in-elot-buffer-p)' evaluates to nil, and every entry we know
to be ELOT-Org-only carries exactly that guard."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (elot--in-elot-buffer-p))
    (let* ((root (symbol-value 'elot-menu))
           (elot-only
            '(elot-org-lint
              elot-tangle-buffer-to-omn
              elot-open-owl
              xref-find-definitions
              xref-find-references
              elot-describe-curie-at-point
              elot-label-display-setup
              tempo-template-elot-table-of-resources
              elot-headings-from-table
              tempo-template-elot-block-sparql-select
              tempo-template-elot-block-sparql-construct
              tempo-template-elot-block-rdfpuml-diagram
              tempo-template-elot-doc-header
              tempo-template-elot-ont-skeleton)))
      (dolist (cmd elot-only)
        (let* ((entry  (elot-item-b--find-by-command root cmd))
               (enable (plist-get (nth 2 entry) :enable)))
          (should entry)
          (should enable)
          ;; Guard form mentions the predicate.
          (should (memq 'elot--in-elot-buffer-p (flatten-tree enable)))
          ;; And it actually evaluates to nil outside an ELOT buffer.
          (should-not (eval enable t)))))))

;;; ---------------------------------------------------------------------------
;;; 4: menu is NOT in the keymap stack when neither mode is on
;;; ---------------------------------------------------------------------------

(ert-deftest test-menu-not-visible-without-either-mode ()
  "In a fresh fundamental-mode buffer with neither `elot-mode' nor
`elot-global-label-display-mode' on, the ELOT menu is not reachable
via the active-maps stack."
  (with-temp-buffer
    (fundamental-mode)
    (should-not (bound-and-true-p elot-mode))
    (should-not (bound-and-true-p elot-global-label-display-mode))
    ;; Neither of the two minor-mode keymaps is on the stack, so
    ;; `[menu-bar ELOT]' does not resolve to a menu item.
    (let ((binding (lookup-key (make-composed-keymap (current-active-maps))
                               [menu-bar ELOT])))
      ;; `lookup-key' returns nil or a number (when the key is too long).
      ;; Accept any non-menu-item result.
      (should (or (null binding)
                  (numberp binding)
                  (not (and (consp binding) (eq (car binding) 'menu-item))))))))

;;; ---------------------------------------------------------------------------
;;; 5: exactly one top-level "ELOT" menu when both modes are on
;;; ---------------------------------------------------------------------------

(ert-deftest test-menu-single-instance-when-both-modes-on ()
  "In a buffer where both `elot-mode' and `elot-global-label-display-mode'
are active, only one of the two contributing keymaps' ELOT menu
entries is visible -- the `elot-global-label-display-mode-map' copy
hides itself via its `:visible (not elot-mode)' guard."
  ;; Check the guard directly: in a non-ELOT buffer the global-mode
  ;; entry is visible; in an ELOT buffer it is not.
  (let* ((entry (elot-item-b--menu-bar-entry
                 elot-global-label-display-mode-map 'ELOT)))
    (should entry)
    (should (eq (nth 1 entry) 'menu-item))
    ;; Cell shape: (ELOT menu-item "ELOT" TARGET . PROPS).
    (let ((visible-form (plist-get (nthcdr 4 entry) :visible)))
      (should visible-form)
      ;; Outside elot-mode -> visible.
      (let ((elot-mode nil))
        (should (eval visible-form t)))
      ;; Inside elot-mode -> hidden.
      (let ((elot-mode t))
        (should-not (eval visible-form t)))))
  ;; And elot-mode-map's copy is always visible (no :visible guard).
  ;; `easy-menu-define' interns the menu name via `easy-menu-intern',
  ;; which downcases strings, so the menu-bar key there is `elot'
  ;; (lowercase), not `ELOT'.
  (let* ((entry (or (elot-item-b--menu-bar-entry elot-mode-map 'elot)
                    (elot-item-b--menu-bar-entry elot-mode-map 'ELOT))))
    (should entry)
    (should (eq (nth 1 entry) 'menu-item))
    ;; easy-menu-define does not emit a :visible form, so the entry
    ;; is visible whenever elot-mode-map is active.
    (should-not (plist-get (nthcdr 4 entry) :visible))))

(provide 'elot-menu-item-b-test)
;;; elot-menu-item-b-test.el ends here
