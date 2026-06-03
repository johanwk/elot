; -*- coding: utf-8; lexical-binding: t; -*-

;; Initialization file for ELOT, loading some typical supporting
;; packages.
;;
;; If you are new to Emacs, you can save this file as ".emacs" in your
;; home directory, then restart Emacs. Please see below for external
;; software programs that you should install if you plan to actually
;; use Elot!

;; If you have cloned Elot from Git, a command line like the following
;; will allow you to run a test without disturbing your existing Emacs
;; installation (here, for a Windows terminal). Note, if you see a
;; "gpg" error after Emacs opens, also uncomment the line further down
;; in this file saying "(package-check-signature nil)".
;;
;;  emacs -Q --init-directory c:/tmp/elot-test -l elot-init.el

;; convenience
(setq frame-title-format
      '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b")))) 
(repeat-mode)

;; get packages
;; We add MELPA (snapshot/latest) so that `elot' tracks the development
;; tip.  If you prefer the frozen release, use melpa-stable instead:
;;   ("melpa-stable" . "https://stable.melpa.org/packages/")

;; If you are on Windows and don't have gpg installed, you may well
;; see a "No public key" error that blocks loading packages. You may
;; then uncomment the following line to evade the checks; only
;; recommended for testing.
;; (package-check-signature nil)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))

;;; The following is likely necessary for package installation in
;;; Windows (avoids a problem with gpg key for installing packages):
;;; create the directory .emacs.d/elpa/gnupg before installing
;;; packages, see
;;; https://emacs.stackexchange.com/questions/60278/gpg-no-public-key
;;;
(make-directory (expand-file-name "elpa/gnupg" user-emacs-directory) :parents)
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))


;; various packages used by the full ELOT environment.
;; `elot' itself is now on MELPA, so we install it alongside its
;; companions rather than loading it from a local checkout.
(dolist (package '(elot
                   htmlize
                   omn-mode
                   sparql-mode
                   plantuml-mode
                   org-ql
                   ;; the following are for a modern Emacs interface (optional)
                   modus-themes
                   general
                   orderless
                   vertico
                   marginalia
                   consult
                   ))
 (unless (package-installed-p package)
   (package-install package)))

(require 'ob-lob) ; Library of Babel
(require 'ob-sparql)
(require 'ox) ; export functions
(require 'ol) ; link functions
(setq tempo-interactive t)

;; Load ELOT from the installed MELPA package.
;;
;; If you instead want to hack on a local checkout of ELOT, don't load
;; the package, but comment uncomment the following line; this is only
;; an EXAMPLE -- adjust the path to wherever you have your Elot
;; repository clone:
;; (push (expand-file-name "~/src/elot/elot-package/") load-path)
(require 'elot-mode)

;; ELOT mode will now automatically activate for ontology files.
;; You can also manually enable it with M-x elot-mode.

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t) (plantuml . t)))

;; Queries/statistics and diagrams in ELOT rely on external software
;; programs, which need to be installed separately.
;;
;; the following variables then need to be edited with M-x customize
;; - elot-robot-jar-path    (for ROBOT query and more)
;; - org-plantuml-jar-path  (for PlantUML)
;; - elot-rdfpuml-path      (for RDF diagrams)
;;
;; The line below is only an EXAMPLE -- adjust the path to wherever
;; you have installed plantuml.jar on your system (or remove it and
;; set the variable via M-x customize).
(setq org-plantuml-jar-path (expand-file-name "~/bin/plantuml/plantuml.jar"))



;; ----------------------------------------------------------------------
;;  The following is entirely optional. Gives a reasonable setup for
;;  new Emacs users.
;;  ----------------------------------------------------------------------

(load-theme 'modus-operandi-tritanopia t)

(setq warning-minimum-level :error)

(use-package general)  ; a dependency

(use-package vertico
  :init
  (vertico-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))


(use-package marginalia
  :config (marginalia-mode))

(use-package consult
  :general
  ("M-y" 'consult-yank-from-kill-ring
   "C-x b" 'consult-buffer
   "M-g i" 'consult-imenu))

(recentf-mode)

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)


