; -*- coding: utf-8; lexical-binding: t; -*-

;; Initialization file for ELOT, loading what is required for making
;; an ontology and document.
;; 
;; If you are new to Emacs, you can save this file as ".emacs" in your
;; home directory, then restart Emacs.

;; convenience
(setq inhibit-startup-message t)
(setq frame-title-format
      '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b")))) 
(repeat-mode)

;; get packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;;; The following may be necessary for package installation in Windows
;;; (if a problem with gpg key for installing packages)
;;; create manually the directory .emacs.d/elpa/gnupg
;;; before installing packages
;;; https://emacs.stackexchange.com/questions/60278/gpg-no-public-key
;;;
;; (make-directory "~/.emacs.d/elpa/gnupg" :parents)
;; (setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg")


;; various packages
(dolist (package '(htmlize
                   omn-mode
                   sparql-mode
                   plantuml-mode
                   org-ql
                   ;; the following are completely optional
                   modus-themes
                   general
                   orderless
                   vertico
                   marginalia
                   consult
                   ))
 (unless (package-installed-p package)
   (package-install package)
   (require package)))

(require 'ob-lob) ; Library of Babel
(require 'ob-sparql)
(require 'ox) ; export functions
(require 'ol) ; link functions
(setq tempo-interactive t)

;; location of the elot package -- edit path to fit
(push (expand-file-name "~/elisp/elot/elot-package/") load-path)
(load-library "elot")

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


