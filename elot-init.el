; -*- coding: utf-8; lexical-binding: t; -*-

;; init file for ELOT, to see what is required for making an ontology and document
;; borrowing from http://xahlee.info/emacs/emacs/emacs_sample_init.el

;; convenience
(setq inhibit-startup-message t)
(setq frame-title-format
      '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b")))) 
(repeat-mode)

;; get packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(dolist (package '(htmlize
                   omn-mode
                   ;; apparently not needed org-tempo
                   modus-themes  ;; good looks
                   orderless ;; easier to find commands
                   ))
 (unless (package-installed-p package)
   (package-install package)
   (require package)))

(require 'ob-lob) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions

(setq tempo-interactive t)
(load-theme 'modus-operandi-tritanopia t)

;; location of the elot package -- edit path to fit
(push (expand-file-name "~/elisp/elot-package/") load-path)
(load-library "elot")


;; only emacs-lisp is needed for minimal setup
;; (org-babel-do-load-languages
;;       'org-babel-load-languages
;;       '((emacs-lisp . t) (plantuml . t)))

;; only needed when we get to rdfpuml
;; need direct pointer to plantuml
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
