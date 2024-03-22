; -*- coding: utf-8; lexical-binding: t; -*-

;; init file for ELOT, to see what is required for making an ontology and document
;; borrowing from http://xahlee.info/emacs/emacs/emacs_sample_init.el

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(dolist (package '(htmlize
                   ))
 (unless (package-installed-p package)
   (package-install package)
   (require package)))

(require 'ob-lob) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions

;; only emacs-lisp is needed for minimal setup
;; (org-babel-do-load-languages
;;       'org-babel-load-languages
;;       '((emacs-lisp . t) (plantuml . t)))

;; only needed when we get to rdfpuml
;; need direct pointer to plantuml
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
