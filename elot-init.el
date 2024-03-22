; -*- coding: utf-8; lexical-binding: t; -*-

;; init file for ELOT, to see what is required for making an ontology and document
;; borrowing from http://xahlee.info/emacs/emacs/emacs_sample_init.el


(setq inhibit-startup-message t)

(set-language-environment "utf-8")
(set-default-coding-systems 'utf-8-unix)
(require 'recentf)
(recentf-mode 1)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(dolist (package '(;; the standard works: org
		               ;; make optional: org-ql
		               ;; dash
                   ;; replaced with string-trim: s
                   ;; f ht)
                   ob-lob
	               )
 (unless (package-installed-p package)
   (package-install package))
   (require package))

(require 'ox) ; export functions
(require 'ol) ; link functions

(org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)(plantuml . t)))

;; need direct pointer to plantuml
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; allow BIND for attr_latex settings
(setq org-export-allow-bind-keywords t)

