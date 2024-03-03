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

(dolist (package '(org
		   org-ql
		   dash s f ht)
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

;; allow all evaluations of source blocks without confirmation
(setq org-confirm-babel-evaluate nil)

;; allow BIND for attr_latex settings
(setq org-export-allow-bind-keywords t)

;; utility, get contents of named block
;; (probably obsolete, replace with recent org function)
(defun expand-named-babel-block (block)
  (save-excursion
    (org-babel-goto-named-src-block block)
    (org-babel-expand-src-block))
  )

