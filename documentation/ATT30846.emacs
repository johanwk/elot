;;
;; .emacs for testing elot with fresh Emacs install
;;
;; example startup batch file for Windows contains:
;;;; rem from https://stackoverflow.com/a/17149070/1540149
;;;; set HOME=%~dp0
;;;; C:\opt\emacs-29.2\bin\runemacs.exe --init-directory c:\Data\elot\testbareemacs --xrm "emacs.Background: bisque"


;; convenience
(setq inhibit-startup-message t)
(setq frame-title-format
      '("%S: " (buffer-file-name "%f" (dired-directory dired-directory "%b")))) 
(repeat-mode)

;; problem with gpg key for installing packages
;; to solve it, create manually the directory .emacs.d/elpa/gnupg
;; before installing packages
;; https://emacs.stackexchange.com/questions/60278/gpg-no-public-key
;; this is for testing only!
(make-directory "~/.emacs.d/elpa/gnupg" :parents)
(setq package-gnupghome-dir "~/.emacs.d/elpa/gnupg")

;; location of the elot package
(push (expand-file-name "~/../elot-package/") load-path)
(load-library "elot")

;; get packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(dolist (package '(htmlize
                   omn-mode
		   sparql-mode
                   ;; apparently not needed org-tempo
                   modus-themes  ;; good looks
		   general
                   orderless ;; easier to find commands
		   vertico
		   marginalia
		   consult
		   f
                   ))
 (unless (package-installed-p package)
   (package-install package)
   (require package)))

(load-theme 'modus-operandi-tritanopia t)

(require 'ob-lob) ; Library of Babel
(require 'ob-sparql) ; Library of Babel
(require 'ox) ; export functions
(require 'ol) ; link functions

;;;;
;;;; start -- Completion framework from zamansky "Using Emacs"
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

;(load-library "org-tempo")
(require 'tempo)
(setq tempo-interactive t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8a0eba8f06120605030161981032469d87a9832402e4441daa91296aa463f124" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" default))
 '(package-selected-packages
   '(sparql-mode f consult marginalia vertico general modus-themes orderless nano-modeline nano-theme omn-mode htmlize))
 '(safe-local-variable-values '((eval load-library "elot-defaults"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 100 :width normal :foundry "outline" :family "Consolas"))))
 '(fixed-pitch ((t (:height 0.85 :family "Consolas"))))
 '(org-block-background ((t (:inherit nil :background "snow"))))
 '(org-list-dt ((t (:inherit italic))))
 '(org-table ((t (:inherit modus-themes-fixed-pitch :foreground "#193668"))))
 '(stripe-highlight ((t (:background "#FFFDFD"))))
 '(variable-pitch ((t (:height 1.2 :family "Calibri")))))
