;;; elot-defaults.el --- Emacs Literate Ontology Tool (ELOT): Default settings   -*- lexical-binding: t; no-native-compile: t; -*-

;; Copyright (C) 2024, 2025 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Default startup settings for ELOT.
;;
;; This file is intended to be loaded when an ELOT org-mode file is
;; opened.  To achieve this, add the following text to the first line
;; of the ELOT file:
;;
;;   # -*- eval: (load-library "elot-defaults") -*-

;;; Code:

;; [[file:../elot-defs.org::src-settings-defaults][src-settings-defaults]]
;; default settings, replaces Local Variables block
(require 'ox-latex)
(setq-local
 org-confirm-babel-evaluate nil
 org-export-allow-bind-keywords t
 org-babel-default-inline-header-args '((:exports . "code"))
 org-latex-src-block-backend 'listings
 org-latex-prefer-user-labels t
 org-latex-image-default-scale .8
 tempo-interactive t
 time-stamp-line-limit 100
 time-stamp-format "%Y-%m-%d %H:%M"
 time-stamp-active t
 time-stamp-start "(version of "
 time-stamp-end ")"
 org-startup-folded 'show2levels
 org-export-with-sub-superscripts nil  ; preserve "_"
 org-export-headline-levels 8  ; deep numbering
 org-export-with-section-numbers 8  ; deep numbering
 org-latex-default-class "elot-scrreprt"
 org-latex-packages-alist
 (append org-latex-packages-alist
         '(("" "svg" t)
           ("" "enumitem" t)
           "\\setlist[description]{font=\\normalfont\\itshape\\space}"
           "\\sloppy"
           ;; subsubsubsection, see https://tex.stackexchange.com/questions/356567/subsubsubsection-for-scrbook
           "\\DeclareNewSectionCommand[style=section,counterwithin=subsubsection,afterskip=1.5ex plus .2ex,"
           "  beforeskip=3.25ex plus 1ex minus .2ex,afterindent=false,level=\\paragraphnumdepth,tocindent=10em,"
           "  tocnumwidth=5em]{subsubsubsection}"
           "\\RedeclareSectionCommand[level=\\numexpr\\subsubsubsectionnumdepth+1\\relax,toclevel=\\numexpr\\subsubsubsectiontocdepth+1\\relax,]{paragraph}"
           "\\RedeclareSectionCommand[level=\\numexpr\\subsubsubsectionnumdepth+2\\relax,toclevel=\\numexpr\\subsubsubsectiontocdepth+2\\relax,]{subparagraph}"
           "\\RedeclareSectionCommand[counterwithin=subsubsubsection,tocnumwidth=6em]{paragraph}"
           "\\RedeclareSectionCommand[tocnumwidth=7em]{subparagraph}"
           ;; section numbers in margin
           "\\RedeclareSectionCommands[runin=false,afterskip=1.5ex plus .2ex,afterindent=false,indent=0pt]{paragraph,subparagraph}"
           "\\renewcommand\\othersectionlevelsformat[3]{\\makebox[0pt][r]{#3\\autodot\\enskip}}"
           "\\renewcommand\\sectionformat{\\makebox[0pt][r]{\\thesection\\autodot\\enskip}}"
           "\\renewcommand\\subsectionformat{\\makebox[0pt][r]{\\thesubsection\\autodot\\enskip}}"
           "\\renewcommand\\subsubsectionformat{\\makebox[0pt][r]{\\thesubsubsection\\autodot\\enskip}}"
           "\\renewcommand\\subsubsubsectionformat{\\makebox[0pt][r]{\\thesubsubsubsection\\autodot\\enskip}}"
           "\\renewcommand\\paragraphformat{\\makebox[0pt][r]{\\theparagraph\\autodot\\enskip}}"
           "\\renewcommand\\subparagraphformat{\\makebox[0pt][r]{\\thesubparagraph\\autodot\\enskip}}"
           "\\PassOptionsToPackage{hyphens}{url}"
           "\\hypersetup{pdfborder=0 0 0,colorlinks=true}"
           "\\lstdefinelanguage{omn}{basicstyle=\\small\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}"
           "\\lstdefinelanguage{ttl}{basicstyle=\\footnotesize\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}"
           "\\lstdefinelanguage{sparql}{basicstyle=\\footnotesize\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}")))
(progn
  (org-cycle-set-startup-visibility)
  (load-library "elot")
  (org-babel-lob-ingest (concat (file-name-directory (locate-library "elot")) "elot-lob.org"))
  (elot-update-link-abbrev)
  (add-to-list 'org-latex-classes
               '("elot-scrreprt"
                 "\\documentclass[11pt,a4paper,numbers=noenddot,twoside=false]{scrreprt}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 elot-ontology-resource-section))
  ;;
  (modify-syntax-entry ?\: "w")
  (modify-syntax-entry ?\_ "w")
  (add-hook 'org-babel-post-tangle-hook 'elot-tangled-omn-to-ttl
            'local) ;; make it a local hook only
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images 'local)
  (declare-function elot-update-link-abbrev "elot.el")
  (add-hook 'after-save-hook 'elot-update-link-abbrev)
  ;; the label display functions are in a separate file
  (require 'elot-label-display)
  (declare-function elot-label-display-setup "elot-label-display.el")
  (elot-label-display-setup)
  (declare-function elot-hydra/body "elot.el")
  (local-set-key (kbd "<f4>") #'elot-hydra/body)
  (declare-function elot-toggle-label-display "elot-label-display.el")
  (local-set-key (kbd "<f5>") #'elot-toggle-label-display))
;; src-settings-defaults ends here

(provide 'elot-defaults)
;;; elot-defaults.el ends here
