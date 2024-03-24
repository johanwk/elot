;; [[file:../elot-defs.org::*Default settings][Default settings:1]]
;; default settings, replaces Local Variables block
(setq-local
 org-confirm-babel-evaluate nil
 org-export-allow-bind-keywords t
 org-babel-default-inline-header-args '((:exports . "code"))
 org-latex-src-block-backend 'listings
 org-latex-prefer-user-labels t
 org-latex-image-default-scale .8
 time-stamp-line-limit 100
 time-stamp-format "%Y-%m-%d %H:%M"
 time-stamp-active t
 time-stamp-start "(version of "
 time-stamp-end ")"
 org-startup-folded 'show2levels
 org-export-with-sub-superscripts nil  ; preserve "_"
 org-export-headline-levels 8  ; deep numbering
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
           "\\RedeclareSectionCommand[level=\\numexpr\\subsubsubsectionnumdepth+1\\relax,toclevel=\\numexpr\\subsubsubsectiontocdepth+1\\relax,"
           "  ]{paragraph}"
           "\\RedeclareSectionCommand[level=\\numexpr\\subsubsubsectionnumdepth+2\\relax,toclevel=\\numexpr\\subsubsubsectiontocdepth+2\\relax,"
           "  ]{subparagraph}"
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
           "\\hypersetup{pdfborder=0 0 0}"
           "\\lstdefinelanguage{omn}{basicstyle=\\small\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}"
           "\\lstdefinelanguage{ttl}{basicstyle=\\footnotesize\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}"
           "\\lstdefinelanguage{sparql}{basicstyle=\\footnotesize\\ttfamily,commentstyle=\\color{gray},frame=single,breaklines=true,breakatwhitespace=true,postbreak=\\mbox{{\\color{gray}\\tiny$\\rightarrow$}},tabsize=2,comment=[l]{\\#},columns=fullflexible,}"
           ))
 )
(progn
  (require 'elot)
  (update-link-abbrev)
  (org-babel-lob-ingest "elot-lob.org")
  (org-cycle-set-startup-visibility)
  (add-to-list 'org-latex-classes
               '("elot-scrreprt"
                 "\\documentclass[11pt,a4paper,numbers=noenddot,twoside=false]{scrreprt}
[DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ontology-resource-section
                 ))
  )
;; Default settings:1 ends here
