;; [[file:../elot-defs.org::*Default settings][Default settings:1]]
;; default settings, replaces Local Variables block
(setq-local
 org-confirm-babel-evaluate nil
 org-export-allow-bind-keywords t
 org-babel-default-inline-header-args '((:exports . "code"))
 org-latex-src-block-backend 'listings
 org-latex-classes '(("book" "\\documentclass[10pt]{scrbook}" ontology-resource-section))
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
 )
(progn
  (require 'elot)
  (update-link-abbrev)
  (org-babel-lob-ingest "elot-lob.org")
  (org-cycle-set-startup-visibility)
  )
;; Default settings:1 ends here
