;;; elot-defaults.el --- Emacs Literate Ontology Tool (ELOT): Default settings   -*- lexical-binding: t; no-native-compile: t; -*-

;; Copyright (C) 2024, 2025, 2026 Johan W. Klüwer

;; Author: Johan W. Klüwer <johan.w.kluwer@gmail.com>
;; URL: https://github.com/johanwk/elot
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Backward-compatible shim for ELOT.
;;
;; Older ELOT Org files load this file via a file-local variable:
;;
;;   # -*- eval: (load-library "elot-defaults") -*-
;;
;; This now simply loads the main `elot' library.

;;; Code:

;; [[file:../elot-defs.org::src-settings-defaults][src-settings-defaults]]
(load-library "elot")
;; src-settings-defaults ends here

(provide 'elot-defaults)
;;; elot-defaults.el ends here
