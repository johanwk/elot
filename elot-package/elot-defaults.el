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
