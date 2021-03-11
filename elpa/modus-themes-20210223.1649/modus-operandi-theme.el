;;; modus-operandi-theme.el --- Accessible light theme (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (C) 2019-2021  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 1.1.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Modus Operandi is the light variant of the Modus themes (Modus
;; Vivendi is the dark one).  The themes are designed for color-contrast
;; accessibility.  More specifically:
;;
;;     1. Provide a consistent minimum contrast ratio between background
;;     and foreground values of 7:1 or higher.  This meets the highest
;;     such accessibility criterion per the guidelines of the Worldwide
;;     Web Consortium's Working Group on Accessibility (WCAG AAA
;;     standard).
;;
;;     2. Offer as close to full face coverage as possible.  The list is
;;     already quite long, with more additions to follow as part of the
;;     ongoing development process.
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-themes.el            (Main code shared between the themes)
;; - modus-vivendi-theme.el     (Dark theme)

;;; Code:



(require 'modus-themes)

(deftheme modus-operandi
  "Light theme that conforms with the highest accessibility
  standard for color contrast between background and foreground
  elements (WCAG AAA).")

(modus-themes-theme modus-operandi)

(provide-theme 'modus-operandi)

(provide 'modus-operandi-theme)

;;; modus-operandi-theme.el ends here
