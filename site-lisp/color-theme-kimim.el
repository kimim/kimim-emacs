;;; color-theme-kimim.el --- Kimim color theme for GNU Emacs.

;; Copyright (C) 2014 Kimi MA <jjmmma@gmail.com>

;; Author:Kimi MA
;; Keywords: faces local color theme of kimim's preference
;; URL: http://github.com/kimim/color-theme-kimim
;; Version: 0.0.1
;; Package-Requires: ((color-theme "6.6.1"))

;;; License:

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Code:

;; color theme (requires http://www.emacswiki.org/cgi-bin/wiki?ColorTheme )
(require 'color-theme)

(defun color-theme-kimim ()
  "Customize based on Eclipse default color.
Black foreground on white background.
Includes faces for font-lock, widget, custom, speedbar."
  (interactive)
  (color-theme-install
   '(color-theme-kimim
     ((foreground-color . "black")
      (background-color . "white")
      (mouse-color . "sienna3")
      (cursor-color . "orange")
      (border-color . "Blue")
      (background-mode . light))
     (default ((t (nil))))
     (hl-line ((t (:background "gray90"))))
     (mode-line ((t (:background "gainsboro" :foreground "black"
                                 :box (:line-width -1 :style released-button) :height 0.85))))
     (mode-line-inactive ((t (:background "gainsboro" :foreground "RoyalBlue"
                                          :box (:line-width -1 :style released-button) :height 0.85))))
     (modeline-buffer-id ((t (:background "gainsboro" :foreground "blue4" :weight bold))))
     (modeline-mousable ((t (:background "gainsboro" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:background "gainsboro" :foreground "green4"))))
     (which-func  ((t (:background "gainsboro" :foreground "green4"))))
     (highlight ((t (:foreground nil :background "darkseagreen2"))))
     (bold ((t (:bold t))))
     (italic ((t (:italic t))))
     (bold-italic ((t (:bold t :italic t))))
     (region ((t (:foreground nil :background "LightBlue"))))
     (secondary-selection ((t (:background "paleturquoise"))))
     (underline ((t (:underline t))))
     (lazy-highlight-face ((t (:foreground "dark magenta" :bold t))))
     (font-lock-comment-face ((t (:foreground "ForestGreen" :italic t))))
     (font-lock-if0-face ((t (:foreground "gray30"))));; for #if 0 block
     (font-lock-string-face ((t (:foreground "RoyalBlue3"))))
     (font-lock-keyword-face ((t (:foreground "dark magenta" :bold t))))
     (font-lock-builtin-face ((t (:foreground "black"))))
     (font-lock-function-name-face ((t (:foreground "MediumBlue"))))
     (font-lock-variable-name-face ((t (:foreground "blue" :italic t))))
     (font-lock-type-face ((t (:foreground "purple4"))))
     (font-lock-constant-face ((t (:foreground "CadetBlue"))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (widget-documentation-face ((t (:foreground "dark green"))))
     (widget-button-face ((t (:bold t))))
     (widget-field-face ((t (:background "gray85"))))
     (widget-single-line-field-face ((t (:background "gray85"))))
     (widget-inactive-face ((t (:foreground "dim gray"))))
     (widget-button-pressed-face ((t (:foreground "red"))))
     (custom-invalid-face ((t (:foreground "yellow" :background "red"))))
     (custom-rogue-face ((t (:foreground "pink" :background "black"))))
     (custom-modified-face ((t (:foreground "white" :background "blue"))))
     (custom-set-face ((t (:foreground "blue" :background "white"))))
     (custom-changed-face ((t (:foreground "white" :background "blue"))))
     (custom-saved-face ((t (:underline t))))
     (custom-button-face ((t (nil))))
     (custom-documentation-face ((t (nil))))
     (custom-state-face ((t (:foreground "dark green"))))
     (custom-variable-tag-face ((t (:foreground "blue" :underline t))))
     (custom-variable-button-face ((t (:bold t :underline t))))
     (custom-face-tag-face ((t (:underline t))))
     (custom-group-tag-face-1 ((t (:foreground "red" :underline t))))
     (custom-group-tag-face ((t (:foreground "blue" :underline t))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "SkyBlue3" :bold t))))    ;; JDE
     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue" :italic t))));; JDE
     (cperl-nonoverridable-face ((t (:foreground "CadetBlue"))));; CPerl
     (speedbar-button-face ((t (:foreground "green4"))))
     (speedbar-file-face ((t (:foreground "cyan4"))))
     (speedbar-directory-face ((t (:foreground "blue4"))))
     (speedbar-tag-face ((t (:foreground "brown"))))
     (speedbar-selected-face ((t (:foreground "red" :underline t))))
     (speedbar-highlight-face ((t (:background "green"))))
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "light blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (org-scheduled-previously ((t (:foreground "navy"))))
     (org-todo ((t (:foreground "orange red" :weight bold))))
     (org-warning ((t (:foreground "dark orchid" :weight bold)))))))

(defun color-theme-jekyll ()
  "Customize Sitaramv NT theme like Eclipse default color.
Black foreground on white background.
Includes faces for font-lock, widget, custom, speedbar."
  (interactive)
  (color-theme-install
   '(color-theme-jekyll
     ((foreground-color . "gray80")
      (background-color . "gray6")
      (mouse-color . "sienna3")
      (cursor-color . "yellow")
      (border-color . "Blue")
      (background-mode . dark))
     (mode-line ((t (:background "gray10" :foreground "gainsboro"
                                 :box (:line-width -1 :style released-button) :height 0.85))))
     (mode-line-inactive ((t (:background "gray10" :foreground "RoyalBlue"
                                          :box (:line-width -1 :style released-button) :height 0.85))))
     (modeline-buffer-id ((t (:background "gray10" :foreground "light blue" :weight bold))))
     (modeline-mousable ((t (:background "gray10" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:background "gray10" :foreground "green4"))))
     (which-func  ((t (:background "gray10" :foreground "HotPink"))))
     (hl-line ((t (:background "gray17"))))
     (highlight ((t (:foreground nil :background "LightSteelBlue4"))))
     (region ((t (:foreground nil :background "LightSteelBlue4")))) ;; Selected region
     (secondary-selection ((t (:background "gray25"))))
     (font-lock-comment-face ((t (:foreground "LimeGreen" :italic t))))
     (font-lock-string-face ((t (:foreground "SandyBrown"))))
     (font-lock-keyword-face ((t (:foreground "magenta" :bold t))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-function-name-face ((t (:foreground "light blue"))))
     (font-lock-variable-name-face ((t (:foreground "cyan" :italic t))))
     (font-lock-type-face ((t (:foreground "violet"))))
     (font-lock-constant-face ((t (:foreground "dark turquoise"))))
     (font-lock-warning-face ((t (:foreground "firebrick" :bold t))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "SkyBlue3" :bold t))))    ;; JDE
     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue" :italic t))));; JDE
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (org-scheduled-previously ((t (:foreground "light blue"))))
     (org-todo ((t (:foreground "orange" :weight bold))))
     (outline-3 ((t (:foreground "deep sky blue"))))
     (org-warning ((t (:foreground "orchid" :weight bold)))))))

(color-theme-kimim)

(provide 'color-theme-kimim)

;;; color-theme-kimim.el ends here
