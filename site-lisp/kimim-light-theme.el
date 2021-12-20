;;; kimim-theme.el --- Kimim color theme for GNU Emacs.

;; Copyright (C) 2014 Kimi MA <kimi.im@outlook.com>

;; Author:Kimi MA
;; Keywords: faces local color theme of kimim's preference
;; URL: http://github.com/kimim/kimim-theme
;; Version: 0.0.1
;; Keywords: theme

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

;;; Commentary:
;;
;; This theme assumes light background.  To load it, use:
;;
;;     (require 'kimim-theme)

;;; Code:

(deftheme kimim-light
    "Color theme by kimim.")

(let ((class '((class color) (min-colors 88)))
      (kimim-theme-bg "#f8f8f8")
      (kimim-theme-fg "#000000")
      (kimim-theme-definition "blue")
      (kimim-theme-const "#110099")
      (kimim-theme-comment "#3F7F5F")
      (kimim-theme-error "#FF0000")
      (kimim-theme-builtin "#7F0055")
      (kimim-theme-string "#2A00FF")
      (kimim-theme-blue-3 "#758BC6")
      (kimim-theme-region "LightBlue")
      (kimim-theme-shadow "grey20")
      (kimim-theme-highlight "azure")
      (kimim-theme-modeline-bg "gainsboro"))
  (apply 'custom-theme-set-faces 'kimim-light
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default :foreground ,kimim-theme-fg :background ,kimim-theme-bg)
            (bold :inherit default :weight bold)
            (italic :inherit default :slant italic)
            (underline :inherit default :foreground "magenta4" :underline t)
            ;; make the code font in markdown look better
            (fixed-pitch :family Consolas)
            (fixed-pitch-serif :family Consolas)
            (cursor :background ,"orange")
            (fringe :background ,kimim-theme-bg)
            (mode-line :foreground "black" :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (mode-line-inactive :foreground "royal blue" :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (mode-line-buffer-id :foreground "blue4" :box nil :weight bold)
            (header-line :foreground "black" :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (shadow :foreground ,kimim-theme-shadow)
            (success :foreground ,kimim-theme-error)
            (error :foreground ,kimim-theme-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (compilation-info :underline t :foreground ,kimim-theme-const)
            (highlight :background "darkseagreen2")
            (region :background ,kimim-theme-region :foreground ,kimim-theme-bg)
            (secondary-selection :background "paleturquoise" :foreground "orange")
            (whitespace-indentation :background "LightYellow" :foreground "lightgray")
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,kimim-theme-builtin :bold t)
            (font-lock-comment-face :foreground ,kimim-theme-comment :slant normal)
            (font-lock-comment-delimiter-face :foreground ,kimim-theme-comment :slant normal)
            (font-lock-constant-face :foreground ,kimim-theme-const)
            (font-lock-doc-face :foreground ,kimim-theme-string)
            (font-lock-doc-string-face :foreground ,kimim-theme-string)
            (font-lock-function-name-face :foreground ,kimim-theme-definition :bold t)
            (font-lock-keyword-face :foreground ,kimim-theme-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,kimim-theme-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,kimim-theme-builtin)
            (font-lock-regexp-grouping-construct :foreground ,kimim-theme-builtin)
            (font-lock-string-face :foreground ,kimim-theme-string)
            (font-lock-type-face :foreground ,kimim-theme-fg :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,kimim-theme-fg)
            (font-lock-warning-face :foreground ,kimim-theme-error)
            (font-lock-doxygen-face :foreground "SaddleBrown" :background "#f7f7f7")
            (org-level-1 :weight bold :foreground "#00008b") ;; dark blue
            (org-level-2 :weight bold :foreground "#6a5acd") ;; slate blue
            (org-level-3 :weight bold :foreground "#0078d7")
            (org-level-4 :weight bold :foreground "#c71585") ;; medium violet red
            (org-level-5 :slant normal :foreground "#00008b")
            (org-level-6 :slant normal :foreground "#6a5acd")
            (org-level-7 :slant normal :foreground "#a0522d")
            (org-level-8 :slant normal :foreground "#c71585")
            (markdown-header-face-1 :inherit org-level-1)
            (markdown-header-face-2 :inherit org-level-2)
            (markdown-header-face-3 :inherit org-level-3)
            (markdown-header-face-4 :inherit org-level-4)
            (markdown-header-face-5 :inherit org-level-5)
            (markdown-header-face-6 :inherit org-level-6)
            (markdown-header-face-7 :inherit org-level-7)
            (markdown-header-face-8 :inherit org-level-8)
            (org-code :foreground ,kimim-theme-builtin :weight bold)
            (org-verbatim :foreground ,kimim-theme-const)
            (org-hide :foreground ,kimim-theme-bg)
            (org-tag :slant italic :weight normal :foreground ,kimim-theme-shadow)
            (org-beamer-tag :inherit org-tag)
            (org-block-begin-line :inherit fixed-pitch :foreground ,kimim-theme-const)
            (org-block-end-line :inherit fixed-pitch :foreground ,kimim-theme-const)
            (org-block :background "gray94")
            (org-scheduled-previously :foreground ,kimim-theme-comment)
            (org-todo :foreground "orange red" :weight bold)
            (org-checkbox :inherit org-todo)
            (org-warning :foreground "dark orchid" :weight bold)
            (org-table :foreground "blue1")
            (org-footnote :foreground "purple" :underline t)
            (org-ref-cite-face :inherit link)
            (org-agenda-clocking :inherit secondary-selection
                                 :foreground ,kimim-theme-fg)
            (gnus-summary-cancelled :foreground "lightblue")
            (gnus-header-subject :foreground "blue" :bold t)
            (ido-subdir :weight bold)
            (which-func :foreground ,kimim-theme-builtin)
            (minibuffer-prompt :foreground "medium blue")
            (hl-line :background ,kimim-theme-highlight)
            ;; defaults
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "RoyalBlue3" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,kimim-theme-const)
            (aw-mode-line-face :foreground ,kimim-theme-string)
            (swiper-match-face-1 :background "white smoke")
            (swiper-match-face-2 :background "#FFCCCC") ;;selected match 1
            (swiper-match-face-3 :background "#CCFFFF") ;;selected match 2
            (swiper-match-face-4 :background "#FFFFCC") ;;selected match 3
            (swiper-background-match-face-1 :background "white smoke")
            (swiper-background-match-face-2 :background "#FFECEC") ;; unselected match 1
            (swiper-background-match-face-3 :background "#ECFFFF") ;; unselected match 2
            (swiper-background-match-face-4 :background "#FFFFEC") ;; unselected match 3
            (swiper-line-face :background "azure2")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            ;; (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            ;; (magit-section-heading :inherit header-line)
            (magit-section-highlight :weight bold :background ,kimim-theme-highlight)
            ;; (magit-diff-context :foreground "grey20")
            ;; (magit-diff-context-highlight :weight bold :foreground "grey20")
            ;; (magit-diff-added :inherit diff-added)
            ;; (magit-diff-added-highlight :inherit diff-added :weight bold)
            ;; (magit-diff-removed :inherit diff-removed)
            ;; (magit-diff-removed-highlight :inherit diff-removed :weight bold)
            ;; (magit-diff-file-heading)
            ;; (magit-diff-file-heading-highlight :weight bold)
            ;; (magit-diff-file-heading-selection :foreground "red")
            ;; (magit-diff-hunk-heading :inherit diff-hunk-header)
            ;; (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
            ;; (magit-hash :foreground "firebrick")
            ;; (magit-branch-remote :background "Grey85" :foreground "OliveDrab4" :box t)
            ;; (magit-branch-local :background "Grey85" :foreground "LightSkyBlue4" :box t)
            (ivy-highlight-face)
            (ivy-posframe :background "#eeeeee" :foreground "#000000")
            (wgrep-face :foreground ,kimim-theme-comment)
            (cider-instrumented-face)
            (mu4e-header-highlight-face :background "azure")
            (mu4e-replied-face :foreground "dark green")
            (mu4e-forwarded-face :foreground "dark orange")
            (mu4e-unread-face :foreground "blue" :bold t)
            (message-cited-text-1 :foreground "blue")
            (sr-active-path-face :background ,kimim-theme-bg :foreground "deep pink")
            (sr-passive-path-face :background ,kimim-theme-bg :foreground "blue")
            (hide-ifdef-shadow :inherit shadow :foreground "olive drab")
            (ggtags-global-line :inherit secondary-selection :foreground "black")
            (lsp-ui-doc-url :inherit link :height 0.8)
            (lsp-ui-doc-background :background ,kimim-theme-modeline-bg)
            (term-color-white :foreground "blue")
            (term-color-blue :foreground "blue")))))

(custom-theme-set-variables
 'kimim-light
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kimim-light)

;;; kimim-light-theme.el ends here
