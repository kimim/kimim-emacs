;;; kimim-theme.el --- Kimim color theme for GNU Emacs.

;; Copyright (C) 2014 Kimi MA <kimi.im@outlook.com>

;; Author:Kimi MA
;; Keywords: faces local color theme of kimim's preference
;; URL: http://github.com/kimim/kimim-night-theme
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

(deftheme kimim-night
    "Color theme by kimim-night.")

(let ((class '((class color) (min-colors 88)))
      (kimim-theme-bg "#262626")
      (kimim-theme-fg "#ffffff")
      (kimim-theme-const "cyan2")
      (kimim-theme-definition "deep sky blue")
      (kimim-theme-comment "medium sea green")
      (kimim-theme-error "orange red")
      (kimim-theme-builtin "#ff55ff")
      (kimim-theme-string "lime green")
      (kimim-theme-blue-3 "blue")
      (kimim-theme-region "gray30")
      (kimim-theme-shadow "grey80")
      (kimim-theme-highlight "gray12")
      (kimim-theme-modeline-bg "gray25"))
  (apply 'custom-theme-set-faces 'kimim-night
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default :foreground ,kimim-theme-fg :background ,kimim-theme-bg)
            (bold :inherit default :weight bold)
            (italic :inherit default :slant italic)
            (underline :inherit default :foreground "orange" :underline t)
            (cursor :background ,"orange")
            (fringe :background ,kimim-theme-bg)
            (mode-line :foreground ,kimim-theme-fg :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (mode-line-inactive :foreground "light steel blue" :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (mode-line-buffer-id :foreground ,kimim-theme-fg :box nil :weight bold)
            (header-line :foreground ,kimim-theme-fg :background ,kimim-theme-modeline-bg :box nil :height 0.85)
            (shadow :foreground ,kimim-theme-shadow)
            (success :foreground ,kimim-theme-error)
            (error :foreground ,kimim-theme-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (compilation-info :underline t :foreground ,kimim-theme-const)
            (highlight :background "gray30")
            (region :background ,kimim-theme-region)
            (secondary-selection :background "deep sky blue" :foreground "#f6f3e8")
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
            (org-level-1 :weight bold :foreground "light blue")
            (org-level-2 :weight bold :foreground "turquoise")
            (org-level-3 :weight bold :foreground "orange")
            (org-level-4 :slant normal :foreground "#E3258D")
            (org-level-5 :slant normal :foreground "#0088ff")
            (org-level-6 :slant italic :foreground "#EA6300")
            (org-level-7 :slant italic :foreground "#2EAE2C")
            (org-level-8 :slant italic :foreground "#FD8008")
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
            (org-block-begin-line :foreground ,kimim-theme-const)
            (org-block-end-line :foreground ,kimim-theme-const)
            (org-block :background "gray20")
            (org-scheduled-previously :foreground "cyan")
            (org-todo :foreground "orange" :weight bold)
            (org-warning :foreground "orange red" :weight bold)
            (org-table :foreground "light blue")
            (org-footnote :foreground "magenta2" :underline t)
            (org-ref-cite-face :inherit link)
            (org-agenda-clocking :inherit secondary-selection
                                 :foreground ,kimim-theme-fg)
            (gnus-summary-cancelled :foreground "lightblue")
            (gnus-header-subject :foreground "blue" :bold t)
            (ido-subdir :weight bold)
            (which-func :foreground ,kimim-theme-builtin)
            (minibuffer-prompt :foreground "light blue")
            (hl-line :background ,kimim-theme-highlight)
            ;; defaults
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "light blue" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,kimim-theme-const)
            (aw-mode-line-face :foreground ,kimim-theme-string)
            (swiper-match-face-1 :background "RoyalBlue4")
            (swiper-match-face-2 :background "#7d3c03")
            (swiper-match-face-3 :background "#1a6600")
            (swiper-match-face-4 :background "#750a20")
            (swiper-background-match-face-1 :background "gray20")
            (swiper-background-match-face-2 :background "#7d3c03")
            (swiper-background-match-face-3 :background "#1a6600")
            (swiper-background-match-face-4 :background "#750a20")
            (swiper-line-face :background "RoyalBlue4")
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
            (mu4e-header-highlight-face :background "gray20")
            (mu4e-replied-face :foreground "deep sky blue")
            (mu4e-forwarded-face :foreground "turquoise")
            (mu4e-unread-face :foreground "light green")
            (sr-active-path-face :background ,kimim-theme-bg :foreground "gold")
            (sr-passive-path-face :background ,kimim-theme-bg :foreground "light blue")
            (hide-ifdef-shadow :inherit shadow :foreground "gray")
            (lsp-ui-doc-url :inherit link :height 0.8)
            (lsp-ui-doc-background :background ,kimim-theme-modeline-bg)
            (term-color-white :foreground "white")
            (term-color-blue :foreground "light blue")))))

;;(custom-theme-set-variables
;; 'kimim-night
;; '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
;;                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kimim-night)

;;; kimim-night-theme.el ends here
