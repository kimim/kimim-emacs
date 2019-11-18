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
      (kimim-night-bg "black")
      (kimim-night-fg "white")
      (kimim-night-const "cyan")
      (kimim-night-comment "green yellow")
      (kimim-night-error "red")
      (kimim-night-builtin "#ff55ff")
      (kimim-night-string "cyan")
      (kimim-night-blue-3 "blue")
      (kimim-night-region "light blue")
      (kimim-night-shadow "grey50"))
  (apply 'custom-theme-set-faces 'kimim-night
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default :foreground ,kimim-night-fg :background ,kimim-night-bg)
            (cursor :background ,"orange")
            (shadow :foreground ,kimim-night-shadow)
            (success :foreground ,kimim-night-error)
            (error :foreground ,kimim-night-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (compilation-info :underline t :foreground ,kimim-night-const)
            (highlight :background "dark green")
            (fringe :background ,kimim-night-bg)
            (region :background ,kimim-night-region :foreground ,kimim-night-bg)
            (secondary-selection :background "deep sky blue" :foreground "#f6f3e8")
            (whitespace-indentation :background "LightYellow" :foreground "lightgray")
            (term)
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,kimim-night-builtin :bold t)
            (font-lock-comment-face :foreground ,kimim-night-comment :slant normal)
            (font-lock-comment-delimiter-face :foreground ,kimim-night-comment :slant normal)
            (font-lock-constant-face :foreground ,kimim-night-const)
            (font-lock-doc-face :foreground ,kimim-night-string)
            (font-lock-doc-string-face :foreground ,kimim-night-string)
            (font-lock-function-name-face :foreground ,kimim-night-fg :bold t)
            (font-lock-keyword-face :foreground ,kimim-night-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,kimim-night-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,kimim-night-builtin)
            (font-lock-regexp-grouping-construct :foreground ,kimim-night-builtin)
            (font-lock-string-face :foreground ,kimim-night-string)
            (font-lock-type-face :foreground ,kimim-night-fg :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,kimim-night-fg)
            (font-lock-warning-face :foreground ,kimim-night-error)
            (font-lock-doxygen-face :foreground "SaddleBrown" :background "#f7f7f7")
            (org-code :foreground ,kimim-night-builtin :weight bold)
            (org-verbatim :foreground ,kimim-night-const)
            (org-level-1 :weight bold :foreground "light blue")
            (org-level-2 :weight bold :foreground "cyan")
            (org-level-3 :weight bold :foreground "yellow")
            (org-level-4 :slant normal :foreground "#E3258D")
            (org-level-5 :slant normal :foreground "#0077CC")
            (org-level-6 :slant italic :foreground "#EA6300")
            (org-level-7 :slant italic :foreground "#2EAE2C")
            (org-level-8 :slant italic :foreground "#FD8008")
            (org-block-begin-line :foreground ,kimim-night-const)
            (org-block-end-line :foreground ,kimim-night-const)
            (org-scheduled-previously :foreground ,kimim-night-comment)
            (org-todo :foreground "orange" :weight bold)
            (org-warning :foreground "orange red" :weight bold)
            (gnus-summary-cancelled :foreground "lightblue")
            (gnus-header-subject :foreground "blue" :bold t)
            (ido-subdir :weight bold)
            (mode-line :foreground "white" :background "dim gray" :box nil :height 0.85)
            (mode-line-inactive :foreground "light blue" :background "dim gray" :box nil :height 0.85)
            (mode-line-buffer-id :foreground "light blue" :background "dim gray" :box nil :weight bold)
            (which-func :foreground ,kimim-night-builtin)
            (minibuffer-prompt :foreground "light blue")
            (hl-line :background "dim gray")
            ;; defaults
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "light blue" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,kimim-night-const)
            (aw-mode-line-face :foreground ,kimim-night-string)
            (swiper-match-face-1 :background "#FEEA89")
            (swiper-match-face-2 :background "#fb7905")
            (swiper-match-face-3 :background "#F9A35A")
            (swiper-match-face-4 :background "#F15C79")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            ;; (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            ;; (magit-section-heading :inherit header-line)
            ;; (magit-section-highlight :weight bold)
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
            (wgrep-face :foreground ,kimim-night-comment)
            (cider-instrumented-face)
            (mu4e-header-highlight-face :background "dim gray")
            (mu4e-replied-face :foreground "light green")
            (mu4e-forwarded-face :foreground "light green")
            (mu4e-unread-face :foreground "light blue")))))

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
