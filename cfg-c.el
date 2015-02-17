;; This is a way to hook tempo into cc-mode
;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(require 'tempo)
(setq tempo-interactive t)

(defvar c-tempo-tags nil
  "Tempo tags for C mode")
(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defun coding-mode ()
  ;; start coding mode
  (interactive)
  (ecb-activate)
  (semantic-mode))

(setq ecb-layout-name "left-kimi0")
;;(setq ecb-layout-name "left-symboldef")
(setq ecb-tip-of-the-day nil)
;; use left click as the primary mouse button
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

;; ggtags settings
(setq ggtags-global-ignore-case t)
;; let ggtags use split-window with is redefined by ecb mode
;;(setq ggtags-split-window-function 'split-window-below)
(add-hook 'dired-mode '(lambda ()
                         (local-set-key "\C-c\C-f" 'ggtags-find-file)))
;(yas-global-mode 1)

;; define new c variable symbol for thing-at-point, used in
;; ggtags-find-tag-dwim

;; TODO: how to my own ggtags-bounds-of-tag-function in c-mode only?
(put 'c-variable 'end-op
     (lambda ()
       (re-search-forward "[A-Za-z0-9_]*" nil t)))

(put 'c-variable 'beginning-op
     (lambda ()
       (if (re-search-backward "[^A-Za-z0-9_]" nil t)
           (forward-char)
         (goto-char (point-min)))))

(defun my-c-mode-ggtags-hook()
  (setq ggtags-bounds-of-tag-function
        (lambda ()
          (bounds-of-thing-at-point 'c-variable))))

(add-hook 'c-mode-common-hook
		  (lambda ()
            (ggtags-mode 1)
            ;;(yas-minor-mode 1)
            ;;(yas-load-directory "~/.emacs.d/snippets")
            (c-set-style "abb-c")
            (hs-minor-mode t)
            ;;(c-toggle-auto-newline -1)
			(c-toggle-auto-hungry-state 1)
            (which-function-mode 1)
            (local-set-key "\C-\\" 'tempo-complete-tag)
            (local-set-key "\C-c\C-f" 'ggtags-find-file)
            (my-c-mode-common-hook-if0)))

(add-hook 'c-mode-hook '(lambda ()
                          (tempo-use-tag-list 'c-tempo-tags)))
(add-hook 'c++-mode-hook '(lambda ()
                            (tempo-use-tag-list 'c-tempo-tags)
                            (tempo-use-tag-list 'c++-tempo-tags)))

(defun my-c-mode-font-lock-if0 (limit)
   (save-restriction
     (widen)
     (save-excursion
       (goto-char (point-min))
       (let ((depth 0) str start start-depth)
         (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
           (setq str (match-string 1))
           (if (string= str "if")
               (progn
                 (setq depth (1+ depth))
                 (when (and (null start) (looking-at "\\s-+0"))
                   (setq start (match-end 0)
                         start-depth depth)))
             (when (and start (= depth start-depth))
               (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
               (setq start nil))
             (when (string= str "endif")
               (setq depth (1- depth)))))
         (when (and start (> depth 0))
           (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
   nil)

(defun my-c-mode-common-hook-if0 ()
   (font-lock-add-keywords
    nil
    '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))
