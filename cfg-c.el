;; This is a way to hook tempo into cc-mode
;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(require 'tempo)
(require 'ggtags)
(require 'auto-complete-clang-async)
;;(require 'syntax-subword)
(setq tempo-interactive t)

(defvar c-tempo-tagb nil
  "Tempo tags for C mode")
(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defun coding-mode ()
  ;; start coding mode
  (interactive)
  (ecb-activate)
  (semantic-mode)
  ;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
  ;; but with ecb-compile-window-height = 10, this is no longer needed
  (setq split-height-threshold 0)
  (setq split-width-threshold 60)
  ;; minibuffer completion not work in ecb, use helm instead
  (add-to-list 'ecb-compilation-buffer-names
               '("*helm-mode-execute-extended-command*" . nil)
               '("*helm-mode-bookmark-jump*" . nill))
  (if (eq window-system 'w32)
      (helm-mode)))

(defun working-mode ()
  (interactive)
  (setq split-height-threshold 80)
  (setq split-width-threshold 160)
  (if (eq window-system 'w32)
    (helm-mode -1))
  (ecb-deactivate))

(setq ecb-layout-name "left-kimi0")
(setq ecb-tip-of-the-day nil)
;; use left click as the primary mouse button
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
;; With 'ecb-tree-incremental-search' you can specify if the current
;; search-pattern must be a real prefix of the node (default) or if any
;; substring is matched.
(setq ecb-tree-incremental-search 'substring)
(setq ecb-compile-window-height 15)
(setq ecb-compile-window-width 'edit-window)

;; ggtags settings
;; Activate cygwin mount for gtags CDPATH issue on W32
(cond ((eq window-system 'w32)
		(require 'cygwin-mount)
		(cygwin-mount-activate)))
(setq ggtags-global-ignore-case t)
(setq ggtags-sort-by-nearness t)
(setq ggtags-global-ignore-case nil)
;; let ggtags use split-window with is redefined by ecb mode
;;(setq ggtags-split-window-function 'split-window-below)
(add-hook 'dired-mode '(lambda ()
                         (local-set-key "\C-c\C-f" 'ggtags-find-file)))
(yas-global-mode 1)

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
               (c-put-font-lock-face start (match-beginning 0) 'font-lock-if0-face)
               (setq start nil))
             (when (string= str "endif")
               (setq depth (1- depth)))))
         (when (and start (> depth 0))
           (c-put-font-lock-face start (point) 'font-lock-if0-face)))))
   nil)

(defun my-c-mode-common-hook-if0 ()
   (font-lock-add-keywords
    nil
    '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(defun my-c-mode-ggtags-hook()
  (setq ggtags-bounds-of-tag-function
        (lambda ()
          (bounds-of-thing-at-point 'c-variable))))

(defun kimim/c-mode-ac-complete()
  (global-auto-complete-mode t)
  (setq ac-clang-complete-executable "clang-complete")
  (add-to-list 'ac-sources 'ac-source-clang-async)
  ;; settings inside .dir-locals.el will override this setting!
  ;; then how can I set the default ac-clang-cflags?
  ;; (if ac-clang-cflags
  ;;     (setq ac-clang-cflags (cons ac-clang-cflags '("-I../inc" "-I../include")))
  ;;   (setq ac-clang-cflags '("-I../inc" "-I../include")))
  (ac-clang-launch-completion-process)
  (ac-clang-update-cmdlineargs))

(add-hook 'c-mode-common-hook
          (lambda ()
            (ggtags-mode 1)
            (yas-minor-mode 1)
            (fci-mode 1)
;;            (syntax-subword-mode 1)
            ;;(yas-load-directory "~/.emacs.d/snippets")
            (hs-minor-mode t)
            (c-set-style "S800")
            (c-toggle-auto-newline 0)
            (c-toggle-auto-hungry-state 0)
            (c-toggle-syntactic-indentation 1)
            (which-function-mode 1)
            (local-set-key "\C-\\" 'tempo-complete-tag)
            (local-set-key "\C-co" 'ff-find-other-file)
            (local-set-key "\C-c\C-f" 'ggtags-find-file)
            (my-c-mode-common-hook-if0)
	    (setq c-basic-offset 4)
            (kimim/c-mode-ac-complete)))

(add-hook 'c-mode-hook '(lambda ()
                          (tempo-use-tag-list 'c-tempo-tags)))
(add-hook 'c++-mode-hook '(lambda ()
                            (tempo-use-tag-list 'c-tempo-tags)
                            (tempo-use-tag-list 'c++-tempo-tags)))

;; give clang-complete enough time to parse the code
(setq ac-timer 2)

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang-async ac-source-yasnippet ac-source-gtags) ac-sources)))

(defun kimim/kill-grep-and-ggtags-done()
  (interactive)
;;  (org-agenda-quit)
  (ggtags-navigation-mode-done)
  (if (bufferp (get-buffer "*grep*"))
      (progn
        (switch-to-buffer "*grep*")
        (kill-buffer-and-window)))
  (if (bufferp (get-buffer "*Ibuffer*"))
      (progn
        (switch-to-buffer "*Ibuffer*")
        (kill-buffer-and-window))))

;; close grep window and done ggtags navigation when type C-g
;; but some times it will close all the ecb windows, so remove this advice.
;; (advice-add 'keyboard-quit :before #'kimim/kill-grep-and-ggtags-done)
(defun kimim/recenter()
  (interactive)
  (recenter))

(advice-add 'pop-tag-mark :after #'kimim/recenter)
;;(advice-add 'next-error :after #'kimim/recenter)
;;(advice-add 'previous-error :after #'kimim/recenter)

(provide 'cfg-c)
