;; This is a way to hook tempo into cc-mode
;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(require 'tempo)
(require 'ggtags)
(require 'auto-complete-clang-async)

(setq tempo-interactive t)

(defvar c-tempo-tagb nil
  "Tempo tags for C mode")
(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defun coding-mode ()
  ;; start coding mode
  (interactive)
  (ecb-activate)
  (semantic-mode))

;;(setq ecb-layout-name "left-kimi0")
(setq ecb-layout-name "left-symboldef")
(setq ecb-tip-of-the-day nil)
;; use left click as the primary mouse button
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))

;; ggtags settings
;; Activate cygwin mount for gtags CDPATH issue on W32
(cond ((eq window-system 'w32)
		(require 'cygwin-mount)
		(cygwin-mount-activate)))
(setq ggtags-global-ignore-case t)
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
  (if ac-clang-cflags
      (setq ac-clang-cflags (cons ac-clang-cflags '("-I../inc" "-I../include")))
    (setq ac-clang-cflags '("-I../inc" "-I../include")))
  (ac-clang-launch-completion-process)
  (ac-clang-update-cmdlineargs))

(add-hook 'c-mode-common-hook
          (lambda ()
            (ggtags-mode 1)
            (yas-minor-mode 1)
            ;;(yas-load-directory "~/.emacs.d/snippets")
            (hs-minor-mode t)
            (c-set-style "linux")
            (c-toggle-auto-newline -1)
            (c-toggle-auto-hungry-state 1)
            (c-toggle-syntactic-indentation 1)
            (which-function-mode 1)
            (local-set-key "\C-\\" 'tempo-complete-tag)
            (local-set-key "\C-c\C-f" 'ggtags-find-file)
            (my-c-mode-common-hook-if0)
            (kimim/c-mode-ac-complete)))

(add-hook 'c-mode-hook '(lambda ()
                          (tempo-use-tag-list 'c-tempo-tags)))
(add-hook 'c++-mode-hook '(lambda ()
                            (tempo-use-tag-list 'c-tempo-tags)
                            (tempo-use-tag-list 'c++-tempo-tags)))

(defadvice pop-tag-mark (after pop-tag-mark-advice (arg) activate)
  "Recenter when back from tag, advice"
  (interactive "p")
  (recenter))

;; give clang-complete enough time to parse the code
(setq ac-timer 2)

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang-async ac-source-yasnippet ac-source-gtags) ac-sources)))

(provide 'cfg-c)
