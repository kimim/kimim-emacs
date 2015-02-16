;;==============================================================================
;; Global Key Settings
;;==============================================================================
(global-set-key [f1] 'delete-other-windows)
(global-set-key (kbd "C-<f1>") 'nuke-other-buffers)
(global-set-key [f2] 'other-window)
(global-set-key [f3] 'other-frame)
(global-set-key [f4] 'delete-window)
(global-set-key [f5] (lambda() (interactive)
                       (switch-to-buffer "*scratch*") (delete-other-windows)))
(global-set-key [f6] (lambda() (interactive)
                       (if (not (boundp 'ecb-minor-mode))
                           (ecb-activate)
                         (if ecb-minor-mode
                             (ecb-deactivate)
                           (ecb-activate)))))
(global-set-key [f8] (lambda() (interactive) (list-charset-chars 'ascii)))
(global-set-key [f9] 'kimim/cyg)
(global-set-key  (kbd "S-<f9>") 'kimim/cmd)
(global-set-key (kbd "C-<f11>") (lambda()
                                  (interactive)
                                  (toggle-frame-fullscreen)))
(global-set-key [f12] 'org-toggle-home-or-office)

(global-set-key "\C-xg" 'grep-find)
(global-set-key "\C-x\C-b" 'ibuffer-other-window)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'backward-kill-word)
(global-set-key (kbd "C-?") 'help)
(global-set-key "\M-?" 'mark-paragraph)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key "\C-xj" 'bookmark-jump)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-v" 'view-file-other-window)
(global-set-key "\C-c\C-o" 'occur)
;;(global-set-key "\C-c;" 'flyspell-correct-word-before-point) not available in org mode
(global-set-key "\C-x/" 'pop-global-mark)
(global-set-key "\C-\\" 'tempo-complete-tag)
(global-set-key "\C-z" 'set-mark-command)
(global-set-key "\M-/" 'hippie-expand)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" 'previous-error)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-c=" 'get-file-line)
(define-key hs-minor-mode-map "\C-c/" 'hs-toggle-hiding)
(define-key global-map "\M-." 'ggtags-find-tag-dwim)
(global-set-key "\C-c\C-x\C-x" 'org-clock-in-last)
(global-set-key "\C-c\C-x\C-i" 'org-clock-in)
(global-set-key "\C-c\C-x\C-o" 'org-clock-out)
(global-set-key [?\C-c ?\C-/] 'comment-or-uncomment-region)

(define-key global-map (kbd "RET") 'newline-and-indent)


(define-prefix-command 'ctl-x-m-map)
;; 定义了一个新的前缀，并且绑定到 C-x m
(global-set-key "\C-xm" 'ctl-x-m-map)
;; 查看光标处的单词的 man page
(define-key ctl-x-m-map "m" 'man-follow)
;;查看 kill-ring，都曾经 kill 过哪些文本
(define-key ctl-x-m-map "l" 'browse-kill-ring)

;; C-x r j ?x 打開常用文件
(set-register ?e '(file . "~/kimim-emacs/init.el"))
(set-register ?m '(file . "~/kimim-emacs/cfg-kimim.el"))
(set-register ?o '(file . "~/kimim-emacs/cfg-org.el"))
(set-register ?k '(file . "~/kimim-emacs/cfg-keybinding.el"))
(set-register ?c '(file . "~/kimim-emacs/cfg-c.el"))
