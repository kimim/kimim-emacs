;; (setq debug-on-error t)
;; System Environment and Path Settings
(setenv "LC_CTYPE" "en_US") ;; remove svn log LC_TYPE not defined warning.
(if (not (boundp 'path-home-sync))
    (setq path-home-sync "~/"))
;; configuration main path
(add-to-list 'load-path "~/kimim-emacs/")
;; local settings
(add-to-list 'load-path "~/kimim-emacs/site-lisp/")

(add-to-list 'Info-additional-directory-list "~/info")

(cond ((eq window-system 'w32)
       (setq cygwin-path "C:/cygwin/")
       (add-to-list 'Info-additional-directory-list
                    (concat cygwin-path "usr/share/info"))
       (add-to-list 'Info-additional-directory-list
                    (concat cygwin-path "usr/local/share/info"))
       (add-to-list 'exec-path (concat cygwin-path "usr/local/bin"))
       (add-to-list 'exec-path (concat cygwin-path "usr/bin"))
       (add-to-list 'exec-path (concat cygwin-path "bin"))
       (add-to-list 'exec-path "C:/emacs/bin/")
       (setenv "PATH"
               (concat
                (concat cygwin-path "usr/local/bin" ";")
                (concat cygwin-path "usr/bin" ";")
                (concat cygwin-path "bin" ";")
                "C:/emacs/bin;"
                (getenv "PATH"))))
      ((eq window-system 'ns)
       (add-to-list 'exec-path "/usr/local/bin")
       (setenv "PATH"
               (concat
                "/usr/local/bin:"
                (getenv "PATH")))))


;;==============================================================================
;; Initialize Packages
;;==============================================================================
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;============================================================================
;; Language and Encoding Settings
;;============================================================================
;;(setenv "LANG" "en_US.UTF-8")
(set-locale-environment "English")
(set-language-environment 'English)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-16le)
(cond ((eq window-system 'w32)
       (set-file-name-coding-system 'gbk)
       (set-terminal-coding-system 'gbk))
      ((eq window-system 'ns)
       (set-file-name-coding-system 'utf-8)
       (set-terminal-coding-system 'utf-8)))

;;==============================================================================
;; Apparance Settings
;;==============================================================================
(require 'color-theme-kimim)
(setq inhibit-startup-message t)                ; 不显示 Emacs 的开始画面
(setq initial-scratch-message nil)              ; scratch buffer 默認為空白
(setq visible-bell t)
(setq ring-bell-function #'ignore)
(tool-bar-mode -1)
(cond ((eq window-system 'ns)
       (menu-bar-mode 1))
      ((eq window-system 'w32)
       (setq x-select-enable-clipboard t)      ; Enable copy and paste in Win32
       (menu-bar-mode -1)))
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode 1)                  ; 显示列号
(blink-cursor-mode -1)                  ; 光标不闪烁
(show-paren-mode 1)                     ; 高亮显示匹配的括号
(global-hl-line-mode 1)                 ; 高亮當前行
(setq inhibit-eol-conversion nil)       ; 不要轉換 end-of-line style
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-interval 10)
(display-time-mode t)                   ; 在 mode-line 上显示时间
(setq font-lock-maximum-decoration t)
(setq font-lock-global-modes '(not shell-mode text-mode))
(setq font-lock-verbose t)
(global-font-lock-mode 1)               ; 开启语法高亮


(cond ((eq window-system 'w32)
       (set-frame-font "Bitstream Vera Sans Mono-11")
       (set-fontset-font "fontset-default" 'han (font-spec :family "Microsoft Yahei" :size 18))
       (setq default-frame-alist
             '((top . 80) (left . 250) (width . 128) (height . 45)
               (font . "Bitstream Vera Sans Mono-11")
               )))
      ((eq window-system 'ns)
       (set-fontset-font  "fontset-default" 'han (font-spec :family "Microsoft Yahei" :size 16))
       ;; OS X 下，不和前一個漢字緊挨著的標點符號的字體不是雅黑，就會顯得很難看，這裡設定一下
       (set-fontset-font  "fontset-default" 'cjk-misc (font-spec :family "Microsoft Yahei" :size 16))
       (setq default-frame-alist
             '((top . 100) (left . 600) (width . 166) (height . 70)
               (font . "Bitstream Vera Sans Mono-14")
               ))))

;;==============================================================================
;; Editor setting
;;==============================================================================
(setq fill-column 80)
;; (require 'ace-isearch)
;; (global-ace-isearch-mode 1)
(require 'ace-window)
(delete-selection-mode 1)		; 輸入的文字覆蓋選中的文字
(setq kill-ring-max 200)                ; kill-ring 最多的记录个数
(setq-default kill-whole-line t)        ; 在行首 C-k 时，同时删除该行。
(setq require-final-newline t)          ; 存盘的时候，要求最后一个字符时换行符
(setq-default tab-width 4)              ; 用space替换tab，tab长度为4
(setq tab-stop-list
      (number-sequence 4 120 4))        ; 每次tab空格數
(setq track-eol t)                      ; 当光标在行尾上下移动的时候保持在行尾
(winner-mode t)
;; 对于每个备份文件，保留最原始的两个版本和最新的五个版本。并且备份的时
;; 候，备份文件是复本，而不是原件。
(setq backup-directory-alist '(("." . "~/Temp")))
(setq version-control t)
(setq kept-old-versions 10)
(setq kept-new-versions 20)
(setq delete-old-versions t)
(setq backup-by-copying t)

(setq auto-save-interval 50)
(setq auto-save-timeout 60)
(setq auto-save-default nil)           ; auto-save of every file-visiting buffer
(setq auto-save-list-file-prefix "~/Temp/auto-saves-")
(setq auto-save-file-name-transforms `((".*"  , "~/Temp/")))
(setq create-lockfiles nil)
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S kimi")
(add-hook 'write-file-hooks 'time-stamp); 自动更新 time-stamp
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default ispell-program-name "aspell")
(add-hook 'text-mode-hook
		  (lambda ()
			(when (derived-mode-p 'org-mode 'markdown-mode 'text-mode)
              (flyspell-mode)
              (visual-line-mode))))
(setq-default indent-tabs-mode nil)

;; 当有两个文件名相同的缓冲时，使用前缀的目录名做 buffer 名字
(setq uniquify-buffer-name-style 'forward)

;; bookmark setting
(setq bookmark-default-file (concat path-home-sync "emacs.bmk"))
;; 当使用 M-x COMMAND 后，显示该 COMMAND 绑定的键 5 秒鐘時間
(setq suggest-key-bindings 5)
;; 每当设置书签的时候都保存书签文件，否则只在你退出 Emacs 时保存
(setq bookmark-save-flag 1)

;;==============================================================================
;; Settings for dired mode
;;==============================================================================
(require 'dired-x)
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (dired-omit-mode 1)
            (setq dired-omit-localp t)
            (setq dired-omit-files
                  "|NTUSER\\|ntuser\\|Cookies\\|AppData\\\
|Contacts\\|Links\\|Intel\\|NetHood\\|PrintHood\\|Recent\\|Start\\|SendTo\\\
|^\\.DS_Store")))
;; Dired buffer 中列出文件时传递给 ls 的参数。加个 "l" 可以使大写的文
;; 件名在顶部，临时的改变可以用 C-u s。
(setq dired-listing-switches "-avhl")
;; 复制(删除)目录的时，第归的复制(删除)其中的子目录。
(setq dired-recursive-copies t)
(setq dired-recursive-deletes t)
(define-key dired-mode-map (kbd "<left>") 'dired-up-directory)
(define-key dired-mode-map (kbd "<right>") 'dired-find-file)

(defadvice dired-next-line (after dired-next-line-advice (arg) activate)
  "Move down lines then position at filename, advice"
  (interactive "p")
  (if (eobp)
      (progn
        (goto-char (point-min))
        (forward-line 2)
        (dired-move-to-filename))))

(defadvice dired-previous-line (before dired-previous-line-advice (arg) activate)
  "Move up lines then position at filename, advice"
  (interactive "p")
  (if (= 3 (line-number-at-pos))
      (goto-char (point-max))))

;;============================================================================
;; Global Mode Settings
;;============================================================================
(setq auto-mode-alist
      (append '(("\\.py\\'" . python-mode)
                ("\\.css\\'" . css-mode)
                ("\\.A\\w*\\'" . asm-mode)
                ("\\.S\\'" . asm-mode)
                ("\\.C\\w*\\'" . c-mode)
                ("\\.md\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode)
                ("\\.svg\\'" . html-mode)
                ("\\.pas\\'" . delphi-mode)
                ("\\.txt\\'" . org-mode)
                )
              auto-mode-alist))

;; intelligent completion setting
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(icomplete-mode 1)
(ido-mode 1)

;;==============================================================================
;; Load other configuration files
;;==============================================================================
(require 'cfg-org)
(require 'cfg-gnus)
(require 'cfg-kimim)
(require 'cfg-jekyll)
(require 'cfg-c)
(require 'cfg-keybinding)
(load-file "~/.emacs.d/work.el")
(load-file "~/.emacs.d/home.el")
