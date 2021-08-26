(defgroup kimim nil
  "kimim group."
  :group 'local)

(defun kimim/mail-new ()
  (interactive)
  (mu4e-compose-new)
  (kimim/mail-setup))

(defun kimim/mail-new-empty ()
  (interactive)
  (save-restriction
    (mu4e-compose-new)
    (kill-region (point-min) (point-max))))

(defun kimim/mail-setup ()
  (interactive)
  (save-restriction
    ;; always bcc to myself
    ;;(mail-bcc)
    ;;(insert user-mail-address)
    ;; construct default mail text stuff
    (mail-text)
    (insert "\n\n")
    (insert user-full-name)
    ;;(insert "\n")
    ;;(insert "--\n")
    (open-line 1)
    ;;(insert-file mail-signature-file)
    ;; if subject is empty, fill subject
    (mail-subject)
    (goto-char (line-beginning-position))
    (if (search-forward-regexp "^Subject: $" nil t)
        (mail-subject)
      (progn
        (narrow-to-region (line-beginning-position) (line-end-position))
        (goto-char (point-min))
        (while (search-forward "回复：" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (search-forward "回复: " nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (search-forward "答复：" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (search-forward "答复: " nil t)
          (replace-match ""))
        (widen)
        ;; if To: is empty, fill mail-to
        (mail-to)
        (goto-char (line-beginning-position))
        (if (search-forward-regexp "^To: $" nil t)
            (mail-to)
          (mail-text))))))

;; unfill paragraph: the opposite of fill-paragraph
(defun kimim/unfill-paragraph-or-region (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; generate timestamp such as 2016_1031_ for file name
(defun kimim/genfile-timestamp()
  (concat (format-time-string "%Y%m%d")
          (char-to-string (+ 65 (random 26)))
          (char-to-string (+ 65 (random 26)))
          "_"))

;; self define functions
(defun kimim/imenu-default-goto-function-advice (orig-fun &rest args)
  (apply orig-fun args)
  (recenter))

(defun now () (interactive)
       (insert (shell-command-to-string "date")))

(defun day ()
  "Insert string for today's date nicely formatted in American style,
    e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

(defun today ()
  "Insert string for today's date nicely formatted in American style,
    e.g. 2000-10-12."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

(defun today-pretty ()
  "Insert string for today's date nicely formatted in American style,
    e.g. 2000-10-12."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %b %d, %Y")))

(defun toyear ()
  "Insert string for today's date nicely formatted in American style,
    e.g. 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y")))


(defun get-file-line ()
  "Show (and set kill-ring) current file and line"
  (interactive)
  (unless (buffer-file-name)
    (error "No file for buffer %s" (buffer-name)))
  (let ((msg (format "%s::%d"
                     (file-truename (buffer-file-name))
                     (line-number-at-pos))))
    (kill-new msg)
    (message msg)))


(defun open-folder-in-explorer ()
  "Call when editing a file in a buffer.
    Open windows explorer in the current directory and select the current file"
  (interactive)
  (w32-shell-execute
   "open" "explorer"
   (concat "/e,/select," (convert-standard-filename buffer-file-name))
   ))


(defun mac-open-terminal ()
  (interactive)
  (let ((dir ""))
    (cond
     ((and (local-variable-p 'dired-directory) dired-directory)
      (setq dir dired-directory))
     ((stringp (buffer-file-name))
      (setq dir (file-name-directory (buffer-file-name))))
     ((stringp default-directory)
      (setq dir default-directory))
     )
    (do-applescript
     (format "
     tell application \"Terminal\"
       activate
       try
         do script with command \"cd %s\"
       on error
         beep
       end try
     end tell" dir))
    ))

(defun kimim/cmd ()
  "Open cmd.exe from emacs just as you type: Win-R, cmd, return."
  (interactive)
  (w32-shell-execute
   "open" "cmd"))

(defun kimim/xterm ()
  "Open shell window from emacs."
  (interactive)
  (cond ((eq system-type 'cygwin)
         (start-process "" nil "xdg-open" "mintty"))
        ((eq system-type 'windows-nt)
         (w32-shell-execute "open" "mintty" " -e bash"))
        ((eq system-type 'darwin)
         (mac-open-terminal))))

(defun kimim/4nt ()
  "Open 4NT terminal"
  (interactive)
  (w32-shell-execute
   "open" "4nt"))

(defun kimim/dc ()
  "Open file location in double commander"
  (interactive)
  (let ((current-fold (replace-regexp-in-string "/" "\\\\" default-directory)))
    (w32-shell-execute
     "open" "doublecmd" (concat "-L \"" current-fold "\" -R \"" current-fold "\""))))

(defun kill-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

(setq scroll-margin                   0 )
(setq scroll-conservatively      100000 )
                                        ;(setq scroll-preserve-screen-position 1 )
(setq scroll-up-aggressively       0.01 )
(setq scroll-down-aggressively     0.01 )



(defun encode-buffer-to-utf8 ()
  "Sets the buffer-file-coding-system to UTF8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8 nil))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(defun nuke-other-buffers ()
  "Kill all buffers, leaving current-buffer only."
  (interactive)
  (mapcar
   (lambda (x)
     (if (not
          (or (eq x (current-buffer))
              (member
               (buffer-name x)
               ;; all ecb related buffers
               '(" *ECB Sources*" " *ECB History*"
                 " *ECB Methods*" " *Minibuf-1*"
                 " *Minibuf-0*" " *ECB Analyse*"
                 " *ECB Directories*"))))
         (kill-buffer x)))
   (buffer-list))
  (delete-other-windows)
  (message "All other buffers clear"))

(defun indent-whole-buffer ()
  "Indent whole buffer and delete trailing whitespace.
    This command will also do untabify."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

(defun fold-long-comment-lines ()
  "This functions allows us to fold long comment lines
     automatically in programming modes. Quite handy."
  (interactive "p")
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-no-break-predicate)
       (lambda ()
         (not (eq (get-text-property (point) 'face)
                  'font-lock-comment-face)))))

(defun new-note ()
  (interactive)
  (find-file (concat default-doc-path "/Notes/"
                     (format-time-string "%Y%m-")
                     (read-string (concat "Filename: " (format-time-string "%Y%m-"))) ".org")))

(add-hook 'comint-output-filter-functions
          'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)

;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(cond ((eq window-system 'w32)
       (setq explicit-shell-file-name "bash.exe")
       (setq shell-file-name explicit-shell-file-name)))

(setq color-list '(hi-yellow hi-green hi-blue hi-pink));; hi-red-b hi-green-b hi-blue-b))
(setq color-index 0)
(setq color-list-length (length color-list))

(defun kimim/toggle-highlight-tap ()
  "Highlight pattern at the point"
  (interactive)
  (if (and (listp (get-text-property (point) 'face))
           (memq (car (get-text-property (point) 'face)) color-list))
      (unhighlight-regexp (thing-at-point 'symbol))
    (progn
      (highlight-regexp (thing-at-point 'symbol) (nth color-index color-list))
      (setq color-index (+ color-index 1))
      (if (>= color-index color-list-length)
          (setq color-index 0))
      )))


(defun my-blink(begin end)
  "blink a region. used for copy and delete"
  (interactive)
  (let* ((rh (make-overlay begin end)))
    (progn
      (overlay-put rh 'face '(:background "DodgerBlue" :foreground "White"))
      (sit-for 0.2 t)
      (delete-overlay rh)
      )))

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring. Remove leading and
    trailing whitespace while we're at it. Also, remove whitespace before
    column, if any. Also, font-lock will be removed, if any. Also, the
    copied region will be highlighted shortly (it 'blinks')."
  (save-excursion
    (let* ((beg (get-point begin-of-thing 1))
           (end (get-point end-of-thing arg)))
      (progn
        (copy-region-as-kill beg end)
        (with-temp-buffer
          (yank)
          (goto-char 1)
          (while (looking-at "[ \t\n\r]")
            (delete-char 1))
          (delete-trailing-whitespace)
          (delete-whitespace-rectangle (point-min) (point-max)) ;; del column \s, hehe
          (font-lock-unfontify-buffer) ;; reset font lock
          (kill-region (point-min) (point-max))
          )
        ))))

(defun copy-word (&optional arg)
  "Copy word at point into kill-ring"
  (interactive "P")
  (my-blink (get-point 'backward-word 1) (get-point 'forward-word 1))
  (copy-thing 'backward-word 'forward-word arg)
  (message "word at point copied"))

(defun copy-line (&optional arg)
  "Copy line at point into kill-ring, truncated"
  (interactive "P")
  (my-blink (get-point 'beginning-of-line 1) (get-point 'end-of-line 1))
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (message "line at point copied"))

(defun copy-paragraph (&optional arg)
  "Copy paragraph at point into kill-ring, truncated"
  (interactive "P")
  (my-blink (get-point 'backward-paragraph 1) (get-point 'forward-paragraph 1))
  (copy-thing 'backward-paragraph 'forward-paragraph arg)
  (message "paragraph at point copied"))

(defun copy-buffer(&optional arg)
  "Copy the whole buffer into kill-ring, as-is"
  (interactive "P")
  (progn
    (my-blink (point-min) (point-max))
    (copy-region-as-kill (point-min) (point-max))
    (message "buffer copied")))


(defvar kimim/last-edit-list nil)
;; ((file location) (file location))
;;   1              2

(defun kimim/backward-last-edit ()
  (interactive)
  (let ((position (car kimim/last-edit-list)))
    (when position
      ;;(print position)
      ;;(print kimim/last-edit-list)
      (find-file (car position))
      (goto-char (cdr position))
      (setq kimim/last-edit-list (cdr kimim/last-edit-list)))))


;; TODO shrink list if more items
(defun kimim/buffer-edit-hook (beg end len)
  (interactive)
  (let ((bfn (buffer-file-name)))
    ;; insert modification in current index
    ;; remove forward locations
    ;; if longer than 100, remove old locations
    (when bfn
      (progn
        (add-to-list 'kimim/last-edit-list (cons bfn end))))))

(add-hook 'after-change-functions 'kimim/buffer-edit-hook)
(global-set-key (kbd "M-`") 'kimim/backward-last-edit)

;; copy from http://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
;; many thanks to Bozhidar Batsov (https://github.com/bbatsov)
(defun kimim/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting. Binded to
  key C-c r"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)
(setq grep-find-command
      "find . -type f -not -name \"*.svn-base\" -and -not -name \"*#\" -and -not -name \"*.tmp\" -and -not -name \"*.obj\" -and -not -name \"*.386\" -and -not -name \"*.img\" -and -not -name \"*.LNK\" -and -not -name GTAGS -print0 | xargs -0 grep -n -e ")

(defun kimim/grep-find()
  (interactive)
  (grep-find (concat grep-find-command (buffer-substring-no-properties (region-beginning) (region-end)))))



(defun kimim/delete-trailing-whitespace (&optional start end)
  (interactive)
  (if (or (not (boundp 'deft-auto-save-buffers))
          (not (member (current-buffer) deft-auto-save-buffers)))
      (delete-trailing-whitespace)))


(defun kimim/open-external (&optional file)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
copy from xah lee: http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html"
  (interactive)
  (let (doit
        (flist
         (cond
          ((or (string-equal major-mode "dired-mode")
               (string-equal major-mode "sr-mode"))
           (dired-get-marked-files))
          ((not file) (list (buffer-file-name)))
          (file (list file)))))

    (setq doit (if (<= (length flist) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ")))

    (when doit
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (path) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)) ) flist))
       ((string-equal system-type "darwin")
        (mapc (lambda (path) (shell-command (format "open \"%s\"" path)))  flist))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path)) ) flist))
       ((string-equal system-type "cygwin")
        (mapc (lambda (path) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" path)) ) flist))))))

(defun kimim/update-kimim-emacs()
  (interactive)
  ;; set shell-file-name for WIN, otherwise, cmdproxy is chosen
  (if (eq window-system 'w32)
      (setq shell-file-name "bash.exe"))
  ;; pull updates from http://github.com/kimim/kimim-emacs
  (shell-command
   (concat "cd " kimim/path-kimim-emacs " && git pull")))


(defun kimim/mail-attach-files()
  "Convert clipboard files list to attach string format"
  (interactive)
  (let ((file-list (x-get-clipboard)))
    (save-excursion
      (end-of-buffer)
      (mapc #'(lambda (item)
               (insert (concat "<#part filename=\""
                               ;; remove "\\" in Windows environment
                               (replace-regexp-in-string
                                "C:" "/cygdrive/c"
                                (replace-regexp-in-string (regexp-quote "\\") "/" item) t)
                               "\" disposition=attachment><#/part>\n")))
            (split-string file-list "\n" t)))))


(defun kimim/toggle-path-header ()
  "Toggle display path header"
  (interactive)
  (if header-line-format
      (path-header-line-off)
    (path-header-line-on)))


(defvar sumantrapdf-config
  (concat kimim/path-kimikit "sumatrapdf/SumatraPDF-settings.txt"))


(defun kimim/sumantrapdf-theme (window-color)
  "Change color theme in sumatrapdf, just modify the config file"
  (interactive)
  (find-file sumantrapdf-config)
  (goto-char (point-min))
  (replace-regexp "\\(MainWindowBackground = \\).*" (concat "\\1" window-color))
  (goto-char (point-min))
  (replace-regexp "\\(TextColor = \\).*"
                  (concat "\\1" (face-attribute 'default :foreground)))
  (goto-char (point-min))
  (replace-regexp "\\(BackgroundColor = \\).*"
                  (concat "\\1" (face-attribute 'default :background)))
  (save-buffer)
  (kill-this-buffer))

(defun kimim/theme-night ()
  "Change emacs theme and sumatrapdf theme to night color"
  (interactive)
  (load-theme 'kimim-night)
  (kimim/sumantrapdf-theme "#333333"))


(defun kimim/theme-light ()
  "Change emacs theme and sumatrapdf theme to light color"
  (interactive)
  (load-theme 'kimim-light)
  (kimim/sumantrapdf-theme "#aaaaaa"))


(defun kimim/theme-paper ()
  "Change emacs theme and sumatrapdf theme to paper color"
  (interactive)
  (load-theme 'kimim-paper)
  (kimim/sumantrapdf-theme "#e4dcc8"))


(defun kimim/org-roam-migrate-buffer ()
  (interactive)
  (org-roam-migrate-v1-to-v2))


(defun kimim/shake-mouse ()
  "shake mouse to remove yodao translation window"
  (interactive)
  (let ((x (* (frame-char-width) (or (cadr (mouse-position))
                                     (- (frame-width) 36))))
        (y (* (frame-char-height) (or (cddr (mouse-position))
                                      0))))
    (set-mouse-pixel-position (selected-frame) (- x 100) y)
    (run-with-timer 0.5 nil
                    (lambda (x y)
                      (set-mouse-pixel-position (selected-frame) x y))
                    x y)))

;; (advice-add 'recenter-top-bottom
;;             :after #'kimim/shake-mouse)

(defun kimim/top-right-mouse ()
  "Move mouse to top right corner"
  (interactive)
  (let ((x (* (frame-char-width) (- (frame-width) 33)))
        (y 7))
    (set-mouse-pixel-position (selected-frame) x y)))

(defun kimim/shrink-down ()
  "Shrink window to the middle down part, leave space for reading
documents."
  (interactive)
  (kimim/frame-and-font)
  (set-frame-height (selected-frame) 18)
  (set-frame-position
   (selected-frame)
   (/ (nth 3 (caar (display-monitor-attributes-list))) 10)
   (* 6 (/ (nth 4 (caar (display-monitor-attributes-list))) 10))))

(defun kimim/shrink-up ()
  "Shrink window to the middle up part, leave space for reading
documents."
  (interactive)
  (kimim/frame-and-font)
  (set-frame-height (selected-frame) 18)
  (set-frame-position
   (selected-frame)
   (/ (nth 3 (caar (display-monitor-attributes-list))) 10)
   (* 1 (/ (nth 4 (caar (display-monitor-attributes-list))) 20))))

(defun kimim/shrink-right ()
  "Shrink window to the right part, leave space for reading
documents."
  (interactive)
  (kimim/frame-and-font)
  (set-frame-width (selected-frame) (round (/ (frame-width) 2.1)))
  (set-frame-position
   (selected-frame)
   (/ (* (nth 3 (caar (display-monitor-attributes-list))) 3) 5)
   (cdr (frame-position))))

(defun kimim/shrink-left ()
  "Shrink window to the left part, leave space for reading
documents."
  (interactive)
  (kimim/frame-and-font)
  (set-frame-width (selected-frame) (round (/ (frame-width) 2.1)))
  (set-frame-position
   (selected-frame)
   (/ (nth 3 (caar (display-monitor-attributes-list))) 100)
   (cdr (frame-position))))


(defun kimim/toggle-proxy ()
  "Turn proxy on/off"
  (interactive)
  (if url-proxy-services
      (progn
        (setq url-proxy-services nil)
        (message "proxy is off"))
    (progn
      (setq url-proxy-services '(("no_proxy" . "localhost")
                                 ("http" . "127.0.0.1:1080")
                                 ("https" . "127.0.0.1:1080")))
      (message "proxy is on"))))

(provide 'kimim)
