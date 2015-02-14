;; Time-stamp: <2015-02-14 Sat 20:57:46 kimi>
;; self define functions
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

(defun kimim/cmd ()
  "Open cmd.exe from emacs just as you type: Win-R, cmd, return."
  (interactive)
  (w32-shell-execute
   "open" "cmd"))

(defun kimim/cyg ()
  "Open cygwin mintty from emacs."
  (interactive)
  (w32-shell-execute
   "open" "mintty" " -e bash"))

(defun kimim/4nt ()
  "Open 4NT terminal"
  (interactive)
  (w32-shell-execute
   "open" "4nt"))



;;(yas-global-mode 1)
;; Completing point by some yasnippet key
;; (defun yas-ido-expand ()
;;   "Lets you select (and expand) a yasnippet key"
;;   (interactive)
;;     (let ((original-point (point)))
;;       (while (and
;;               (not (= (point) (point-min) ))
;;               (not
;;                (string-match "[[:space:]\n]" (char-to-string (char-before)))))
;;         (backward-word 1))
;;     (let* ((init-word (point))
;;            (word (buffer-substring init-word original-point))
;;            (list (yas-active-keys)))
;;       (goto-char original-point)
;;       (let ((key (remove-if-not
;;                   (lambda (s) (string-match (concat "^" word) s)) list)))
;;         (if (= (length key) 1)
;;             (setq key (pop key))
;;           (setq key (ido-completing-read "key: " list nil nil word)))
;;         (delete-char (- init-word original-point))
;;         (insert key)
;;         (yas-expand)))))

;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-ido-expand)

(setq everything-cmd "~/../Tools/es.exe")



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


;;============================================================================
;; Default Grep settings
;;============================================================================
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device."
  (let ((null-device "/dev/null"))
    ad-do-it))
(ad-activate 'grep-compute-defaults)
(setq grep-find-command "cygfind . -type f -not -name \"*.svn-base\" -and -not -name \"*#\" -and -not -name \"*.tmp\" -and -not -name \"*.obj\" -and -not -name \"*.386\" -and -not -name \"*.img\" -and -not -name \"*.LNK\" -print0 | xargs -0 -e grep -n -e ")

(defun encode-buffer-to-utf8 ()
  "Sets the buffer-file-coding-system to UTF8."
  (interactive)
  (set-buffer-file-coding-system 'utf-8 nil))

(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

;; (defun nuke-all-buffers ()
;;   "Kill all buffers, leaving *scratch* only."
;;   (interactive)
;;   (mapcar (lambda (x) (if (not (member (buffer-name x)
;;                                        '(" *ECB Sources*" " *ECB History*" " *ECB Methods*" " *Minibuf-1*" " *Minibuf-0*" " *ECB Analyse*" " *ECB Directories*")))
;;                           (kill-buffer x)
;;                           ))
;;           (buffer-list))
;;   (delete-other-windows))

(defun nuke-other-buffers ()
  "Kill all buffers, leaving current-buffer only."
  (interactive)
  (mapcar (lambda (x)
            (if (not (or (eq x (current-buffer))
                         (member (buffer-name x)
                                 ;; all ecb related buffers
                                 '(" *ECB Sources*" " *ECB History*" " *ECB Methods*"
                                   " *Minibuf-1*" " *Minibuf-0*" " *ECB Analyse*"
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
