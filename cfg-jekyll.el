
;; ============================================================================
;; org for blog system
;; ============================================================================
(setq org-publish-project-alist
      '(
        ("org-blog-content"
         ;; Path to your org files.
         :base-directory "~/kimi.im/_notes"
         :base-extension "org"
         ;; Path to your jekyll project.
         :publishing-directory "~/kimi.im/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :section-numbers t
         :html-extension "html"
         :body-only t ;; Only export section between <body></body>
         :with-toc nil
         )
        ("org-blog-static"
         :base-directory "~//kimi.im/_notes/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|svg"
         :publishing-directory "~/kimi.im/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("blog" :components ("org-blog-content" "org-blog-static"))
        ))

(define-key org-mode-map (kbd "C-c p") (lambda ()
                                         (interactive)
                                         (org-publish-current-file)
                                         (with-temp-buffer(dired "~/kimi.im/")
                                                          (kimim/cyg)
                                                          (kill-buffer))))
(defun jekyll-post ()
  "Post current buffer to kimi.im"
  (interactive)
  ;; get categories
  ;; get buffer file name
  (let ((category (jekyll-get-category))
        (filename (file-name-nondirectory buffer-file-name))
        newfilename)
        ;; append date to the beginning of the file name
    (setq newfilename (concat path-jekyll-base "/" category "/" (format-time-string "%Y-%m-%d-") filename))
    ;; mv the file to the categories folder
    (rename-file buffer-file-name newfilename)
    (switch-to-buffer (find-file-noselect newfilename))
;;    (color-theme-initialize)
;;    (color-theme-jekyll)
    ;; execute org-publish-current-file
    (org-publish-current-file)
;;    (color-theme-eclipse)
    ;; go to kimi.im folder and execute cyg command
    (with-temp-buffer(dired "~/kimi.im/")
                     (kimim/cyg)
                     (kill-buffer))
    ))

(defun jekyll-tag ()
"add new tags"
  (interactive)
  ;find "tags: [" and replace with "tags: [new-tag, "
  (goto-char (point-min))
;;  (search-forward "tags: [")
  (re-search-forward "tags: \\[" nil t)
  (insert (ido-completing-read "tags: " '("travel" "photography" "emacs" "org-mode"
                                          "holiday" "street-shots" "Windows"
                                          "baby care" "Deutsch" "Français"
                                          "c prog" "management" "team"
                                          "cygwin")))
  (insert ", ")
  )

(defun jekyll-header()
  "Insert jekyll post headers,
catergories and tags are generated from exisiting posts"
  (interactive)
  (insert "#+BEGIN_HTML\n---\nlayout: post\ntitle: ")
  (insert (read-string "Title: "))
  (insert "\ncategories: [")
  (insert (ido-completing-read "categories: " '("education" "language" "photography"
                                                "psychology" "fasion"  "management"
                                                "programming" "productivity")))
  (insert "]")
  (insert "\ntags: [")
  (insert (ido-completing-read "tags: " '("travel" "photography" "emacs" "org-mode"
                                          "holiday" "street-shots" "Windows"
                                          "baby care" "Deutsch" "Français"
                                          "c prog" "management" "team"
                                          "cygwin")))
  (insert "]\n---\n#+END_HTML\n\n")
  )


(defun jekyll ()
  (interactive)
  (find-file (concat path-jekyll-draft "/" (read-string "Filename: ") ".org"))
  (jekyll-header)
  (save-buffer)
  )

(defun jekyll-get-category ()
  (interactive)
  (goto-char (point-min))
  (re-search-forward "^categories: \\[\\([a-z-]*\\)\\]$" nil t)
  (match-string 1)
  )

(defun jekyll-test ()
  (interactive)
  (color-theme-initialize)
  (color-theme-jekyll)
  (org-open-file (org-html-export-to-html nil)))


(defun color-theme-jekyll ()
  "Customize Sitaramv NT theme like Eclipse default color.
Black foreground on white background.
Includes faces for font-lock, widget, custom, speedbar."
  (interactive)
  (color-theme-install
   '(color-theme-jekyll
     ((foreground-color . "gray80")
      (background-color . "gray6")
      (mouse-color . "sienna3")
      (cursor-color . "yellow")
      (border-color . "Blue")
      (background-mode . dark))
     (mode-line ((t (:background "gray10" :foreground "gainsboro"
                                 :box (:line-width -1 :style released-button) :height 0.85))))
     (mode-line-inactive ((t (:background "gray10" :foreground "RoyalBlue"
                                          :box (:line-width -1 :style released-button) :height 0.85))))
     (modeline-buffer-id ((t (:background "gray10" :foreground "light blue" :weight bold))))
     (modeline-mousable ((t (:background "gray10" :foreground "firebrick"))))
     (modeline-mousable-minor-mode ((t (:background "gray10" :foreground "green4"))))
     (which-func  ((t (:background "gray10" :foreground "HotPink"))))
     (hl-line ((t (:background "gray17"))))
     (highlight ((t (:foreground nil :background "LightSteelBlue4"))))
     (region ((t (:foreground nil :background "LightSteelBlue4")))) ;; Selected region
     (secondary-selection ((t (:background "gray25"))))
     (font-lock-comment-face ((t (:foreground "LimeGreen" :italic t))))
     (font-lock-string-face ((t (:foreground "SandyBrown"))))
     (font-lock-keyword-face ((t (:foreground "purple" :bold t))))
     (font-lock-builtin-face ((t (:foreground "white"))))
     (font-lock-function-name-face ((t (:foreground "light blue"))))
     (font-lock-variable-name-face ((t (:foreground "cyan" :italic t))))
     (font-lock-type-face ((t (:foreground "violet"))))
     (font-lock-constant-face ((t (:foreground "dark turquoise"))))
     (font-lock-warning-face ((t (:foreground "firebrick" :bold t))))
     (jde-java-font-lock-doc-tag-face ((t (:foreground "SkyBlue3" :bold t))))    ;; JDE
     (jde-java-font-lock-constant-face ((t (:foreground "CadetBlue" :italic t))));; JDE
     (ff-paths-non-existant-file-face ((t (:foreground "NavyBlue" :bold t))))
     (show-paren-match-face ((t (:background "blue"))))
     (show-paren-mismatch-face ((t (:foreground "white" :background "purple"))))
     (org-scheduled-previously ((t (:foreground "light blue"))))
     (org-todo ((t (:foreground "orange" :weight bold))))
     (org-warning ((t (:foreground "orchid" :weight bold)))))))
