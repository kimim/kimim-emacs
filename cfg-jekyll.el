;; ============================================================================
;; org for blog system
;; ============================================================================
;; file in jekyll base will also be uploaded to github
(setq path-jekyll-base "~/kimi.im/_notes/_posts")
;; in order to sync draft with cloud sync driver
(setq path-jekyll-draft (concat sync-path-home "kimim/_draft/"))

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
         :base-directory "~/kimi.im/_notes/"
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
  (insert (ido-completing-read "tags: " '(
                                          "emacs" "org-mode"
                                          "Deutsch" "Français" "English"
                                          "Windows" "RTOS" "industry"
                                          "travel"  "street-shots" "photography"
                                          "leadership"
                                          )))
  (insert ", ")
  )

(defun jekyll-header()
  "Insert jekyll post headers,
catergories and tags are generated from exisiting posts"
  (interactive)
  (insert "#+BEGIN_HTML\n---\nlayout: post\ntitle: ")
  (insert (read-string "Title: "))
  (insert "\ncategories: [")
  (insert (ido-completing-read "categories: " '(
                                                "technology"
                                                "productivity" "leadership"
                                                "psychology" "language"
                                                "education" "photography"
                                                )))
  (insert "]")
  (insert "\ntags: [")
  (insert (ido-completing-read "tags: " '("emacs" "org-mode" "c prog"
                                          "Deutsch" "Français" "English"
                                          "management")))
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

(provide 'cfg-jekyll)
