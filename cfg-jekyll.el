

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
  (find-file (concat blog-draft-dir "/" (read-string "Filename: ") ".org"))
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
