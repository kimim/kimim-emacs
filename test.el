(load-file "~/.emacs")

(with-temp-buffer
  (find-file "~/kimim-emacs/README.org")
  (org-html-export-to-html))
