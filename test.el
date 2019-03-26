(load-file "~/.emacs")

(with-temp-buffer
  (find-file "~/kimim-emacs/README.org")
  (org-latex-export-to-pdf))
