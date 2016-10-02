(require 'org)

;; load the latest configuration
(if (>
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.org")))
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.el"))))
    (progn
      (org-babel-load-file "~/kimim-emacs/README.org")
      (byte-compile-file "~/kimim-emacs/README.el"))
  (load-file  "~/kimim-emacs/README.el"))
