(require 'org)

(if (>
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.org")))
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.el"))))
    ;; load and byte compile the latest configuration
    (org-babel-load-file "~/kimim-emacs/README.org" t)
  (load-file  "~/kimim-emacs/README.el"))
