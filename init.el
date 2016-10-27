(shell-command "cd ~/kimim-emacs/ && git pull")

(setq readme-org "~/kimim-emacs/README.org")
(setq readme-el "~/kimim-emacs/README.el")

(if (or
     ;; README.el not generated
     (not (file-exists-p readme-el))
     ;; or README.org newer than README.el
     (>
      (nth 1 (nth 5 (file-attributes readme-org)))
      (nth 1 (nth 5 (file-attributes readme-el)))))
    ;; load and byte compile the latest configuration
    (progn
      (org-babel-load-file readme-org t)
      (load-file readme-el))
  (load-file readme-el))
