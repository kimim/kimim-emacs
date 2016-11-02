;; set shell-file-name for WIN, otherwise, cmdproxy is chosen
(if (eq window-system 'w32)
    (setq shell-file-name "bash.exe"))

;; pull updates from http://github.com/kimim/kimim-emacs
(shell-command
 (concat "cd " kimim/path-kimim-emacs " && git pull"))

(setq readme-org (concat kimim/path-kimim-emacs "README.org")
(setq readme-el (concat kimim/path-kimim-emacs "README.el"))

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
