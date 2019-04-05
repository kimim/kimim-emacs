(setq readme-org (concat kimim/path-kimim-emacs "README.org"))
(setq readme-el (concat kimim/path-kimim-emacs "README.el"))

(defun kimim/last-modify-time (file-name)
  "get the last modification time of specified file"
  (+ (* 65536 (nth 0 (nth 5 (file-attributes file-name))))
     (nth 1 (nth 5 (file-attributes file-name)))))

(if (or
     ;; README.el not generated
     (not (file-exists-p readme-el))
     ;; or README.org newer than README.el
     (>
      (kimim/last-modify-time readme-org)
      (kimim/last-modify-time readme-el)))
    ;; load and byte compile the latest configuration
    (progn
      (org-babel-load-file readme-org nil)
      (load-file readme-el))
  (load-file readme-el))
