(add-hook 'python-mode-common-hook
		  (lambda ()
			(when (derived-mode-p 'python-mode)
			  (ggtags-mode 1))))

;; tell where to load the various files
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-for-python/")
(require 'epy-setup)      ;; It will setup other loads, it is required!
;;(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
;;(require 'epy-editing)    ;; For configurations related to editing [optional]
(require 'epy-bindings)   ;; For my suggested keybindings [optional]
(require 'epy-nose)       ;; For nose integration
