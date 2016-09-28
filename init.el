;; (require 'cask "~/.cask/cask.el")
;; (cask-initialize)
;; (require 'pallet)
;; (pallet-mode t)

(require 'package)
(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")
                         ;;("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(setq debug-on-error t)

(require 'org)
(org-babel-load-file
  (expand-file-name "~/kimim-emacs/README.org"
		    user-emacs-directory))
