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


(if (>
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.org")))
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.el"))))
    (progn
      (print "org is new")
      (org-babel-load-file "~/kimim-emacs/README.org")
      (byte-compile-file "~/kimim-emacs/README.el"))
  (load-file  "~/kimim-emacs/README.el"))

(if (>
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.org")))
     (nth 1 (nth 5 (file-attributes "~/kimim-emacs/README.el"))))
    (print "org is newer")
  (print "org is older"))
