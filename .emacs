;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq system-name "kimi.im"
      user-login-name "kimim"
      user-full-name "Kimi Ma"
      user-mail-address "kimim@kimi.im")

;; root directory
(defvar kimim/path-root "/")
;; path of kimim-emacs files
(defvar kimim/path-kimim-emacs "~/kimim-emacs/")
;; favorate applications on Windows
(defvar kimim/path-kimikit "~/kimikit/")
;; synchronization driver, for example ~/Dropbox/
(defvar kimim/path-sync "~/")
(if (not (file-exists-p (concat kimim/path-sync "kimikit/emacs.d")))
    (make-directory (concat kimim/path-sync "kimikit/emacs.d") t))
;; path for the GTD files
(defvar kimim/path-org (concat kimim/path-sync "org/"))
;; path to keep notes
(defvar kimim/path-notes (concat kimim/path-sync "notes/"))
;; custom.el file location
(defvar kimim/file-custom "~/.emacs.d/custom.el")

;; if kimim-emacs is clone to ~/kimim-emacs
(load-file (concat kimim/path-kimim-emacs "init.el"))

(set-register ?e '(file . "~/.emacs"))
(set-register ?i '(file . "~/kimim-emacs/init.el"))
(set-register ?o '(file . "~/kimim-emacs/README.org"))

;; put other local settings below
