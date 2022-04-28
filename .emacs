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
(defvar kimim/path-kimikit "~/")
;; synchronization driver, for example ~/Dropbox/
(defvar kimim/path-sync "~/")
;; path to synchronize some files in .emacs.d
(defvar kimim/path-sync-emacs (concat kimim/path-sync ".emacs.d/"))
;; path for the GTD files
(defvar kimim/path-org (concat kimim/path-sync "org/"))
;; path to keep notes
(defvar kimim/path-notes (concat kimim/path-sync "notes/"))
;; path to refrence documents
(defvar kimim/path-docs (concat kimim/path-sync "docs/"))

;; if kimim-emacs is clone to ~/kimim-emacs
(load (concat kimim/path-kimim-emacs "init.el"))

(set-register ?e '(file . "~/.emacs"))
(set-register ?i '(file . "~/kimim-emacs/init.el"))
(set-register ?o '(file . "~/kimim-emacs/README.org"))

;; put other local settings below
