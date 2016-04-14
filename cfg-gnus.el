;; erc settings
(require 'erc-join)
(erc-autojoin-mode 1)
(erc-autojoin-enable)
(setq erc-default-server "irc.freenode.net")
(setq erc-autojoin-channels-alist
          '(("irc.freenode.net" "#emacs")))
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; gnus settings
(setq message-directory "~/../../Gnus/Mail/")
(setq gnus-directory "~/../../Gnus/News/")
(setq nnfolder-directory "~/../../Gnus/Mail/Archive")

(setq gnus-agent t)
(setq gnus-agent-expire-days 90)
; prompt for how many articles only for larger than 1000 articles
(setq gnus-large-newsgroup 1000)
(setq gnus-use-cache t)
(setq gnus-fetch-old-headers nil) ; show previous messages in a thread
(setq gnus-thread-indent-level 1)
(add-hook 'gnus-summary-prepare-hook 'gnus-summary-hide-all-threads)
(setq gnus-select-method '(nnml ""))
(setq gnus-secondary-select-methods nil)
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gmane.org"))
(add-to-list 'gnus-secondary-select-methods '(nnml ""))


(provide 'cfg-gnus)
