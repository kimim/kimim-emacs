;;============================================================================
;; org-mode general settings
;;============================================================================

;; exporting format
(load "ox-mediawiki.el")
(load-file "~/.emacs.d/site-lisp/org-reveal/ox-reveal.el")
(require 'ox-reveal)
;; load htmlize.el , which org-babel export syntax highlight source code need it
(require 'htmlize)
(require 'ox-mediawiki)
(require 'ox-md)

;; plant uml setting
(require 'ob-plantuml)
(setenv "GRAPHVIZ_DOT" "C:\\cygwin\\bin\\dot.exe")
(setq org-plantuml-jar-path "~/../Tools/plantuml.jar")

(setq org-hide-leading-stars t)
(setq org-footnote-auto-adjust t)
(setq org-html-validation-link nil)
(setq org-export-creator-string "")
;; src block setting
(setq org-src-window-setup 'current-window)
(setq org-src-fontify-natively t)

;; (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
;; 							  "xelatex -interaction nonstopmode %f"))
;;(setq org-latex-pdf-process '("pdflatex -interaction nonstopmode %f"))
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python .t)
   (emacs-lisp . t)
   (sh . t)
   (dot . t)
   (ditaa . t)
   (js . t)
   (latex . t)
   (plantuml . t)
   (clojure .t)
   (org . t)
   (R . t)
   ))

;;============================================================================
;; Calendar and Holiday Settings
;;============================================================================
(setq calendar-latitude +30.16)
(setq calendar-longitude +120.12)
(setq calendar-location-name "Hangzhou")
(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-solar-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-general-holidays '((holiday-fixed 1 1 "元旦")
                         (holiday-fixed 4 1 "愚人節")
                         (holiday-float 5 0 2 "父親節")
                         (holiday-float 6 0 3 "母親節")))
(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag nil)
(setq calendar-view-holidays-initially-flag nil)
(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

;;============================================================================
;; org-reveal settings for html5 ppt
;;============================================================================
(setq org-reveal-root "reveal.js")
;;(setq org-reveal-root "~/../Tools/reveal.js")
;;(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.5.0/")
(setq org-reveal-theme "simple")
(setq org-reveal-width 1200)
(setq org-reveal-height 750)
(setq org-reveal-transition "fade")
(setq org-reveal-hlevel 2)

;;============================================================================
;; org as GTD system
;;============================================================================
(setq org-todo-keywords
	  '(
        ;; for tasks
        (sequence "TODO(t!)" "SCHED(s)" "|" "DONE(d@/!)")
        ;; for risks, actions, problems
        (sequence "OPEN(o!)" "WAIT(w)" "|" "CLOSE(c@/!)")
        ;; special states
        (sequence "REPEAT(r)" "|" "SOMEDAY(m)" "ABORT(a@/!)")
        ))

(setq org-tag-alist '((:startgroup . nil)
                      ("@office" . ?o) ("@home" . ?h)
                      (:endgroup . nil)
                      ("@team" . ?t) ("@leader" . ?l)
                      ("risk" . ?k)
                      ("sync" . ?s)
                      ("reading" . ?r)
                      ("writing" . ?w)
                      ("project" . ?p)
                      ("habit" . ?H)
                      ("next" . ?n)))
;; Level=2 or 3, state is not DONE/ABORT/CLOSED/SOMEDAY
;; contains no TODO keywords or SOMEDAY
;; contains no project tag
;; subtree contains TODO

(setq org-tags-exclude-from-inheritance '("project"))
(setq org-stuck-projects (quote ("+LEVEL>=2-project-habit/-TODO-SCHED-DONE-OPEN-WAIT-CLOSE-SOMEDAY-REPEAT-ABORT"
                                 ("") nil nil)))
;; (setq org-stuck-projects (quote ("+LEVEL>=2-project-habit/-TODO-SCHED-DONE-OPEN-WAIT-CLOSE-SOMEDAY-REPEAT-ABORT"
;;                                  ("SOMEDAY") ("project") nil)))
(setq org-refile-targets '(;; refile to maxlevel 2 of current file
                           (nil . (:maxlevel . 1))
                           ;; refile to maxlevel 1 of org-refile-files
                           (org-refile-files :maxlevel . 1)
                           ;; refile to item with 'project' tag in org-refile-files
                           (org-refile-files :tag . "project")))
(defadvice org-schedule (after add-todo activate)
  (if (or (string= "OPEN" (org-get-todo-state)) (string= "CLOSE" (org-get-todo-state)))
      (org-todo "WAIT")
    (org-todo "SCHED")))
(defadvice org-deadline (after add-todo activate)
  (if (or (string= "OPEN" (org-get-todo-state)) (string= "CLOSE" (org-get-todo-state)))
      (org-todo "WAIT")
    (org-todo "SCHED")))
(setq org-log-done t)
(setq org-todo-repeat-to-state "REPEAT")
;; settings for org-agenda-view
(setq org-agenda-span 'day)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-deadline-warning-days 2)
(defcustom org-location-home-or-office "office" "office")
(defun org-toggle-office ()
  (interactive)
  (setq org-location-home-or-office "office")
  (setq org-agenda-files (list (concat path-gtd "office/")
                               (concat path-gtd "office/projects/")
                               (concat path-gtd "world.org")
                               (concat path-gtd "capture.org")))
  (setq org-refile-files (append (list (concat path-gtd "capture.org")
                                       (concat path-gtd "world.org")
                                       (concat path-gtd "home/new-words.org"))
                                 (file-expand-wildcards (concat path-gtd "office/*.org"))
                                 (file-expand-wildcards (concat path-gtd "office/*/*.org"))
                                 (file-expand-wildcards (concat path-gtd "home/kimi.org"))))
  (message "Agenda is from office..."))

(defun org-toggle-home ()
  (interactive)
	  (setq org-location-home-or-office "home")
      (setq org-agenda-files (list (concat path-gtd "home/")
                                   (concat path-gtd "world.org")))
      (setq org-refile-files (append (list (concat path-gtd "capture.org")
                                           (concat path-gtd "world.org")
                                           (file-expand-wildcards (concat path-gtd "home/*.org")))))
	  (message "Agenda is from home..."))

(defun org-toggle-home-or-office()
  (interactive)
  (if (string= org-location-home-or-office "home")
	  (org-toggle-office)
    (org-toggle-home)))
(org-toggle-office)

(setq org-agenda-custom-commands
      '(("o" todo "OPEN"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("w" todo "WAIT"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("d" todo "TODO"
         ((org-agenda-sorting-strategy '(priority-down))))
        ("h" tags "habit"
         ((org-agenda-sorting-strategy '(todo-state-down))))))

(setq org-capture-templates
      '(("t" "Inbox Task" entry (file+headline (concat path-gtd "capture.org") "Inbox")
		 "* %?\n/Entered on %U/ \\\\")
        ("o" "Inbox Actions" entry (file+headline (concat path-gtd "capture.org") "Inbox")
         "* OPEN %?\n/Entered on %U/ \\\\")
        ("b" "Bugs" entry (file+headline (concat path-gtd "office/projects/prj-maint.org") "Maintenance")
         "* OPEN PRC:%?\n/Entered on %U/ \\\\")
        ("h" "Habit - 好的行為習慣" entry (file+headline (concat path-gtd "world.org") "Habit")
         "* %?  :habit:\n/Entered on %U/ \\\\")
        ("w" "Work Journal - 工作中的感悟" entry (file+datetree (concat path-gtd "work-journal.org"))
		 "* %?\n  %i")
        ("m" "Meeting Journal - 會議紀要和討論結果" entry (file+datetree (concat path-gtd "meeting-journal.org"))
		 "* %?")
        ("a" "R&D Journal" entry (file+datetree (concat path-gtd "lab-journal.org"))
		 "* %?")
        ("n" "New word" entry (file+headline (concat path-gtd "home/new-words.org") "new-words")
		 "* %?\n")))

;;============================================================================
;; org-mode-reftex-search
;;============================================================================
(defun org-mode-reftex-search ()
 ;; jump to the notes for the paper pointed to at from reftex search
 (interactive)
 (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))

(setq org-link-abbrev-alist
 '(("bib" . "~/reference/ref.bib::%s")
   ("notes" . "~/reference/notes.org::#%s")
   ("papers" . "~/reference/papers/%s.pdf")))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
    ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
    (global-auto-revert-mode t)
    (reftex-parse-all)
    ;; add a custom reftex cite format to insert links
    (reftex-set-cite-format
      '((?b . "[[bib:%l][%l-bib]]")
        (?c . "\\cite{%l}")
        (?n . "[[notes:%l][%l-notes]]")
        (?p . "[[papers:%l][%l-paper]]")
        (?t . "%t")
        (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))


(defadvice org-html-paragraph (before fsh-org-html-paragraph-advice
                                      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let ((fixed-contents)
        (orig-contents (ad-get-arg 1))
        (reg-han "[[:multibyte:]]"))
    (setq fixed-contents (replace-regexp-in-string
                          (concat "\\(" reg-han "\\) *\n *\\(" reg-han "\\)")
                          "\\1\\2" orig-contents))
    (ad-set-arg 1 fixed-contents)
    ))

;;============================================================================
;; function redifinition
;;============================================================================
(defun org-babel-result-to-file (result &optional description)
  "Convert RESULT into an `org-mode' link with optional DESCRIPTION.
If the `default-directory' is different from the containing
file's directory then expand relative links."
  (when (stringp result)
    (if (string= "svg" (file-name-extension result))
        (progn
          (with-temp-buffer
            (if (file-exists-p (concat result ".html"))
                (delete-file (concat result ".html")))
            (rename-file result (concat result ".html"))
            (insert-file-contents (concat result ".html"))
            (message (concat result ".html"))
            (format "#+BEGIN_HTML
<div style=\"text-align: center;\">
%s
</div>
#+END_HTML"
                    (buffer-string)
                    )))
      (progn
        (format "[[file:%s]%s]"
                (if (and default-directory
                         buffer-file-name
                         (not (string= (expand-file-name default-directory)
                                       (expand-file-name
                                        (file-name-directory buffer-file-name)))))
                    (expand-file-name result default-directory)
                  result)
                (if description (concat "[" description "]") ""))))))

;; R-mode
;; Now we set up Emacs to find R
;; The path to R might need to be changed
(setq-default inferior-R-program-name
	      "C:/Program Files/R/R-3.1.1/bin/i386/Rterm.exe")
;(setenv "PATH" (concat "C:\\Program Files\\R\\R-2.15.3\\bin\\i386" ";"
;    (getenv "PATH")))
;;(setq-default inferior-R-program-name "C:/cygwin/lib/R/bin/exec/R.exe")

;; Configuring org mode to know about R and set some reasonable default behavior
;; (require 'ess-site)
(require 'org-install)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-confirm-babel-evaluate nil)
(setq org-export-html-validation-link nil)
(setq org-export-allow-BIND t)
(setq org-support-shift-select t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
