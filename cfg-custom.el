(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "09cd505d845d21f7ea1b61f677d31cd33d8612db")
 '(ecb-compilation-buffer-names
   (quote
    (("*Calculator*")
     ("*vc*")
     ("*vc-diff*")
     ("*Apropos*")
     ("*Occur*")
     ("*shell*")
     ("\\*[cC]ompilation.*\\*" . t)
     ("\\*i?grep.*\\*" . t)
     ("*JDEE Compile Server*")
     ("*Help*")
     ("*Completions*")
     ("*Backtrace*")
     ("*Compile-log*")
     ("*bsh*")
     ("*Messages*")
     ("Calendar")
     ("\\*helm.*\\*" . t)
     ("\\*helm-mode-.*\\*" . t))))
 '(ecb-compile-window-width (quote edit-window) t)
 '(ecb-layout-name "left-kimi0" t)
 '(ecb-layout-window-sizes
   (quote
    (("left-kimi0"
      (ecb-sources-buffer-name 0.18681318681318682 . 0.18518518518518517)
      (ecb-methods-buffer-name 0.18681318681318682 . 0.6296296296296297)
      (ecb-history-buffer-name 0.18681318681318682 . 0.16666666666666666))
     ("left-right-kimi0"
      (ecb-directories-buffer-name 0.2087912087912088 . 0.2777777777777778)
      (ecb-symboldef-buffer-name 0.2087912087912088 . 0.7037037037037037)
      (ecb-methods-buffer-name 0.1813186813186813 . 0.9814814814814815)))))
 '(ecb-minor-mode-text "")
 '(ecb-options-version "2.40")
 '(ecb-source-file-regexps
   (quote
    ((".*"
      ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|scc\\)$\\)\\)" "^GPATH$" "^GRTAGS$" "^GTAGS$")
      ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-source-path (quote (("C:\\Users\\CNKIMA\\Workspace" "workspace"))))
 '(ecb-tag-visit-post-actions
   (quote
    ((default ecb-tag-visit-smart-tag-start ecb-tag-visit-highlight-tag-header ecb-tag-visit-recenter-top)
     (java-mode ecb-tag-visit-goto-doc-start)
     (jde-mode ecb-tag-visit-goto-doc-start))))
 '(ecb-tip-of-the-day nil t)
 '(fill-column 80)
 '(ggtags-find-tag-hook (quote (recenter)))
 '(package-selected-packages
   (quote
    (jedi col-highlight ox-ioslide company history drag-stuff whole-line-or-region w32-registry ox-html5slide htmlize graphviz-dot-mode google-c-style gnuplot find-file-in-repository fill-column-indicator everything elpy ecb cygwin-mount color-theme-github browse-kill-ring bbdb auto-complete-clang-async auto-complete-clang ace-window ace-isearch ac-clang)))
 '(safe-local-variable-values
   (quote
    ((ac-clang-cflags "-I/Users/kimim/Workspace/kimix/include" "-I/Users/kimim/Workspace/kimix/sys/include" "-I/Users/kimim/Workspace/kimix/usr/include" "-I../include")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-bucket-node-face ((t nil)))
 '(ecb-default-general-face ((t (:height 0.8))))
 '(ecb-source-read-only-face ((t (:foreground "steel blue"))))
 '(helm-selection ((t (:background "medium spring green" :distant-foreground "black"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "deep sky blue" :weight normal))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :width normal)))))
