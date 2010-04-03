;; Svart bakground och lite andra grejer
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-amsmath-label "eq:")
 '(LaTeX-fill-break-at-separators (quote (\\\()))
 '(TeX-auto-save t)
 '(TeX-parse-self t)
 '(ac-auto-show-menu 0.1)
 '(ac-delay 0.1)
 '(c-basic-offset 8)
 '(comment-style (quote extra-line))
 '(diff-switches "-c -r")
 '(haskell-program-name "ghci -XExistentialQuantification")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching nil)
 '(indent-tabs-mode nil)
 '(inferior-haskell-find-project-root nil)
 '(initial-scratch-message "")
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-6-sun/"))))
 '(jira2-wsdl-descriptor-url "http://bugs.gluteus.se:8080/rpc/soap/jirasoapservice-v2?wsdl")
 '(js2-auto-indent-flag t)
 '(js2-basic-offset 4)
 '(js2-enter-indents-newline t)
 '(js2-mirror-mode t)
 '(js2-mode-escape-quotes nil)
 '(mk-proj-ack-cmd-name "ack-grep")
 '(mk-proj-use-ido-selection t)
 '(org-agenda-files (quote ("~/org")))
 '(org-default-notes-file "~/org/notes.org")
 '(org-remember-templates (quote (("remember" 116 "" nil nil nil))))
 '(org-reverse-note-order t)
 '(org-todo-keyword-faces (quote (("NOPE" :foreground "violet" :weight bold) ("DONE" :foreground "darkgreen" :weight bold) ("WAIT" :foreground "blue" :weight bold) ("TODO" :foreground "red" :weight bold))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE" "WAIT" "NOPE"))))
 '(php-completion-file "~/.emacs.d/plugins/php-completion-file")
 '(php-manual-path "~/docs/php/php-manual-english")
 '(reb-re-syntax (quote string))
 '(sgml-basic-offset 2)
 '(svn-log-edit-show-diff-for-commit t)
 '(svn-status-default-diff-arguments (quote ("-x --ignore-eol-style" "-x -w")))
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 48 56 64 72 80 88 96 104 112 120)))
 '(tab-width 4)
 '(timeclock-file "~/Dropbox/gluteus/timelogs/.timelog"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit highlight :background "#022"))))
 '(org-level-2 ((t (:inherit outline-8))))
 '(org-level-4 ((t (:inherit outline-6)))))