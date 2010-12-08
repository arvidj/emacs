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
 '(ac-candidate-limit 20)
 '(ac-delay 0.1)
 '(add-log-mailing-address "arvid@gluteus.se")
 '(c-basic-offset 8)
 '(comment-style (quote extra-line))
 '(diff-switches "-r -u")
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fic-highlighted-words (quote ("REFACTOR" "FIXME" "TODO" "BUG" "KLUDGE")))
 '(flymake-start-syntax-check-on-newline nil)
 '(grep-files-aliases (quote (("asm" . "*.[sS]") ("c" . "*.c") ("cc" . "*.cc *.cxx *.cpp *.C *.CC *.c++") ("cchh" . "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++") ("hh" . "*.hxx *.hpp *.[Hh] *.HH *.h++") ("ch" . "*.[ch]") ("el" . "*.el") ("h" . "*.h") ("l" . "[Cc]hange[Ll]og*") ("m" . "[Mm]akefile*") ("tex" . "*.tex") ("texi" . "*.texi") ("ts" . "*\.ts"))))
 '(haskell-program-name "ghci -XExistentialQuantification")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching nil)
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`.*\\.hi")))
 '(indent-tabs-mode t)
 '(inferior-haskell-find-project-root nil)
 '(initial-scratch-message "")
 '(jde-jdk-registry (quote (("1.6.0" . "/usr/lib/jvm/java-6-sun/"))))
 '(jira2-wsdl-descriptor-url "http://bugs.gluteus.se:8080/rpc/soap/jirasoapservice-v2?wsdl")
 '(js-enabled-frameworks (quote (javascript prototype)))
 '(js-flat-functions t)
 '(js-indent-level 4)
 '(js2-auto-indent-flag t)
 '(js2-basic-offset 4)
 '(js2-enter-indents-newline t)
 '(js2-mirror-mode t)
 '(js2-mode-escape-quotes nil)
 '(magit-commit-all-when-nothing-staged (quote ask))
 '(magit-completing-read-function (quote ido-completing-read))
 '(magit-process-popup-time 5)
 '(mk-proj-ack-cmd "ack-grep")
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
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-menu-items 30)
 '(ruby-indent-level 4)
 '(safe-local-variable-values (quote ((indent-tabs-mode nil))))
 '(sgml-basic-offset 4)
 '(smerge-command-prefix "m")
 '(sql-mysql-options (quote ("-P 8888")))
 '(svn-log-edit-show-diff-for-commit t)
 '(svn-status-default-diff-arguments nil)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 48 56 64 \.\.\.)))
 '(tab-width 4)
 '(timeclock-file "~/Dropbox/gluteus/timelogs/.timelog")
 '(vc-handled-backends nil)
 '(yas/prompt-functions (quote (yas/ido-prompt yas/no-prompt))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#141414" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "Mensch"))))
 '(compilation-error ((t (:inherit font-lock-warning-face))))
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(font-lock-warning-face ((t (:foreground "red"))))
 '(highlight ((t (:background "green" :underline t :slant italic :weight bold))))
 '(hl-line ((t (:inherit highlight :background "#022"))))
 '(org-level-2 ((t (:inherit outline-8))))
 '(org-level-4 ((t (:inherit outline-6))))
 '(smerge-refined-change ((t (:background "blue")))))
