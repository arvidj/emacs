(require 'psvn)

;; Fråga Johan: egentligen vill jag inte sätta dom till
;; scroll*keep-cursor ... jag vill unsetta dom så dom får tillbaka
;; sitt default, vilket ju är scroll*keep-cursor
(add-hook 'svn-status-diff-mode-hook
          '(lambda () 
             (define-key diff-mode-map (kbd "M-p") 'scroll-down-keep-cursor)
             (define-key diff-mode-map (kbd "M-n") 'scroll-up-keep-cursor)))

(global-set-key (kbd "C-x g") 'svn-status)

(provide 'arvid-psvn)
