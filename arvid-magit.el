(require 'magit)
(require 'magit-svn)

(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))

(global-set-key (kbd "C-x G") 'magit-status)

(provide 'arvid-magit)
