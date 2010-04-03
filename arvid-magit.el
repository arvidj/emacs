(require 'magit)


(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))

(provide 'arvid-magit)
