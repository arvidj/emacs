(require 'magit)
(require 'magit-svn)

;; TODO: Modify magit-visit-item so that (magit-visit-item -1) visits
;; item in other window.

(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")))

;; Add spell-checking when writing commit messages.
(add-hook 'magit-log-edit-mode-hook (lambda () (flyspell-mode)))

(global-set-key (kbd "C-x G") 'magit-status)

(provide 'arvid-magit)
