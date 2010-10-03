(require 'magit)

(add-hook 'magit-mode-hook
          (lambda()
            (set-face-foreground 'magit-diff-add "green3")
            (set-face-foreground 'magit-diff-del "red3")
			(define-key magit-mode-map (kbd "C-o")
			  (lambda () (interactive) (magit-visit-item -1)))))

;; Add spell-checking when writing commit messages.
(add-hook 'magit-log-edit-mode-hook (lambda () (flyspell-mode)))

(global-set-key (kbd "C-x G") 'magit-status)

(provide 'arvid-magit)
