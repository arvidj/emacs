(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 1000)

(global-set-key (kbd "C-x C-r") 'recentf-open)

;; auto-save recentf list every 2 minutes
(run-at-time
 nil (* 2 60)
 (lambda ()
   (let ((inhibit-message t))
     (recentf-save-list))))

(provide 'arvid-recentf)
