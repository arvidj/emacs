(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 1000)

;; auto-save recentf list every 2 minutes
(run-at-time
 nil (* 2 60)
 (lambda ()
   (let ((inhibit-message t))
     (recentf-save-list))))

(provide 'arvid-recentf)
