(use-package
 recentf
 :hook (after-init . recentf-mode)
 :config
 (setq
  recentf-max-saved-items 1000
  recentf-auto-cleanup 'never)


 ;; auto-save recentf list every 2 minutes
 (run-at-time
  (* 2 60) (* 2 60)
  (lambda ()
    (let ((inhibit-message t))
      (recentf-save-list)))))


(provide 'arvid-recentf)
