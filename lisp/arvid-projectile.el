(use-package ag :ensure t :config)

(use-package
 projectile
 :ensure t
 :config
 ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
 (define-key
  projectile-mode-map (kbd "C-c p") 'projectile-command-map)
 (projectile-mode +1))


(provide 'arvid-projectile)
