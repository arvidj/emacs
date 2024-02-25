(use-package
 projectile
 :ensure t
 :pin melpa-stable
 :init (projectile-mode +1)
 :bind
 (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(provide 'arvid-projectile)
