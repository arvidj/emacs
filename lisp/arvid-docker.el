(use-package
 dockerfile-mode
 :ensure t
 :init
 (add-to-list 'auto-mode-alist '(".*Dockerfile.*" . dockerfile-mode)))

(provide 'arvid-docker)
