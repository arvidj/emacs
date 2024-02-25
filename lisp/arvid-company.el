(use-package
 company
 :ensure t
 :custom (company-tooltip-align-annotations 't)
 :config (define-key company-active-map (kbd "<tab>") nil))

(provide 'arvid-company)
