(use-package
 anything
 :ensure t
 :config
 ;; Default is f5 which conflicts with my binding for whitespace mode.
 ;; (global-unbind-key (kbd "C-a"))
 (setq anything-command-map-prefix-key "C-a")
 (global-set-key (kbd "C-a C-a") 'anything))

(provide 'arvid-anything)
