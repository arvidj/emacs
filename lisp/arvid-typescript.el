(use-package nvm :ensure t)

(defun aj/typescript-mode-hook ()
  "My typescript-mode hook."
  ;; Set correct node version
  (nvm-use-for-buffer)
  (subword-mode 1))

(use-package typescript-mode
  :ensure t
  :hook aj/typescript-mode-hook
  :config
  ;; aligns annotation to the right hand side
  (add-to-list 'typescript-mode-hook 'aj/typescript-mode-hook))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         )
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (company-mode +1))

(provide 'arvid-typescript)
