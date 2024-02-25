(use-package
 lsp-mode
 :ensure t
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "C-c l")
 :hook
 ( ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (tuareg-mode . lsp-deferred)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  (before-save . lsp-format-buffer))
 :commands lsp
 :config)

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

;; optional if you want which-key integration
(use-package which-key :ensure t :config (which-key-mode))

(provide 'arvid-lsp)
