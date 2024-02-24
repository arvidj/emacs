;; (use-package
;;  direnv
;;  :ensure t
;;  :config (direnv-mode)
;;  ;; This hack, as obtained
;;  ;; [here](https://github.com/wbolster/emacs-direnv/issues/17),
;;  ;; ensures that direnv updates the environment before tuareg-mode
;;  ;; loads, and thus before the load of lsp. This ensure that the
;;  ;; ocamllsp binary from the correct switch is found and used.
;;  (advice-add 'tuareg-mode :before #'direnv-update-environment)
;;  (setq direnv-show-paths-in-summary nil))

(use-package envrc :ensure t :config (envrc-global-mode))

(provide 'arvid-direnv)
