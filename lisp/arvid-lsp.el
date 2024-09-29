(defun aj/lsp-format-buffer ()
  ""
  (interactive)
  (when (lsp-feature? "textDocument/formatting")
    (lsp-format-buffer)))
		
(use-package
 lsp-mode
 :ensure t
 :init
 ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
 (setq lsp-keymap-prefix "C-c l")
 :hook
 ( ;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  (tuareg-mode . lsp-deferred)
  (gitlab-ci-mode . lsp)
  (sh-mode . lsp)
  ;; if you want which-key integration
  (lsp-mode . lsp-enable-which-key-integration)
  (before-save . aj/lsp-format-buffer))
 :commands lsp
 :config
 ;; lsp-signature-mode-map
 (unbind-key "M-a" lsp-signature-mode-map)
 (unbind-key "M-n" lsp-signature-mode-map)
 (unbind-key "M-p" lsp-signature-mode-map)

 ;; This language server thing is very annoying
 (setq lsp-disabled-clients '(semgrep-ls))

 ;; Avoids crazy highlighting in OCaml
 (setq lsp-symbol-highlighting-skip-current t)

 ;; Associate GitLab CI Yaml files with the given schema, to enable
 ;; inline validation and documentation.
 (setq
  lsp-yaml-schemas
  '((https://gitlab.com/gitlab-org/gitlab/-/raw/master/app/assets/javascripts/editor/schema/ci.json
     . ["/.gitlab/ci/*/*.yml"])))

 ;; Turn off the default yaml formatter
 (setq lsp-yaml-format-enable nil))

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

;; optional if you want which-key integration
(use-package which-key :ensure t :config (which-key-mode))

(provide 'arvid-lsp)
