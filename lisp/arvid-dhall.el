;; dhall-mode highlight the syntax and run dhall format on save
(use-package dhall-mode
  :ensure t
  :config
  (setq
    ;; uncomment the next line to disable automatic format
    ;; dhall-format-at-save nil

    ;; comment the next line to use unicode syntax
    dhall-format-arguments (\` ("--ascii"))

    ;; header-line is obsoleted by lsp-mode
    dhall-use-header-line nil))

;; lsp-mode provides the lsp client and it configure flymake to explain errors
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  ;; :hook ((dhall-mode . lsp))
  :commands lsp)

;; lsp-ui shows type annotations on hover
(use-package lsp-ui
  :ensure t
  ;; :hook ((lsp-mode-hook . lsp-ui-mode))
  )

;; company-lsp simplifies completion-at-point
(use-package company-lsp
  :ensure t
  :after company
  :init
  (push 'company-lsp company-backends))
