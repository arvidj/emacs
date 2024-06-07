(defun aj/recenter-top1 ()
  "Scroll up to the line just above the cursors

Used to emulate terminals C-l with a two-line prompt."
  (interactive)
  (recenter-top-bottom 1))

(defun aj/zsh-shell-mode-setup ()
  ;; (setq-local comint-process-echoes nil)
  (shell-dirtrack-mode -1)
  (dirtrack-mode -1)
  ;; (company-mode 0)
  (add-hook
   'comint-output-filter-functions #'comint-osc-process-output)
  ;; (setq comint-output-filter-functions '(comint-osc-process-output))

  ;; (aj/define-keys
  ;;    shell-mode-map
  ;;   `(("C-l" aj/recenter-top1) ("C-c C-o" browse-url-at-point)))
  )

(add-hook 'shell-mode-hook #'aj/zsh-shell-mode-setup)

(setq explicit-shell-file-name "/bin/zsh")

(provide 'arvid-shell)
