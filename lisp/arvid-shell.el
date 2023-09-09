(defun aj/recenter-top1 ()
  "Scroll up to the line just above the cursors

Used to emulate terminals C-l with a two-line prompt."
  (interactive)
  (recenter-top-bottom 1))

(defun zsh-shell-mode-setup ()
  (setq-local comint-process-echoes t)
  (shell-dirtrack-mode -1)
  (dirtrack-mode -1)
  (add-hook
   'comint-output-filter-functions #'comint-osc-process-output)
  ;; (setq 'comint-output-filter-functions '(comint-osc-process-output)

  (aj/define-keys
   shell-mode-map
   `(("C-l" aj/recenter-top1) ("C-c C-o" browse-url-at-point))))

(add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

(provide 'arvid-shell)
