;;; TypoScript-mode
(autoload 'ts-mode "ts-mode")
(add-to-list 'auto-mode-alist '("\\.ts$" . ts-mode))
(add-hook 'ts-mode-hook 'my-ts-mode-hook)

(defun my-ts-mode-hook () 
  (setq default-tab-width 2)
  (c-subword-mode)
  (define-key ts-mode-map (kbd "C-j") 'join-line))

(provide 'arvid-ts)
