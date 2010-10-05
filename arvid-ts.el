;; TODO: It would be nice to have a mode for trying out TS.


;;; TypoScript-mode
(autoload 'ts-mode "ts-mode")
(add-to-list 'auto-mode-alist '("\\.ts$\\|/ext_typoscript_setup\\.txt$" . ts-mode))
(add-hook 'ts-mode-hook 'my-ts-mode-hook)

(defun my-ts-mode-hook () 
  (setq tab-width 2)
  (c-subword-mode)
  (define-key ts-mode-map (kbd "C-j") 'join-line))

(provide 'arvid-ts)
