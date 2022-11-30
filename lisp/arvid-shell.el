(add-hook 'shell-mode-hook 'arvid-shell-mode-hook)

(defun arvid-shell-mode-hook () 
  (setenv "PATH"
          (concat
           "/home/arvid/.cabal/bin" ":"
           (getenv "PATH")))

  (ansi-color-for-comint-mode-on)
  (setq comint-prompt-read-only t))

(provide 'arvid-shell)
