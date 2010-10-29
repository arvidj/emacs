(require 'fic-mode)

(dolist (hook '(js-mode-hook c-mode-hook emacs-lisp-mode-hook ts-mode-hook))
  (when (boundp hook)
	(add-hook hook 'turn-on-fic-mode)))

(provide 'arvid-fic)
