(require 'fic-mode)

;; TODO add more modes

;; TODO: this does not work since i dont understand proper quoting.
(dolist (hook '(js-mode-hook c-mode-hook emacs-lisp-mode-hook ts-mode-hook))
  (when (fboundp hook)
	(add-hook hook 'turn-on-fic-mode)))

(add-hook 'js-mode-hook 'turn-on-fic-mode)
(add-hook 'c-mode-hook 'turn-on-fic-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)
(add-hook 'ts-mode-hook 'turn-on-fic-mode)

(provide 'arvid-fic)
