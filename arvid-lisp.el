;;; elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 
	  '(lambda () (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)))

(provide 'arvid-lisp)