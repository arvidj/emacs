;;; elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook
 'emacs-lisp-mode-hook
 #'(lambda ()
     (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)))

(use-package
 elisp-autofmt
 :ensure t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(provide 'arvid-lisp)
