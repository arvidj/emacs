;;; elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook
 'emacs-lisp-mode-hook
 #'(lambda ()
     (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)))

;; TODO: hack since this package only works on emacs 29.1 and my
;; laptop runs an older version.
(if (version<= "29.1" emacs-version)
    (use-package
      elisp-autofmt
      :ensure t
      :commands (elisp-autofmt-mode elisp-autofmt-buffer)
      :hook (emacs-lisp-mode . elisp-autofmt-mode)))

(provide 'arvid-lisp)
