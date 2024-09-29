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
     :hook (emacs-lisp-mode . elisp-autofmt-mode)
     :config
     (defun aj/elisp-autofmt-allow ()
       "Returns true if an .elisp-autofmt file exists and not custom file"
       (and ;; This is too annoying -- disabling autofmt due to hangs
        nil
        (elisp-autofmt-check-elisp-autofmt-exists)
        (not
         (string=
          buffer-file-name
          "/home/arvid/.emacs.d/lisp/arvid-emacs-custom.el"))))

     ;; This is too annoying
     (setq elisp-autofmt-on-save-p 'aj/elisp-autofmt-allow)

     (setq
      elisp-autofmt-python-bin
      (if (executable-find "python3")
          "python3"
        (if (executable-find "python")
            "python"
          (message
           "[arvid-list.el] found no python or python3 binary in path, cannot enable [elisp-autofmt]")
          nil)))))

(provide 'arvid-lisp)
