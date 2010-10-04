(require 'etest)

;; Set load path
(add-to-list 'etest-load-path "~/.emacs.d/tests/")

;; Use elisp mode for .etest files
(add-to-list 'auto-mode-alist '("\\.etest$" . emacs-lisp-mode))

;; Key binding for running tests.
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c et") 'etest-execute)))

(provide 'arvid-etest)
