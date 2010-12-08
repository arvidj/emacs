;; Shell-script

;; TODO Fix beginning-of-defun
(add-hook 'sh-mode-hook
 	  '(lambda ()
		 (setq sh-indent-comment t
			   open-paren-in-column-0-is-defun-start nil)

		 (define-keys
		   sh-mode-map
		   '(("RET" newline-and-indent)))))

(provide 'arvid-sh)
