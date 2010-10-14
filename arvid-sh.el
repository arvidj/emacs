;; Shell-script
(add-hook 'sh-mode-hook
 	  '(lambda ()
	     (setq sh-indent-comment t)))

(provide 'arvid-sh)
