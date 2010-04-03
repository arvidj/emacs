;; Shell-script
(add-hook 'shell-script-mode-hook
 	  '(lambda ()
	     (setq sh-indent-comment t)

	     ;; det här funkar inte, jag förstår inte varför
  	     (define-key sh-mode-map (kbd "RET") 'newline-and-indent)
))
	    
;; (add-hook 'shell-script-mode-hook
;; 	  '(lambda ()
;; 	     (define-key sh-mode-map "\C-c\C-c" 'comment-region)
;; 	     (font-lock-mode 1)
;; ))