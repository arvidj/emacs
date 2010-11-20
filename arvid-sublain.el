(autoload 'sublain-list "sublain" "List directory entries in the repository." t)
(autoload 'sublain-bookmark "sublain" "Display bookmark file." t)

(add-hook 'sublain-list-mode-hook
		  (lambda ()
			(define-keys sublain-list-mode-map
			  '(("RET" sublain-list-visit)
				("o" sublain-list-visit)))))

(add-hook 'sublain-bookmark-mode-hook
		  (lambda ()
			(define-keys sublain-bookmark-mode-map
			  '(("RET" sublain-bookmark-visit)
				("o" sublain-bookmark-visit)))))


(provide 'arvid-sublain)
