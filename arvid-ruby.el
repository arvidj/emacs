(add-hook 'ruby-mode-hook 'arvid-ruby-mode-hook)

(defun arvid-ruby-mode-hook () 
  ""
  ;; "C-c C-e" ends a block.
  (define-key ruby-mode-map (kbd "C-c C-e")
	(lambda () (interactive) (insert "end") (indent-for-tab-command)))

  ;; Add | to auto-pairs
  ;; ... Turns out this is not possible:
  ;; http://code.google.com/p/autopair/issues/detail?id=18
  ;;; (push '(?& . ?&)
  ;;; 		(getf autopair-extra-pairs :code))

  ;; Instead, the author of auto-pair suggest putting the character
  ;; one wants paired in the string-class of the syntax table. Let's
  ;; see how it works.
  (modify-syntax-entry ?| "\""))
  
(provide 'arvid-ruby)
