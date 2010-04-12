
;;; Python
(add-hook 'python-mode-hook
	  '(lambda () 
		 (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
		 (define-key python-mode-map (kbd "C-M-a") 'my-python-beginning-of-block)
		 (define-key python-mode-map (kbd "C-M-e" ) 'my-python-end-of-block)))

(defun my-python-beginning-of-block () 
  " Goes to the start of current block, and move point to indentation."
  (interactive)
  (python-beginning-of-block)
  (back-to-indentation))

(defun my-python-end-of-block ()
  " Goes to the end of current block, and move point to end of line."
  (interactive)
  (python-end-of-block)
  (move-end-of-line 0))


(provide 'arvid-python)