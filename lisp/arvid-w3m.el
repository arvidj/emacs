
(defun my-w3m-hook () 
  ""
  (interactive)
  (define-keys w3m-mode-map
	'(("M-n" scroll-up-keep-cursor)
	  ("M-j" backward-char)
	  ("M-k" next-line)
	  ("M-i" previous-line)
	  ("M-l" forward-char)
	  ("o" arvid-w3m-open-in-tab))))

(defun arvid-w3m-open-in-tab () 
  (interactive)
  (w3m-view-this-url nil t)
  (switch-to-buffer nil))

(add-hook 'w3m-mode-hook 'my-w3m-hook)
(setq w3m-use-tab t)

(provide 'arvid-w3m)
