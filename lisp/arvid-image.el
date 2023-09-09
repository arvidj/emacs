(add-hook image-mode-hook 'arvid-image-mood-hook)

(defun arvid-image-mood-hook ()
  ""
  (interactive)
  (define-key
   image-mood-map (kbd "C-c I")
   (lambda ()
     (interactive)
     (shell-command (concat "identify " (buffer-file-name))))))
