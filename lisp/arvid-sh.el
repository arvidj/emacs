;; Shell-script

;; TODO Fix beginning-of-defun
(add-hook 'sh-mode-hook 'arvid-sh-mode-hook)

(defun arvid-sh-mode-hook ()
  ""
  (setq
   sh-indent-comment t
   open-paren-in-column-0-is-defun-start nil)

  (flycheck-mode))

(provide 'arvid-sh)
