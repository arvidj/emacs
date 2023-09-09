;; Shell-script

;; TODO Fix beginning-of-defun
(add-hook 'sh-mode-hook 'arvid-sh-mode-hook)

(defun arvid-sh-mode-hook ()
  ""
  (setq
   sh-indent-comment t
   open-paren-in-column-0-is-defun-start nil)

  (flycheck-mode)

  (aj/define-keys
   sh-mode-map
   `(("RET" newline-and-indent)
     ("ö" ,(make-inserter ";"))
     ("<" self-insert-command)
     ("ä" ,(make-inserter "$")))))

(provide 'arvid-sh)
