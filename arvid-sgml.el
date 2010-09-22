(require 'zencoding-mode)

;; TODO: turn off auto-complete-mode when running
;; 'zencoding-expand-line
(define-key zencoding-mode-keymap (kbd "C-c C-z") 
  'zencoding-expand-line)

(add-to-list 'auto-mode-alist '("\\.tpl$\\|\\.tmpl$" . sgml-mode))
 ;; Auto-start on any markup modes
(add-hook 'sgml-mode-hook 'my-sgml-mode-hook)
(defun my-sgml-mode-hook () 
	(define-key sgml-mode-map (kbd "RET") 'newline-and-indent)
	(zencoding-mode))

(provide 'arvid-sgml-mode)
