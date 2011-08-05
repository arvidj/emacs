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
	(define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)
	(define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)

	(define-key sgml-mode-map (kbd "C-c th") 'arvid-tag-wrap-header-line-or-region)
	(define-key sgml-mode-map (kbd "C-c tt") 'arvid-tag-wrap-line-or-region)
	(define-key sgml-mode-map (kbd "C-c tp") (lambda () (interactive) (arvid-tag-wrap-line-or-region "p")))
	(define-key sgml-mode-map (kbd "C-c te") (lambda () (interactive) (arvid-tag-wrap-line-or-region "em")))
	(define-key sgml-mode-map (kbd "C-c ts") (lambda () (interactive) (arvid-tag-wrap-line-or-region "strong")))

	;; (dolist (binding '("p" ("b" "br")))
	;;   (let ((key (car binding)) (command (cadr binding)))
	;; 	(global-set-key (read-kbd-macro key) command)))

	(zencoding-mode))

(defun arvid-tag-wrap-line-or-region (tag) 
  ""
  (interactive "MTag: ")
  (let ((beg (if mark-active (region-beginning) (line-beginning-position)))
		(end (if mark-active (region-end) (line-end-position))))
	(save-excursion
	  (goto-char end) (insert (concat "</" tag ">"))
	  (goto-char beg) (insert (concat "<" tag ">")))))

(defun arvid-tag-wrap-header-line-or-region (level)
  (interactive "p")
  (arvid-tag-wrap-line-or-region (concat "h" (number-to-string level))))

(provide 'arvid-sgml)
