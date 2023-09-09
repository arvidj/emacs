(defun aj/tag-wrap-line-or-region (tag)
  ""
  (interactive "MTag: ")
  (let ((beg
         (if mark-active
             (region-beginning)
           (line-beginning-position)))
        (end
         (if mark-active
             (region-end)
           (line-end-position))))
    (save-excursion
      (goto-char end)
      (insert (concat "</" tag ">"))
      (goto-char beg)
      (insert (concat "<" tag ">")))))

(defun aj/tag-wrap-header-line-or-region (level)
  (interactive "p")
  (aj/tag-wrap-line-or-region (concat "h" (number-to-string level))))

(defun aj/sgml-mode-hook ()
  (define-key sgml-mode-map (kbd "RET") 'newline-and-indent)
  (define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)
  (define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)

  (define-key
   sgml-mode-map (kbd "C-c th") 'aj/tag-wrap-header-line-or-region)
  (define-key
   sgml-mode-map (kbd "C-c tt") 'aj/tag-wrap-line-or-region)
  (define-key
   sgml-mode-map (kbd "C-c tp")
   (lambda ()
     (interactive)
     (aj/tag-wrap-line-or-region "p")))
  (define-key
   sgml-mode-map (kbd "C-c te")
   (lambda ()
     (interactive)
     (aj/tag-wrap-line-or-region "em")))
  (define-key
   sgml-mode-map (kbd "C-c ts")
   (lambda ()
     (interactive)
     (aj/tag-wrap-line-or-region "strong")))

  (modify-syntax-entry ?\' "." html-mode-syntax-table))

(use-package
 sgml-mode
 :config (add-hook 'sgml-mode-hook 'aj/sgml-mode-hook))

(provide 'arvid-sgml)
