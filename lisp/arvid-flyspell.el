;; Does not work.
(defun aj/flyspell-mode-hook ()
  ""
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key
   flyspell-mode-map (kbd "M-s-j") 'flyspell-goto-next-error)
  (define-key
   flyspell-mode-map (kbd "M-s-k") 'flyspell-goto-prev-error))

(defun aj/run-flyspell ()
  ""
  (interactive)
  (flyspell-mode)
  ;; for speed
  (let ((flyspell-issue-message-flag nil))
    (when (< (buffer-size) 10000)
      (flyspell-buffer))))

(use-package
 flyspell
 :config
 (setq flyspell-auto-correct-binding nil)
 (add-hook 'flyspell-mode-hook 'aj/flyspell-mode-hook)

 ;; for different modes:
 (add-hook 'org-mode-hook 'flyspell-mode)
 (add-hook 'rst-mode-hook 'aj/run-flyspell)
 (add-hook 'markdown-mode-hook 'aj/run-flyspell))

(use-package
 guess-language
 :ensure t
 :config (setq guess-language-languages '(en fr sv)))

(provide 'arvid-flyspell)
