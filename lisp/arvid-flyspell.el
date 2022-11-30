(require 'flyspell)

(setq flyspell-auto-correct-binding nil)

(use-package guess-language
  :ensure t
  :config
  (setq guess-language-languages '(en fr sv)))

;; Does not work.
(defun my-flyspell-mode-hook ()
    ""
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (define-key flyspell-mode-map (kbd "M-s-j") 'flyspell-goto-next-error)
  (define-key flyspell-mode-map (kbd "M-s-k") 'flyspell-goto-prev-error)
  ;; (guess-language-mode)
  )

(add-hook 'flyspell-mode-hook 'my-flyspell-mode-hook)

;; for different modes:

(defun my-run-flyspell ()
	""
  (interactive)
  (flyspell-mode)
  ;; for speed
  (when (< (buffer-size) 10000)
    (flyspell-buffer)))

(add-hook 'org-mode-hook 'my-run-flyspell)
(add-hook 'rst-mode-hook 'my-run-flyspell)
(add-hook 'markdown-mode-hook 'my-run-flyspell)

(provide 'arvid-flyspell)
