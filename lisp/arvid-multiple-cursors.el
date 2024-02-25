
(use-package
 multiple-cursors
 :ensure t
 :bind ("C-RET" . mc/edit-lines)
 :config
 (define-key mc/keymap (kbd "C-i") 'mc/insert-numbers)
 (define-key mc/keymap (kbd "C-]") 'mc-hide-unmatched-lines-mode))

(provide 'arvid-multiple-cursors)
