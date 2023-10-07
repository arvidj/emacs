
;; (defun enable-disable-ws-trim ()
;;   ""
;;   (interactive)
;;   (let ((enable
;;          (if multiple-cursors-mode
;;              0
;;            1)))
;;     (ws-trim-mode enable)
;;     (electric-pair-mode enable)))


(use-package
 multiple-cursors
 :ensure t
 :config
 ;; ws-trim-mode && electric-pair messes with mc => turn it off.
 ;; (add-hook 'multiple-cursors-mode-hook 'enable-disable-ws-trim)
 (define-key mc/keymap (kbd "C-i") 'mc/insert-numbers)
 (define-key mc/keymap (kbd "C-]") 'mc-hide-unmatched-lines-mode))

(provide 'arvid-multiple-cursors)
