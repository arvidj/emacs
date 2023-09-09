(use-package helm :ensure t)
;; (require 'helm-config)

;; (helm-mode 1)
;; (define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(recentf-mode)
;; (define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "C-a i") 'helm-semantic-or-imenu)

(global-set-key (kbd "M-x") 'helm-M-x)

(unless (boundp 'completion-in-region-function)
  (define-key
   lisp-interaction-mode-map
   [remap completion-at-point]
   'helm-lisp-completion-at-point)
  (define-key
   emacs-lisp-mode-map
   [remap completion-at-point]
   'helm-lisp-completion-at-point))


(provide 'arvid-helm)
