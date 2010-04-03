(require 'dired)

(define-key dired-mode-map (kbd "C-c e") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-c u") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c h") '(lambda () (interactive) (dired "~")))
(define-key dired-mode-map (kbd "C-c h") '(lambda () (interactive) (dired "~")))

(provide 'arvid-dired)
