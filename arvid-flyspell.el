(require 'flyspell)

;; Does not work.
(define-key flyspell-mode-map (kbd "C-,") 'backward-kill-word)

(provide 'arvid-flyspell)
