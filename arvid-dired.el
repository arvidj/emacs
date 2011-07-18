(require 'dired)

;; TODO: Colors per file types, check out dired+
;; TODO: Map over keybindings, see arvid-keys.el

(define-key dired-mode-map (kbd "C-c e") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "I") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c h") '(lambda () (interactive) (dired "~")))

;; http://www.emacswiki.org/emacs-en/DiredSortBySizeAndExtension

(provide 'arvid-dired)
