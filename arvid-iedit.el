(add-to-list 'load-path "~/.emacs.d/plugins/iedit")
(require 'iedit)

(global-set-key (kbd "C-;") 'iedit-mode)

;; Om iedit-mode startas, och narrow-buffer är igång, stäng av
;; autocomplete.

(provide 'arvid-iedit)
