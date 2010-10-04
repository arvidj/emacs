;; TODO
;;; If narrowed, disable auto-complete in iedit-mode.
;;; Fix type face for iedit.
;;; Make it more obvious when turned on, in order to avoid mistakes.

(require 'iedit)

(global-set-key (kbd "C-;") 'iedit-mode)

(provide 'arvid-iedit)
