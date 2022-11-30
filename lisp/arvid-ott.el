;; (load "ott-mode.el")

(require 'ott-mode)

(setq auto-mode-alist (remove '("\\.ott\\'" . archive-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.ott\\'"   . ott-mode))

(provide 'arvid-ott)
