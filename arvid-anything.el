(require 'anything)

;; Default is f5 which conflicts with my binding for whitespace mode.
(setq anything-command-map-prefix-key "C-a")
(require 'anything-config)
(global-set-key (kbd "C-a C-a") 'anything)

(provide 'arvid-anything)
