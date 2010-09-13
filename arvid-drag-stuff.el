(require 'drag-stuff)

(drag-stuff-global-mode t)

(setq drag-stuff-modifier '(meta shift))
;; (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "\M-S-n") 'drag-stuff-down)
;; (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)

(define-key drag-stuff-mode-map (kbd "\M-S-p") 'drag-stuff-up)

(provide 'arvid-drag-stuff)
