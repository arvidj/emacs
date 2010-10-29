(require 'autopair)
(autopair-global-mode t) ;; enable autopair in all buffers
(setq autopair-autowrap t)
(setq autopair-blink nil)

;; TODO
;;
;; * When transient mark in html-mode buffer, and < is pressed,
;;   automatically wrap the marked content with the tag instead of using
;;   the normal autopairing.
;; * Disable autopair-newline in html-mode, or figure out what the
;;   purpose of it is.

(provide 'arvid-autopair)
