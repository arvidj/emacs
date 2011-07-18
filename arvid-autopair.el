(require 'autopair)
(autopair-global-mode t) ;; enable autopair in all buffers
(setq autopair-autowrap t)
(setq autopair-blink nil)

;; Enables delete-selection in when auto-pair is active.
;; http://code.google.com/p/autopair/issues/detail?id=24
(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t)


;; TODO
;;
;; * When transient mark in html-mode buffer, and < is pressed,
;;   automatically wrap the marked content with the tag instead of using
;;   the normal autopairing.
;; * Disable autopair-newline in html-mode, or figure out what the
;;   purpose of it is.

(provide 'arvid-autopair)
