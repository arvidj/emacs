;; Ws-trim automatically trims the whitespace of lines that are edited.
(require 'ws-trim)
(global-ws-trim-mode t)
;; Set ws-trim-level to 1, only modified lines are trimmed.
(set-default 'ws-trim-level 0)
(setq ws-trim-method-hook '(ws-trim-trailing ws-trim-leading-spaces))
;; Turn off ws-trim in message-mode and eshell-mode
(setq ws-trim-global-modes '(guess (not message-mode eshell-mode ruby-mode)))

;; Ethan-wspace keeps track of whether a file is "clean" (free of trailing
;; whitespace and tabs) when you open it, and will automatically
;; remove them when you save it if and only if it was clean when you
;; started. This means it will keep a file clean if it started
;; clean, and will leave alone any files that were
;; dirty (i.e. someone else didn't follow the rules).
(require 'ethan-wspace)
(global-ethan-wspace-mode nil)

(provide 'arvid-wspace)
