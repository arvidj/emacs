;; Ws-trim automatically trims the whitespace of lines that are edited.

(use-package ws-trim
  :ensure t
  :config

  (set-default 'ws-trim-level 0)
  (setq ws-trim-method-hook '(ws-trim-trailing ws-trim-leading))
  (setq ws-trim-global-modes '(guess (not message-mode eshell-mode)))
  (global-ws-trim-mode t))


;; TODO: guess level when opening files. if a file is in a git repo,
;; but not yet added, then we can use a high trimming level. otherwise
;; 0.

;; Set ws-trim-level to 1, only modified lines are trimmed.
;; Turn off ws-trim in message-mode and eshell-mode

;; Disabled because it is too slow on big PHP buffers

;; Ethan-wspace keeps track of whether a file is "clean" (free of trailing
;; whitespace and tabs) when you open it, and will automatically
;; remove them when you save it if and only if it was clean when you
;; started. This means it will keep a file clean if it started
;; clean, and will leave alone any files that were
;; dirty (i.e. someone else didn't follow the rules).
;; (require 'ethan-wspace)
;; (set-default 'ethan-wspace-errors (remove 'tabs ethan-wspace-errors))
;; (global-ethan-wspace-mode nil)

(provide 'arvid-wspace)
