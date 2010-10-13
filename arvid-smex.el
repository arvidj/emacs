(require 'smex)
(smex-initialize)

;; TODO: Smex can show unbound commands. But what is really
;; interesting is not the commands that are unbounds, rather commands
;; that is use often via M-x. These may or may not have bindings, the
;; problem is that is use them in an inconvenient way.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(provide 'arvid-smex)
