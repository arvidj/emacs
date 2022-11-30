(use-package smex
  :ensure t
  :config

  (smex-initialize)

  ;; TODO: Smex can show unbound commands. But what is really
  ;; interesting is not the commands that are unbounds, rather commands
  ;; that is use often via M-x. These may or may not have bindings, the
  ;; problem is that is use them in an inconvenient way.

  (global-set-keys
   '(("M-x" smex)
     ;; <menu> is between my r-alt and r-control on the laptop.
     ("<menu>" smex)
     ("M-X" smex-major-mode-commands)
     ;; This is your old M-x.

     ("C-c C-x M-x" execute-extended-command)
     )))

(provide 'arvid-smex)
