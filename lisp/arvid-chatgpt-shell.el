(use-package
 chatgpt-shell
 :ensure t
 :custom
 ((chatgpt-shell-openai-key
   (lambda () (secrets-get-secret "login" "openai key")))))

(provide 'arvid-chatgpt-shell)
