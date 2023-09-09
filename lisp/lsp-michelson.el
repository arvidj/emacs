(use-package lsp-mode :ensure t)

(add-to-list
 'lsp-language-id-configuration '(michelson-mode . "michelson"))

;; You need to install
;; https://gitlab.com/nomadic-labs/tezos-lang-server, compile and have
;; tezos-lang-server in your PATH.

;; TODO: make tezos-lang-server installable by opam, and ...

(lsp-dependency 'tezos-lang-server '(:system "tezos-lang-server"))

(lsp-register-client
 (make-lsp-client
  :new-connection
  (lsp-stdio-connection
   (list (lsp-package-path 'tezos-lang-server) "--lang-server"))
  :major-modes '(michelson-mode)
  :server-id 'mils))
