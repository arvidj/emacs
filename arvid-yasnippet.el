;; TODO: smarter new-snippet
;;;  After creating a snippet, it should now what to save it as, and
;;;  where, using mode-name and key of the snippet.

(require 'yasnippet)

(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

(global-set-key (kbd "C-c yn") 'yas/new-snippet)
(global-set-key (kbd "C-c yr") 'yas/reload-all)

(provide 'arvid-yasnippet)
