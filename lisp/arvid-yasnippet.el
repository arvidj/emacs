;; TODO: smarter new-snippet
;;;  After creating a snippet, it should now what to save it as, and
;;;  where, using mode-name and key of the snippet.
;; TODO: quick menu showing all available modes in the bottom of the
;; screen, similar to how magit shows available switches.

(use-package
 yasnippet
 :ensure t
 :config
 (setq yas/snippet-dirs '("~/.emacs.d/snippets"))
 (yas/global-mode 1)

 (global-set-key (kbd "C-c yn") 'yas/new-snippet)
 (global-set-key (kbd "C-c yr") 'yas/reload-all))

(use-package auto-yasnippet :ensure t)

(provide 'arvid-yasnippet)
