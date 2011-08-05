;; (require 'ruby-end)
;; (ruby-end-mode t)

(setq
 ruby-end-keywords-re
 "\\(?:^\\|\\s-+\\)\\(?:def\\|if\\|class\\|module\\|unless\\|case\\|while\\|do\\|until\\|for\\|begin\\|it\\|describe\\|before\\)")

(add-hook 'ruby-mode-hook 'arvid-ruby-mode-hook)

(let ((rb-files-re (regexp-opt '("Gemfile"))))
  ;; What is up with the back tick monstrosity :(
  (add-to-list 'auto-mode-alist `(,rb-files-re . ruby-mode)))

(defun arvid-ruby-mode-hook ()
  ""
  ;; "C-c C-e" ends a block.
  (define-key ruby-mode-map (kbd "C-c C-e")
	(lambda () (interactive) (insert "end") (indent-for-tab-command)))

  ;; Add | to auto-pairs
  ;; ... Turns out this is not possible:
  ;; http://code.google.com/p/autopair/issues/detail?id=18
  ;;; (push '(?& . ?&)
  ;;; 		(getf autopair-extra-pairs :code))

  ;; Instead, the author of auto-pair suggest putting the character
  ;; one wants paired in the string-class of the syntax table. Let's
  ;; see how it works.
  (modify-syntax-entry ?| "\""))

(provide 'arvid-ruby)
