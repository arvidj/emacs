;;; Javascript
;; http://www.nongnu.org/espresso/
;; http://github.com/technomancy/emacs-starter-kit/commit/a43b4a669822f7649ec830a25ae3a256b086655a

(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . js-mode))
(autoload 'js-mode "js" nil t)
(add-hook 'js-mode-hook 'my-js-mode-hook)

(defun my-js-mode-hook () 
  (local-set-key (kbd "C-c C-j") 'jquery-search-documentation)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (c-subword-mode 1)
  (setq default-tab-width 4))

(defun escape-js-regex () "Escape javascript regex" 
  (interactive) 
  (let ((re "\\([.?/]\\)")
	(to "\\\\\\1")
	(txt (delete-and-extract-region (region-beginning) (region-end))))
    (insert (replace-regexp-in-string re to txt))))

;;; For searching jquery documentation via jqapi
(setq jquery-search-url "http://jqapi.com/#p=")

(defun jquery-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat jquery-search-url (current-word t))))

(provide 'arvid-js)
