;;; Javascript
;;;; Stöd för js2-mode
;; http://www.nongnu.org/espresso/
;; http://github.com/technomancy/emacs-starter-kit/commit/a43b4a669822f7649ec830a25ae3a256b086655a

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(autoload 'js-mode "js" nil t)
(add-hook 'js-mode-hook 'my-js2-mode-hook)

(defun my-js2-mode-hook () 
  (local-set-key (kbd "C-c C-j") 'jquery-search-documentation)
  (local-set-key (kbd "C-a") 'smart-beginning-of-line)
  (c-subword-mode 1)
)

(defun escape-js-regex () "Escape javascript regex" 
  (interactive) 
  (let ((re "\\([.?/]\\)")
	(to "\\\\\\1")
	(txt (delete-and-extract-region (region-beginning) (region-end))))
    (insert (replace-regexp-in-string re to txt))))

;; For searching jquery documentation
(setq jquery-search-url "http://www.google.com/search?hl=en&aq=f&aqi=&oq=&q=site%3Aapi.jquery.com+jquery+method+")
(defun jquery-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat jquery-search-url (current-word t))))

(provide 'arvid-js)