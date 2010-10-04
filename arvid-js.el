;; TOOD
;;; A nice way of removing console.log statements.
;;; Font lock for "that", make it keyword-ish


;;; Javascript
;; http://www.nongnu.org/espresso/
;; http://github.com/technomancy/emacs-starter-kit/commit/a43b4a669822f7649ec830a25ae3a256b086655a

;; TODO: fix indentation for multiple var-declarations such as
;;    var a,
;;        b;
;;   is right now indented as
;;    var a,
;;    b;
;;   which sucks
;; TODO: fix imenu support for how I declare modules.
;; TODO: Send chrome javascript errors to emacs. How to hook into
;;       console?

(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?\\'" . js-mode))
(autoload 'js-mode "js" nil t)
(add-hook 'js-mode-hook 'my-js-mode-hook)


;; TODO: if negative arg, wrap preceding word.
(defun insert-dollar-or-jquery ()
  "Wrap transient region if any in $(...), otherwise insert $."
  (interactive)
  (if mark-active
	  (save-excursion
		(let ((end (max (region-end) (region-beginning)))
			  (beginning (min (region-end) (region-beginning))))
		  (goto-char end)
		  (insert ")")
		  (goto-char beginning)
		  (insert "$(")))
	(insert "$")))

(defun my-js-mode-hook () 
  (local-set-key (kbd "C-c C-j") 'jquery-search-documentation)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (c-subword-mode 1)
  (setq default-tab-width 4)
  
  (define-key js-mode-map (kbd "ä") 'insert-dollar-or-jquery)
  (define-key js-mode-map (kbd "$") 'report-intelligence-level)
  
  (define-key js-mode-map (kbd "ö") 'report-intelligence-level)
  (define-key js-mode-map (kbd "M-ö") (lambda () (interactive) (insert "ö")))
  (define-key js-mode-map (kbd ";") 'report-intelligence-level)
  
  (define-key js-mode-map (kbd "M-ä") (lambda () (interactive) (insert "ä"))))

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
