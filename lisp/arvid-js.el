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

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-to-list 'auto-mode-alist '("\\.\\(js\\(on\\)?\\|as\\)\\'" . js-mode))
(autoload 'js-mode "js" nil t)
(add-hook 'js-mode-hook 'my-js-mode-hook)

(require 'jquery-doc)
(add-hook 'js-mode-hook 'jquery-doc-setup)


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
  (subword-mode 1)
  (setq default-tab-width 4)

  (setq fill-column 80)

  (define-keys js-mode-map
	`(("ä" insert-dollar-or-jquery)
	  ;; ("$" report-intelligence-level)
	  ("ö" ,(make-inserter ";"))
	  ("M-ä" ,(make-inserter "ä"))
	  ("M-ö" ,(make-inserter "ö"))
	  ("C-c RET" jquery-doc)
	  ("C-c C-k" js-search-documentation)
	  ("C-c C-j" jquery-search-documentation)
	  ("C-n" c-indent-new-comment-line)
	  ("RET" newline-and-indent)
	  ("M-." find-tag)))

  (flycheck-mode t)
  (moz-minor-mode 1))

(defun escape-js-regex () "Escape javascript regex"
  (interactive)
  (let ((re "\\([][.?/*+|(){}]\\)")
	(to "\\\\\\1")
	(txt (delete-and-extract-region (region-beginning) (region-end))))
    (insert (replace-regexp-in-string re to txt))))

;;; For searching jquery documentation via jqapi
(setq jquery-search-url "http://jqapi.com/#p=")

(defun jquery-search-documentation ()
  "Search jQuery documentation for the word at point."
  (interactive)
  (browse-url (concat jquery-search-url (current-word t))))

(setq js-documentation-search-url
	  (concat "http://www.google.com/search?q="
			  "inurl:https://developer.mozilla.org/en/JavaScript/Reference+"))

;; TODO: If no word at point, prompt
(defun js-search-documentation ()
  "Search Mozilla Developer Center for word at point"
  (interactive)
  (browse-url (concat js-documentation-search-url (current-word t))))

(provide 'arvid-js)
