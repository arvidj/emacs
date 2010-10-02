(require 'css-mode)

;; TODO
;;; Function for collapsing / expanding rules written by Fredrik

(add-hook 'css-mode-hook 'arvid-css-mode-hook)
(defun arvid-css-mode-hook ()
  (define-key css-mode-map (kbd "RET") 'newline-and-indent)
  (setup-css-imenu))

(defun css-electric-brace (arg)
  (interactive "P")
  ;; insert a brace
  (self-insert-command 1)
  ;; maybe do electric behavior
  (css-indent-line))

;; Why eval-after-load instead of Hook?
;; Font lock for px, em, %, url-keywords and colors. From rejeep.
(eval-after-load 'css-mode
  '(progn
	 (define-key css-mode-map (kbd "ö") (lambda () (interactive (insert ";"))))
	 (define-key css-mode-map (kbd ";") (lambda () (interactive) (message "Idiot!")))
	 (define-key css-mode-map "}" 'css-electric-brace)
	 (font-lock-add-keywords 'css-mode
                             '(("#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?" . font-lock-reference-face)
                               ("[0-9]+\\(px\\|em\\|%\\)" 1 font-lock-keyword-face)
                               ("\\(url\\)(" 1 font-lock-function-name-face)))))

(defun setup-css-imenu ()
  (setq imenu-generic-expression
  		'(("Selector" find-prev-css-selector 0))))

(defun find-prev-css-selector ()
  "Find the preceding css selector relative to point.

Is probably a bit to convoluted. "
  ;; Go back until { or , is found
  (if (re-search-backward "[{,]" (point-min) t)
	  (progn
		;; Find end of the selector
		(re-search-backward "[^[:space:]]" (point-min) t)
		(let ((end (match-end 0)))
		  ;; Find beginning of the selector
		  (re-search-backward "^\\|," (point-min) t)
		  (re-search-forward "[,[:space:]]*")
		  (let ((beg (match-end 0)))
			;; Set match data bounds of the selector.
			(set-match-data (list beg end))
			(message (match-string 0))
			)))
	nil))

(provide 'arvid-css)
