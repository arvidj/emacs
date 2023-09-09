;; Fails with when there are rules like:
;; background-color: rgba(240,241,242,0.97);
;; Add line number to make unique.
(defun aj/find-prev-css-selector ()
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
            (message (match-string 0)))))
    nil))

(defun aj/setup-css-imenu ()
  (setq imenu-generic-expression
        '(("Selector" aj/find-prev-css-selector 0))))

(defun aj/css-mode-hook ()
  (define-key css-mode-map (kbd "RET") 'newline-and-indent)
  (aj/setup-css-imenu))

(defun aj/css-electric-brace (arg)
  (interactive "P")
  ;; insert a brace
  (self-insert-command 1)
  ;; maybe do electric behavior
  (css-indent-line))

(use-package
 css-mode
 :commands css-mode
 :config

 (aj/define-keys
  css-mode-map
  `(("ö" ,(aj/make-inserter ";"))
    ;; (";" report-intelligence-level)
    ("}" aj/css-electric-brace)))

 (font-lock-add-keywords
  'css-mode
  '(("#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?"
     .
     font-lock-reference-face)
    ("[0-9]+\\(px\\|em\\|%\\)" 1 font-lock-keyword-face)
    ("\\(url\\)(" 1 font-lock-function-name-face)))

 (add-hook 'css-mode-hook 'aj/css-mode-hook))

(use-package rainbow-mode :ensure t :commands rainbow-mode)

;; CSS and Rainbow modes
(defun all-css-modes ()
  (css-mode)
  (rainbow-mode))

;; Load both major and minor modes in one call based on file type
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))


(provide 'arvid-css)
