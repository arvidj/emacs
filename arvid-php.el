;; TODO: Create a chrome extension that sends xdebug backtraces to emacs 

;; Define function documentation function
(setq typo3-search-url "http://typo3.org/fileadmin/typo3api-4.2.6/search.php?query=")
(defun typo3-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat typo3-search-url (current-word t))))

;;; PHP-mode
(add-to-list 'load-path "~/.emacs.d/plugins/php-mode-1.5.0")
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$\\|\\.phpsh$" . php-mode))
 
(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook () 
  (local-set-key (kbd "C-c C-t") 'typo3-search-documentation)
  (define-key c-mode-map [remap c-beginning-of-defun] 'beginning-of-defun)
  (define-key c-mode-map [remap c-end-of-defun] 'end-of-defun)
  (define-key c-mode-map [remap c-mark-function] 'mark-defun)
  (define-key c-mode-map [remap c-fill-paragraph] 'fill-paragraph)
  (define-key c-mode-map (kbd "ä") (lambda () (interactive (insert "$"))))
  (define-key c-mode-map (kbd "$") 'report-intelligence-level)
  (define-key c-mode-map (kbd "M-ä") (lambda () (interactive (insert "ä"))))
  
  ;; I prefer $ not being part of a word. That way, c-backward-subword
  ;; moves to a instead of $ when moving backward in a variable like this:
  ;; $asdfQwerZxcv
  (modify-syntax-entry ?$ ".")

  (setq parens-require-spaces nil))

;; Geben for PHP-debugging
(add-to-list 'load-path "~/.emacs.d/plugins/geben")
(autoload 'geben "geben" "PHP Debugger for Emacs" t)

(provide 'arvid-php)
