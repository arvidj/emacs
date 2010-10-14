;; TODO: Create a chrome extension that sends xdebug backtraces to emacs
;; TODO: Check out http://www.emacswiki.org/emacs/php-completion.el
;; TODO: Generate docstrings
;; TODO: Fix fill-paragraph when writing doc-strings.

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
  ;; Remappings
  (define-key c-mode-map [remap c-beginning-of-defun] 'beginning-of-defun)
  (define-key c-mode-map [remap c-end-of-defun] 'end-of-defun)
  (define-key c-mode-map [remap c-mark-function] 'mark-defun)
  (define-key c-mode-map [remap c-fill-paragraph] 'fill-paragraph)
  
  ;; TODO: Why is this not working!
  (define-keys c-mode-map
  	`(("C-c C-t" typo3-search-documentation)
  	  ("ä" ,(make-inserter "$"))
  	  ("$" report-intelligence-level)
  	  ("M-ä" ,(make-inserter "ä"))
  	  ("ö" ,(make-inserter ";"))))

  ;; I prefer $ not being part of a word. That way, c-backward-subword
  ;; moves to a instead of $ when moving backward in a variable like this:
  ;; $asdfQwerZxcv
  (modify-syntax-entry ?$ ".")

  (setq parens-require-spaces nil))

;; Geben for PHP-debugging
(add-to-list 'load-path "~/.emacs.d/plugins/geben")
(autoload 'geben "geben" "PHP Debugger for Emacs" t)

(provide 'arvid-php)
