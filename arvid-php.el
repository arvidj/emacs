;; TODO: Create a chrome extension that sends xdebug backtraces to emacs
;; TODO: Check out http://www.emacswiki.org/emacs/php-completion.el
;; TODO: Generate docstrings
;; TODO: Fix fill-paragraph when writing doc-strings.

(require 'php-doc)

;; Define function documentation function
(setq typo3-search-url "http://typo3.org/fileadmin/typo3api-4.2.6/search.php?query=")
(defun typo3-search-documentation ()
"Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat typo3-search-url (current-word t))))

;;; PHP-mode
(add-to-list 'load-path "~/.emacs.d/plugins/php-mode-1.5.0")
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; Could use php-file-patterns?
(add-to-list 'auto-mode-alist '("\\.php$\\|\\.phpsh$" . php-mode))



(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'php-mode-hook 'my-common-php-mode-hook)
(add-hook 'inferior-php-mode-hook 'my-common-php-mode-hook)
(defun my-common-php-mode-hook ()
  ;; Remappings
  (c-subword-mode 1)
  
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
	  ("ö" ,(make-inserter ";"))
	  ("M-ö" ,(make-inserter "ö"))

	  (":" (lambda ()
			 (interactive)
			 (if (not (memq (face-at-point) '(font-lock-string-face font-lock-comment-face)))
				 (insert "=>")
			   (insert ":"))))

	  ("M-:" ,(make-inserter ":"))
	  ("å" ,(make-inserter "->"))
	  ("M-å" ,(make-inserter "å"))))

  ;; I prefer $ not being part of a word. That way, c-backward-subword
  ;; moves to a instead of $ when moving backward in a variable like this:
  ;; $asdfQwerZxcv
  (modify-syntax-entry ?$ ".")

  (setq parens-require-spaces nil)

  ;; For php-doc mode
  (setq php-doc-directory "/usr/share/doc/php-doc/html/")
  (local-set-key (kbd "\C-c RET") 'php-doc)
  (set (make-local-variable 'eldoc-documentation-function)
	   'php-doc-eldoc-function)
  (eldoc-mode 1))

(defun my-php-mode-hook ()
  ;; Activate Flymake-mode
  (flymake-mode 1))

;; Add the hook
(add-hook 'before-save-hook 'php-clean-trailing-starting-whitespace)
(setq php-file-patterns '("\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'"))
(defun php-clean-trailing-starting-whitespace ()
  ""
  (interactive)

  ;; If this is an php-buffer
  (let ((pat (mapconcat (lambda (str) (concat "\\(" str "\\)")) php-file-patterns "\\|")))
	(when (string-match pat (buffer-name))
	  (save-excursion
		;; Remove trailing whitespace.
		(goto-char (point-max))

		;; Remove every trailing blank line
		(while (re-search-backward "^[[:space:]]*$" (line-beginning-position) t)
			   (previous-line)
			   (goto-char (line-end-position))
			   (kill-line))

		;; Remove trailing whitespace on last line
		(goto-char (line-beginning-position))
		(if (re-search-forward "[[:space:]]+$" (line-end-position) t)
			(replace-match ""))

		;; Remove trailing whitespace.
		(goto-char (point-min))

		;; Remove every trailing blank line
		(while (re-search-forward "^[[:space:]]*$" (line-end-position) t)
			   (kill-whole-line))

		;; Remove trailing whitespace on last line
		(goto-char (line-beginning-position))
		(if (re-search-forward "^[[:space:]]+" (line-end-position) t)
			(replace-match ""))
		))))

;; Flymake for PHP, see
;; http://sachachua.com/blog/2008/07/emacs-and-php-on-the-fly-syntax-checking-with-flymake/
(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (add-to-list 'flymake-err-line-patterns
			   '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
  (add-to-list 'flymake-err-line-patterns
			   '("^\\(.*?\\):\\([0-9]+\\)[[:space:]]+\\(Warning: .*?\\)$" 1 2 nil 3))
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
		 (local (file-relative-name temp (file-name-directory buffer-file-name))))
	(list "phpmd_warning.sh" (list local "text" "codesize,unusedcode,naming"))))
	;; (list "php" (list "-f" local "-l"))))

;; PHP-extensions
(add-to-list 'flymake-allowed-file-name-masks '("\\.php\(sh\)?$" flymake-php-init))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.php\(sh\)?$" flymake-phpmd-init))

;; Geben for PHP-debugging
(add-to-list 'load-path "~/.emacs.d/plugins/geben")
(autoload 'geben "geben" "PHP Debugger for Emacs" t)
(setq geben-pause-at-entry-line nil
	  geben-display-window-function 'pop-to-buffer)
(defun my-geben-context-mode-hook ()
  ""
  (interactive)
  (message "load")

  (define-key geben-context-mode-map (kbd "u") 'my-geben-context-up))
(add-hook 'geben-context-mode-hook 'my-geben-context-mode-hook)

(defun my-geben-context-up ()
  ""
  (interactive)
  (message "up")
  (let ((start-column (current-column)))
	(widget-backward 1)
	(while (and (not (memq (car (widget-tabable-at)) '(tree-widget-close-icon tree-widget-open-icon)))
				(eq start-column (current-column)))
	  (widget-backward 1)
	  )))

(defun geben-find-cursor () 
  ""
  (interactive)

  ;; use geben-sessions to find the session
  ;; find the overlay that represents the cursor
  ;; use overlay-buffer to get the buffere

  (let* ((session (car geben-sessions))
		 (cursor (geben-session-cursor session))
		 (overlay (plist-get cursor :overlay))
		 (line-no (cdr (plist-get cursor :position)))
		 (buffer (overlay-buffer overlay)))
	(switch-to-buffer buffer)
	(goto-line line-no)))

(defun typo3-include-class () 
  ""
  (interactive)
  ;; Find the file required
  ;;   Is it a t3lib / tslib other well known systme path (t3lib files require no include though)
  ;;   We can probably use tags to find which file includes the class.
  ;;   Is it an extension? If so we should use t3lib_extMgm::extPath . $path
  ;; Find a place to put the require. This would be underneath the copyright notice,
  )

(defun arvid-narrow-to-scope () 
  ""
  (interactive)
  (save-excursion
	(backward-up-list)
	(save-excursion
	  (beginning-of-line)
	  (setq start (point)))
	(forward-list)
	(end-of-line)
	(narrow-to-region start (point))))
(global-set-key (kbd "C-x n s") 'arvid-narrow-to-scope)

(provide 'arvid-php)
