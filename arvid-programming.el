;;;;;;;;;;;;;;;;;;;;;;
;; Programmering

;;; C
(add-hook 'c-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Create my personal style.
(defconst my-c-style
  '((c-hanging-braces-alist     . ((defun-open after)
								   (substatement-open after)))
    (c-offsets-alist            . ((case-label        . +)
				   (arglist-close     . 0)
				   (comment-intro     . 0)))
	(c-hanging-semi&comma-criteria nil))  ;; Does not work
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq c-basic-offset 4)
  (setq tab-width 4)

  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1)
  (c-subword-mode 1)
  ;; (auto-complete-mode 1)

  ;; need to unset this so that I can use it for window-mgmt
  (local-unset-key (kbd "C-M-j")) 
  (local-unset-key (kbd "C-M-k")) 

  (flymake-mode 1)
  (c-toggle-electric-state 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'php-mode-hook 'my-php-mode-hook)

(defun my-php-mode-hook () 
  (local-set-key (kbd "C-c C-t") 'typo3-search-documentation)
  (define-key c-mode-map [remap c-beginning-of-defun] 'beginning-of-defun)
  (define-key c-mode-map [remap c-end-of-defun] 'end-of-defun))


;; Geben for PHP-debugging
(add-to-list 'load-path "~/.emacs.d/plugins/geben")
(autoload 'geben "geben" "PHP Debugger for Emacs" t)


;; Define function documentation function
(setq typo3-search-url "http://typo3.org/fileadmin/typo3api-4.0.0/search.php?query=")
(defun typo3-search-documentation ()
  "Search PHP documentation for the word at point."
  (interactive)
  (browse-url (concat typo3-search-url (current-word t))))

;; måste ha flymake 
;; funkar inte, varför:
;; '(lambda () (define-key c-mode-map (kbd "RET") 'newline-and-indent)))

;;; Python
(add-hook 'python-mode-hook
	  '(lambda () (define-key python-mode-map "\C-m" 'newline-and-indent)))

;;; elisp
(add-hook 'emacs-lisp-mode-hook 
	  '(lambda () (define-key emacs-lisp-mode-map (kbd "C-c C-l") 'eval-buffer)))

;;; Javascript
;;;; Stöd för js2-mode
;; Använd espresso istället!
;; http://www.nongnu.org/espresso/
;; http://github.com/technomancy/emacs-starter-kit/commit/a43b4a669822f7649ec830a25ae3a256b086655a

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

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

;;; PHP-mode
(add-to-list 'load-path "~/.emacs.d/plugins/php-mode-1.5.0")
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

(add-to-list 'auto-mode-alist '("\\.tpl$\\|\\.tmpl$" . sgml-mode))
;; Taggar
 
;;; TypoScript-mode
(autoload 'ts-mode "ts-mode")
(add-to-list 'auto-mode-alist '("\\.ts$" . ts-mode))
(add-hook 'ts-mode-hook 'my-ts-mode-hook)

(defun my-ts-mode-hook () 
  (setq default-tab-width 2)
  (c-subword-mode)
  (define-key ts-mode-map (kbd "C-j") 'join-line))

;;; Zencoding for HTML
;; http://www.emacswiki.org/emacs/ZenCoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'my-sgml-mode-hook) ;; Auto-start on any markup modes
(define-key zencoding-mode-keymap (kbd "C-c C-z") 'zencoding-expand-line)

(defun my-sgml-mode-hook () 
	(define-key sgml-mode-map (kbd "RET") 'newline-and-indent)
	(zencoding-mode))

(add-hook 'sql-mode-hook 'my-sql-mode-hook) 
(defun my-sql-mode-hook () 
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent))

(add-hook 'css-mode-hook 'my-css-mode-hook) 
(defun my-css-mode-hook () 
  (define-key css-mode-map (kbd "RET") 'newline-and-indent))

