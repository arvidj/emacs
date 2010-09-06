(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-etags)

(when (require 'auto-complete nil t)
  ;; Toggle Auto-Complete mode in every possible buffer.
  (global-auto-complete-mode t) 
  (set-face-background 'ac-selection-face "steelblue") 
  ;; (set-face-background 'ac-menu-face "skyblue")
  ;; (define-key ac-complete-mode-map "\M-ö" 'ac-expand) ;; remove mapping
  ;; (define-key ac-complete-mode-map (kbd "<C-return>") 'ac-complete)
  (define-key ac-complete-mode-map (kbd "M-ö") 'ac-complete)
  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  (define-key ac-complete-mode-map (kbd "TAB") nil)
	
	;; Remove irritating auto complete on RET
	(define-key ac-complete-mode-map (kbd "RET") nil)
	
	(setq ac-sources '(ac-source-yasnippet ac-source-etags ac-source-words-in-buffer ac-source-abbrev ))
	(setq ac-modes '( css-mode haskell-mode c-mode xml-mode html-mode php-mode emacs-lisp-mode js-mode ts-mode sql-mode ))
  (setq ac-auto-start 3)
  (setq ac-auto-start t))

(provide 'arvid-auto-complete)
