;; auto-complete 
;; (when (require 'auto-complete nil t)
;;   (global-auto-complete-mode t)
;;   (set-face-background 'ac-selection-face "steelblue")
;;   (set-face-background 'ac-menu-face "skyblue")
;;   (define-key ac-complete-mode-map "\t" 'ac-expand)
;;   (define-key ac-complete-mode-map (kbd "<C-return>") 'ac-complete)
;;   (define-key ac-complete-mode-map "\C-\M-n" 'ac-next)
;;   (define-key ac-complete-mode-map "\C-\M-p" 'ac-previous)
;;   (setq ac-modes '( haskell-mode ))
;;   (setq ac-auto-start 3)
;;  (setq ac-auto-start t))

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
  (setq ac-sources '(ac-source-yasnippet ac-source-etags ac-source-words-in-buffer ac-source-abbrev ))
  (setq ac-modes '( css-mode haskell-mode c-mode xml-mode html-mode php-mode emacs-lisp-mode js2-mode ts-mode sql-mode ))
  (setq ac-auto-start 3)
  (setq ac-auto-start t))

(provide 'arvid-auto-complete)