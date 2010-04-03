

;; saker jag inte använder av en eller annan anledning
;; men som tycks vettigt att spara

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
;; (require 'auto-complete)



;; Desktop katalog
;; (desktop-save-mode 1)
;; (add-to-list 'desktop-path "~/.emacs.d/")

;; load the desktop on startup
;; (desktop-load-default)
;; automatically save the desktop on exit.
;; (setq desktop-enable t) 

;; dropdown-list
;; (require 'dropdown-list)



;; för att fixa encodings i dropboxen
;; för emacs 23:
;;(dir-locals-set-class-variables 
;; 'dropbox
;; '((nil . ((set-buffer-file-coding-system "dos")))))
;;
;; (dir-locals-set-directory-class (expand-file-name "~/Dropbox"))

;; (require 'jde-flymake)
;; (require 'flymake "~/.emacs.d/plugins/flymake.el")

;; yasnippet
;; (require 'yasnippet)
;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")
;; (yas/load-directory "~/.emacs.d/snippets")

;; byt buffer med tabbar
;; http://www.dotemacs.de/dotfiles/BenjaminRutt.emacs.html
;, (global-set-key [\C-tab] 'next-buffer)
;; (global-set-key [\S-\C-iso-lefttab] 'previous-buffer)
;; funkar tydligen inte
;; (global-set-key "\C-\t" 'next-buffer) 


;; (global-set-key (kbd "C-ö") 'undo)

