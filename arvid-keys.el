;;;;;;;;;;;;;;;;;;;;;;
;; Knappar


(defun global-set-keys (bindings)
  "Globally set all BINDINGS."
  (dolist (binding bindings)
    (let ((key (car binding)) (command (cadr binding)))
      (global-set-key (read-kbd-macro key) command))))

(global-set-keys 
 '(("C-c d" duplicate-current-line-or-region)))

;; bra guide: http://xahlee.org/emacs/keyboard_shortcuts.html

;;; snabb-knappar för viktiga config filer
(global-set-key (kbd "<f10>")
  '(lambda()(interactive)(find-file "~/org/ideer.org")))
(global-set-key (kbd "<f12>") ; make F12 switch to .emacs; create if needed
  '(lambda()(interactive)(find-file "~/.emacs.d/init.el"))) 
(global-set-key (kbd "<f11>") ; make F11 switch to xmonad.hs; create if needed
  '(lambda()(interactive)(find-file "~/.xmonad/xmonad.hs"))) 


;; irriterande grejer
(global-unset-key "\C-Z")
;; här fungerade inte (global-unset-key "\C-x \C-z")
(global-unset-key (kbd "\C-x C-z"))


;; Remove noob-bindings
;; (global-unset-key [right])
;; (global-unset-key [left])
;; (global-unset-key [up])
;; (global-unset-key [down])
;; (global-unset-key [next])
;; (global-unset-key [prior])
;; (global-unset-key [home])
;; (global-unset-key [end])

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-#" 'replace-string)
(global-set-key "\M-\"" 'align-regexp)

(global-set-key [\C-return] 'dabbrev-expand)

(global-set-key "\C-a" 'smart-beginning-of-line)
(global-set-key "\M-p" 'scroll-down-keep-cursor)
(global-set-key "\M-n" 'scroll-up-keep-cursor)
(global-set-key "\C-j" 'join-line)

;; window handling
(global-set-key (kbd "C-ä") '(lambda () (interactive) 
			       (enlarge-window 3)))
(global-set-key (kbd "C-Ä") '(lambda () (interactive) 
			       (enlarge-window -3)))
(global-set-key (kbd "C-'") '(lambda () (interactive) 
			       (enlarge-window-horizontally 3)))
(global-set-key (kbd "C-*") '(lambda () (interactive) 
			       (enlarge-window-horizontally -3)))

(global-set-key (kbd "C-c C-k") 'comment-region)



;; Files
(global-set-key (kbd "C-c C-r") 'revert-buffer)

;; Registers
;; Does not work, needs param or something
;; (global-set-key (kbd "C-x r i") '(lambda () (interactive) 
;;                                   (insert-register t)))
(global-set-key (kbd "C-x r a") 'append-to-register)


;; I never use this, but I should
;; (global-set-key (kbd "C-,") (lambda () (interactive)
;; (kill-line 0)))
;; perhaps this is better 
(global-set-key (kbd "C-,") 'backward-kill-word)

;; the formatting of these guys suck
(global-set-key "\C-c\C-d" 'insert-current-date-time)
(global-set-key "\C-c\C-t" 'insert-current-time)

;; Renames current buffer and visiting file.
(global-set-key (kbd "C-c C-r") 'rename-file-and-buffer)

;; Bindings for rejeep-comment
(global-set-key (kbd "C-7") 'comment-or-uncomment-current-line-or-region)

;; Bindings for controlling text scale
(global-set-key (kbd "C-M-+") 'text-scale-increase)
(global-set-key (kbd "C-M--") 'text-scale-decrease)
(global-set-key (kbd "C-M-0") (lambda () (interactive) (text-scale-increase 0)))
