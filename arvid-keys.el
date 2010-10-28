;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;; Nice guide: http://xahlee.org/emacs/keyboard_shortcuts.html
(global-unset-keys
 '("C-Z"
   "\C-x C-z"
   
   ;; Goal columns is bugging me out
   "C-x C-n"
   "C-x C-b"
   ))

(defun make-find-file (file) 
  `(lambda () (interactive) (find-file ,file)))
(defun make-find-file-in-dir (dir)
  `(lambda () (interactive) (ido-find-file-in-dir ,dir)))

(global-set-keys
 `(("C-c d" duplicate-current-line-or-region)

   ;;; Quick access to some config files
   ("<f9>" ,(make-find-file "~/gluteus/information.org"))
   ("<f10>" ,(make-find-file "~/org/ideer.org"))
   ("<f11>" ,(make-find-file "~/.xmonad/xmonad.hs"))
   ("<f12>" ,(make-find-file "~/.emacs.d/init.el"))
   
   ("C-x nf" ,(make-find-file-in-dir "~/.emacs.d/"))
   ("C-x nh" ,(make-find-file-in-dir "~/"))

   ("M-g" goto-line)
   ("M-#" replace-string)
   ("M-\"" align-regexp)

   ("C-a" smart-beginning-of-line)
   ("M-p" scroll-down-keep-cursor)
   ("M-n" scroll-up-keep-cursor)
   
   ("C-å" join-line)
   
   ;; Window handling
   ("C-ä" (lambda () (interactive) (enlarge-window 3)))
   ("C-Ä" (lambda () (interactive) (enlarge-window -3)))
   ("C-'" (lambda () (interactive) (enlarge-window-horizontally 3)))
   ("C-*" (lambda () (interactive) (enlarge-window-horizontally -3)))
   ("C-M-3" (lambda () (interactive)
			  (delete-other-windows)
			  (split-window-horizontally)
			  (split-window-horizontally)
			  (balance-windows)))

   ;; TODO: in temporary windows (define windows?) bind alt-q to kill
   ;; buffer instead of just quitting. Also kill all dired some time
   ;; soon.

   ;; Files
   ("C-c C-r" revert-buffer)


   ;; Registers
   ;; Does not work, needs param or something
   ;; (global-set-key (kbd "C-x r i") '(lambda () (interactive)
   ;;                                   (insert-register t)))
   ("C-x r a" append-to-register)

   ;; I never use this, but I should
   ("C-," backward-kill-word)

   ;; The formatting of these guys suck
   ("C-c C-d" insert-current-date-time)
   ("C-c C-t" insert-current-time)

   ;; Renames current buffer and visiting file.
   ("C-c C-r" rename-file-and-buffer)

   ;; Bindings for rejeep-comment
   ("C-7" comment-or-uncomment-current-line-or-region)

   ;; These keys are always a pain.
   ("M-U" ,(make-inserter "["))
   ("M-I" ,(make-inserter "]"))
   ("M-J" ,(make-inserter "("))
   ("M-K" ,(make-inserter ")"))
   ("M-M" ,(make-inserter "{"))
   ("M-;" ,(make-inserter "}"))
   ("M-D" delete-pair)

   ;; Calculator button I had on one keyboard.
   ("<XF86Calculator>" calc)
   ("<f5>" whitespace-mode)
   ("<f6>" linum-mode)

   ("M-Q" unfill-paragraph)

   ;; Bindings for controlling text scale
   ("C-M-+" text-scale-increase)
   ("C-M--" text-scale-decrease)
   ("C-M-0" (lambda () (interactive) (text-scale-increase 0)))

   ("M-j" backward-char)
   ("M-k" next-line)
   ("M-i" previous-line)
   ("M-l" forward-char)

   ;; Append / prepend to lines
   ("C-c n" append-to-lines)
   ("C-c j" prepend-to-lines)

   ("C-x B" ibuffer)))
