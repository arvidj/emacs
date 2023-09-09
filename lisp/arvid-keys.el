;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

;; Nice guide: http://xahlee.org/emacs/keyboard_shortcuts.html
(aj/global-unset-keys
 '("C-Z" "\C-x C-z"

   "C-x C-n" "C-x C-b"

   "C-x m" "C-n" "C-p" "C-b" "C-e" "C-a"))

(defun aj/make-find-file (file)
  `(lambda ()
     (interactive)
     (find-file ,file)))

(defun aj/make-find-file-readonly (file)
  `(lambda ()
     (interactive)
     (find-file-read-only ,file)))

;; use ido-file-internal instead, since we now cant open dired from
;; ido-find-file-in-dir.
(defun aj/make-find-file-in-dir (dir)
  `(lambda ()
     (interactive)
     (let ((default-directory ,dir))
       (ido-find-file))))

(aj/global-set-keys
 `(("C-c d" aj/duplicate-current-line-or-region)

   ;;; Quick access to some config files
   ;; ("<f9>" ,(aj/make-find-file-readonly "~/gluteus/information.org"))
   ;; ("<f10>" ,(aj/make-find-file "~/org/ideer.org"))
   ;; ("<f11>" ,(aj/make-find-file "~/.xmonad/xmonad.hs"))
   ;; ("<f12>" ,(aj/make-find-file "~/.emacs.d/init.el"))

   ("<f11>" aj/date-arvid)
   ("<f10>"
    ,(aj/make-find-file "~/Dropbox/Jobb/Nomadic_Labs/notes.org"))

   ("C-x nf" ,(aj/make-find-file-in-dir "~/.emacs.d/"))
   ("C-x nj" ,(aj/make-find-file-in-dir "~/"))

   ;; ("M-g" goto-line)
   ("M-#" replace-string)
   ("M-\"" align-regexp)

   ("M-a" aj/smart-beginning-of-line)
   ("M-e" move-end-of-line)

   ("M-p" aj/scroll-down-keep-cursor)
   ("M-n" aj/scroll-up-keep-cursor)

   ("C-å" join-line)
   ("C-\\" join-line)

   ;; Window handling
   ("C-ä" (lambda ()
      (interactive)
      (enlarge-window 3)))
   ("C-Ä" (lambda ()
      (interactive)
      (enlarge-window -3)))
   ("C-'" (lambda ()
      (interactive)
      (enlarge-window-horizontally 3)))
   ("C-*" (lambda ()
      (interactive)
      (enlarge-window-horizontally -3)))
   ("C-M-3" (lambda ()
      (interactive)
      (delete-other-windows)
      (split-window-horizontally)
      (split-window-horizontally)
      (balance-windows)))

   ;; TODO: in temporary windows (define windows?) bind alt-q to kill
   ;; buffer instead of just quitting. Also kill all dired some time
   ;; soon.

   ;; Files
   ("C-c C-r" revert-buffer)

   ("C-c s" ag)

   ;; Registers
   ;; Does not work, needs param or something
   ;; (global-set-key (kbd "C-x r i") #'(lambda () (interactive) (insert-register t)))
   ("C-x r a" append-to-register)

   ;; I never use this, but I should
   ;; ("M-DEL" report-intelligence-level)
   ("C-," backward-kill-word)

   ;; ("C-c C-d" dict-for-word-at-point)
   ("C-c C-d d" dict-for-word-at-point)
   ("C-c C-d w" fr-wordreference-word-at-point)
   ("C-c C-d i" fr-wiktionary-word-at-point)
   ("C-c C-d c" fr-wr-conjugate-word-at-point)
   ("C-c C-d l" fren-linguee-dwim)

   ;; The formatting of these guys suck
   ;; ("C-c C-d" insert-current-date-time)
   ;; ("C-c C-t" insert-current-time)

   ;; Renames current buffer and visiting file.
   ("C-c C-r" aj/rename-file-and-buffer)

   ;; Bindings for rejeep-comment
   ("C-7" comment-or-uncomment-current-line-or-region)

   ;; For in cli-mode
   ("C-c 7" comment-or-uncomment-current-line-or-region)

   ;; These keys are always a pain.
   ;; ("M-U" ,(make-inserter "["))
   ;; ("M-I" ,(make-inserter "]"))
   ("M-J" ,(aj/make-inserter "("))
   ("M-K" ,(aj/make-inserter ")"))
   ;; ("M-M" ,(make-inserter "{"))
   ;; ("M-;" ,(make-inserter "}"))

   ("M-D" delete-pair)

   ;; Calculator button I had on one keyboard.
   ("<XF86Calculator>" calc)
   ("<f5>" whitespace-mode)
   ("<f6>" linum-mode)

   ("M-Q" aj/unfill-paragraph)

   ;; Bindings for controlling text scale
   ("C-M-+" text-scale-increase)
   ("C-M--" text-scale-decrease)
   ("C-M-0" (lambda ()
      (interactive)
      (text-scale-increase 0)))

   ("M-j" backward-char)
   ("M-k" next-line)
   ("M-i" previous-line)
   ("M-l" forward-char)

   ;; Append / prepend to lines
   ("C-c n" aj/append-to-lines)
   ("C-c j" aj/prepend-to-lines)

   ;; What is current directory?
   ("C-c i" pwd)
   ("C-c f" aj/show-path)

   ("C-x B" ibuffer)

   ("C-c C-m" smerge-mode)

   ;; Start getting used to the xah-lee bindings
   ("M-j" backward-char)
   ("M-l" forward-char)
   ("M-i" previous-line)
   ("M-k" next-line)

   ("C-M-l" downcase-word)
   ("C-n" comment-indent-new-line)

   ;; ("§" inc-selective-display)
   ;; ("M-§" dec-selective-display)

   ("\C-xQ" aj/my-macro-query)

   ("M-o" aj/open-line-and-indent)
   ("C-S-k" kill-whole-line)

   ("C-x k" ido-kill-buffer)
   ("C-x C-k" ido-kill-buffer)

   ("C-x nh" aj/narrow-to-paragraph)
   ("C-x nc" aj/narrow-to-scope)

   ("M-s o" aj/occur)

   ("C-:" find-tag-other-window)


   ("C-q" fill-paragraph)
   ("M-C-q" aj/fill-paragraph-line)
   ("M-;" highlight-symbol-next)
   ("C-M-;" highlight-symbol-prev)

   ("M-g" keyboard-quit)
   ("C-e" goto-line)


   ("C-M-7" aj/dup-and-comment)

   ("C-x C-x" exchange-point-and-mark)

   ("M-ö" dabbrev-expand)
   ("M-ù" dabbrev-expand)
   ("C-ù" dabbrev-expand)

   ("C-c C-c" compile)

   ("C-x -" next-error)
   ("C-x M--" previous-error)

   ("C-x C-b" ido-switch-buffer)

   ("C-c C-/" aj/file-browser-here)
   ("C-c C-t" aj/terminal-here)

   ;; ("C-f" fr-wordreference-word-at-point)

   ("C-<return>" mc/edit-lines)
   ("C->" mc/mark-next-like-this)
   ("C-<" mc/mark-previous-like-this)
   ("C-c C-<" mc/mark-all-like-this)

   ;; phpunit
   ("C-c u r" phpunit-rerun)
   ("C-c u t" phpunit-current-test)
   ("C-c u c" phpunit-current-class)
   ("C-c u p" phpunit-current-project)


   ("C-c o" aj/swap-true-false)

   ("<C-M-mouse-4>" text-scale-increase)
   ("<C-M-mouse-5>" text-scale-decrease)

   ("<C-M-S-mouse-4>" default-text-scale-increase)
   ("<C-M-S-mouse-5>" default-text-scale-decrease)

   ("C-c C-w" elfeed)))

(defun aj/dup-and-comment ()
  ""
  (interactive)
  (aj/duplicate-current-line-or-region 1)
  (previous-line)
  (comment-or-uncomment-current-line-or-region)
  (next-line))

(aj/define-keys
 minibuffer-local-map `(("M-g" minibuffer-keyboard-quit)))

(aj/define-keys isearch-mode-map `(("M-g" isearch-abort)))


;; TODO function and key for reset-selective display
;; TODO use only one function for dec/set/reset
;; TODO some way of finding highest indentation level and using that as max.
(defun inc-selective-display ()
  ""
  (interactive)
  (setq selective-display
        (if selective-display
            (1+ selective-display)
          1)))

(defun dec-selective-display ()
  ""
  (interactive)
  (setq selective-display
        (if selective-display
            (1- selective-display)
          10)))

(provide 'arvid-keys)
