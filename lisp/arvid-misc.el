;;; Check out these modes for handling whitespace in a more ordered fashion.
;; http://github.com/glasserc/ethan-wspace
;; http://gist.github.com/452824

(setq require-final-newline t)
(delete-selection-mode t)

;; I want to be able to just discard / revert buffers that I have left
;; changes in.
(add-to-list
 'save-some-buffers-action-alist
 '(?k kill-buffer "discard this buffer"))
(add-to-list
 'save-some-buffers-action-alist
 '(?r
   (lambda (buf)
     (save-current-buffer
       (set-buffer buf)
       (revert-buffer)))
   "revert this buffer"))

(use-package saveplace :ensure t :config (save-place-mode))

;; Indent after yank
;; http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member
            major-mode
            '(emacs-lisp-mode
              lisp-mode
              js-mode
              clojure-mode
              scheme-mode
              plain-tex-mode
              ruby-mode
              rspec-mode
              python-mode
              c-mode
              c++-mode
              objc-mode
              latex-mode
              php-mode
              ada-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))


;; Stops the point from entering the mini-buffer prompt
(setq minibuffer-prompt-properties
      (plist-put
       minibuffer-prompt-properties
       'point-entered
       'minibuffer-avoid-prompt))

(setq-default abbrev-mode t)
;; On new machines, call [write-abbrev-file] if  [~/.emacs.d/misc/abbrev_defs] does not exist:
;;   (write-abbrev-file "~/.emacs.d/misc/abbrev_defs")
(read-abbrev-file "~/.emacs.d/misc/abbrev_defs")
(setq save-abbrevs t)

;; aj/yank-or-pop deletes selection if there is one.
(put 'aj/yank-or-pop 'delete-selection 'yank)

(defvar visual-wrap-column nil)
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list
                (read-number "New visual wrap column, 0 to disable: "
                             (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column) (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook
     'window-configuration-change-hook 'update-visual-wrap-column
     nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available
                               visual-wrap-column))))))


(setq set-mark-command-repeat-pop t)

;; indentation
(setq indent-tabs-mode nil)

(use-package visual-regexp :ensure t)

(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))

(setenv "PATH" (concat (getenv "PATH") ":/home/arvid/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/home/arvid/.cabal/bin"))
(setenv "PATH"
        (concat
         (getenv "PATH")
         ":/home/arvid/.nvm/versions/node/v18.18.2/bin"))
(setq exec-path
      (append
       exec-path
       '("/home/arvid/bin"
         ;; This is required to make yaml-language-server,
         ;; installed through 'npm -g' visible to lsp-mode.
         "/home/arvid/.nvm/versions/node/v18.18.2/bin")))

(provide 'arvid-misc)
