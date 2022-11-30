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

(defun arvid-kill-occurrence-at-point ()
  "Kills the occurence at point"
  (interactive)
  ;; Store the point here. It seems like save-excursion does not work
  ;; across a revert.
  (let ((current-point (point)))
	(save-window-excursion
	  (occur-mode-goto-occurrence)
	  (kill-whole-line))
	(revert-buffer)
	;; current-point could now be larger than the buffer, but
	;; goto-char doesnt care. It
	(goto-char current-point)))
(define-key occur-mode-map (kbd "C-k") 'arvid-kill-occurrence-at-point)

(setq delete-selection-mode t)

;; (setq-default save-place t)
(require 'saveplace)
(save-place-mode)

(add-hook 'calc-mode-hook #'(lambda () (setq autopair-dont-activate t)))

(add-to-list 'file-coding-system-alist
			 '("user-extensions\\.js\\'" . iso-latin-8859))

(setq dired-listing-switches "-alh")

;; Which-func-mode
;; (require 'which-func)
;; (add-to-list 'which-func-modes 'php-mode)
;; (which-func-mode t)


;; Indent after yank
;; http://www.emacswiki.org/emacs/AutoIndentation
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode       js-mode
						     clojure-mode    scheme-mode
						     plain-tex-mode  ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
                             php-mode ada-mode
						     ))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))


(defun capitalize-first (string)
  ""
  (if (string= string "") ""
	(concat (capitalize (substring string 0 1)) (substring string 1))))

;; Not sure what this is for
(setq minibuffer-prompt-properties
	  (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/misc/abbrev_defs")
(setq save-abbrevs t)

(defun arvid-yank-or-pop ()
  "Yanks the first time called, pops otherwise."
  (interactive)
  (if (eq last-command 'yank)
	  (yank-pop)
	(yank)))

(defun arvid-yank-pop-forwards (arg)
  "Pop forward in kill-ring."
  (interactive "p")
  (yank-pop (- arg)))

(global-set-key (kbd "M-y") 'arvid-yank-pop-forwards)
(global-set-key (kbd "C-y") 'arvid-yank-or-pop)

;; arvid-yank-or-pop deletes selection if there is one.
(put 'arvid-yank-or-pop 'delete-selection 'yank)

;; Creates a new empty buffer
(defun arvid-new-note (name)
  "Opens a new empty buffer."
  (interactive "MName: ")
  (let* ((buf-name (concat
					"note"
					(if (string= name "") "" (concat "-" name))
					(concat "-" (format-time-string "%Y-%m-%d_%H:%M"))
					))
		 (path (concat "~/notes/" buf-name)))
	(find-file path)))
(global-set-key (kbd "C-c b") 'arvid-new-note)

(defun arvid-nxml-mode-hook ()
  (setq comment-continue ""))
(add-hook 'nxml-mode-hook 'arvid-nxml-mode-hook)

(add-hook 'c++-mode-hook 'arvid-c++-mode-hook)
(defun arvid-c++-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'compile)
  (subword-mode))

(add-hook 'java-mode-hook 'arvid-java-mode-hook)
(defun arvid-java-mode-hook ()
  (local-set-key (kbd "C-c C-c") 'compile)
  (subword-mode))

(setq visible-bell t)

(defvar visual-wrap-column nil)
(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
		   (zerop new-wrap-column))
	  (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
	(visual-line-mode t)
	(set (make-local-variable 'visual-wrap-column) new-wrap-column)
	(add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
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
							(- current-available visual-wrap-column))))))


(defun revert-files (&rest files)
  "Reload all specified files from disk.
Only files that are currently visited in some buffer are reverted.
Do not ask confirmation unless the buffer is modified."
  (save-excursion
    (let ((revert-without-query '("")))
      (dolist (file-name files)
        (message "Considering whether to revert file %s" file-name)
        (let ((buf (find-buffer-visiting file-name)))
          (when buf
            (message "Reverting file in buffer %s" (buffer-name buf))
            (set-buffer buf)
			(revert-buffer t nil t)))))))

;; (defun arvid-quick-calc () 
;;   ""
;;   (interactive "r")
;;   calc-do-quick-calc
;;   )

;; Convert comma separated lines to org-table
(fset 'convert-csv-org-table
   [?\M-< ?\M-# ?, return ?| return ?\M-< ?\C-  ?\M-> ?\M-i C-return ?| ?\C-g tab])

;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold 80)
(setq split-width-threshold 160)

;; Makes last error visited in the compilation log more visible
(defface right-triangle-face
  '((t (:background "red" :foreground "yellow")))
  "Face for `right-triangle-face`.")
(set-fringe-bitmap-face 'right-triangle 'right-triangle-face)

(setq set-mark-command-repeat-pop t)

;; indentation
(setq indent-tabs-mode nil)

(defun note-event (event file)
  ""
  (interactive)


  (let ((date (format-time-string "%FT%H:%M:%S%z"))
        (line (concat date "\t" event "\n")))
    (write-region line nil file 'append)))

(defun note-noise ()
    ""
  (interactive)
  (note-event "Noise" "~/noise.csv"))

(defun registers-list-symbols ()
  ""
  (interactive)
  (let ((compilation-buffer-name-function
         '(lambda (mode) "*Registration Formalization Symbols*")))
    (ack "ag --tex newcommand ../report ../ott ../analysis")
    ))

(global-set-key (kbd "<f9>") 'note-noise)

;; Turn off the pomidor sound
(setq pomidor-play-sound-file nil)


(use-package visual-regexp :ensure t)

(defun chords-mode ()
  ""
  (interactive)
  (follow-mode)
  (split-window-right)
  (balance-windows)
  )

(add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))

(provide 'arvid-misc)
