;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun prepend-to-lines (with) 
  (interactive  "MPrepend: ")
  (to-line-helper "^" with))

(defun append-to-lines (with) 
  (interactive  "MAppend: ")
  (to-line-helper "$" with))

(defun to-line-helper (what with) 
  (if mark-active
      (replace-regexp what with nil (region-beginning) (region-end))
    (message "Mark not active")))

(defun nuke-all-buffers () 
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list)) 
  (delete-other-windows))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;; Inserting file names 
(defun insert-relative-path (path) 
  (interactive "F")
  (insert (file-relative-name path)))

;; Never understood why Emacs doesn't have this function.
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer '%s' is not visiting a file!" name)
      (ido-nowhere
       (let ((new-name (read-file-name "New name: " filename)))
         (cond ((get-buffer new-name)
                (message "A buffer named '%s' already exists!" new-name))
               (t
                (rename-file filename new-name 1)
                (rename-buffer new-name)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil))))))))

(defmacro ido-nowhere (&rest body)
  `(progn
     (ido-everywhere -1)
     ,@body
     (ido-everywhere 1)))

;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


;; http://geosoft.no/development/emacs.html
(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 4))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 4)) 




;;;;;;;;;;;;;;;;;;;;;;;
;; Insert date and time

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert "==========\n")
;       (insert (let () (comment-start)))
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n"))

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n"))


;; Todo: automatically make a comment
(defun arvid-add ()
  "Insert a comment with my name and date, for creating a comment about modifications"
  (interactive)
  (insert "Arvid add ")
  (insert-current-date)
  (insert ":\n"))

(defun insert-current-date () 
  "Insert the current date into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

;; From rejeep
;; http://github.com/rejeep/emacs
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and (region-active-p) (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if (region-active-p)
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Word count
(defun word-count nil "Count words in buffer" (interactive)
  (shell-command-on-region (region-beginning) (region-end) "wc -w"))

;;;;;;;;;;;;;;;;;;;;;;;
;; Window handling
(defun kill-other-window-2 
  (&optional n) 
  "Kill buffer in other window"
  (interactive "p")

  (other-window n)
  (kill-buffer)
  (other-window (- 0 n)))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;;;;;;;;;;;;;;;;;;;;
;; Shell

(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (setq comint-input-sender 'n-shell-simple-send))

(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer))
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))))
