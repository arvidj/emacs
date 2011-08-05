;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

;; TODO: perhaps it is better to user basename of filename since this
;; does not handle uniquified buffernames very nicely.
(defun kill-ring-save-buffername ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new (buffer-name)))

(defun ninjaword-word-at-point ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (browse-url (concat
			   "http://ninjawords.com/?q="
			   (downcase (current-word t)))))

(defun quick-wikipedia-summary-for-word-at-point ()
  "Opens the a quick Wikipedia summary of the word at point in a new window."
  (interactive)
  ;; dig +short txt <keyword>.wp.dg.cx
  (let ((word (replace-regexp-in-string
			   "^[,\.\?[:space:]]*\\(.*?\\)[,\.\?[:space:]]*$"
			   "\\1"
			   (current-word t))))
	(shell-command (concat "dig +short txt " word ".wp.dg.cx"))))

;; Prepending / appending to lines
(defun prepend-to-lines (arg prefix)
"Prepend prefix to all lines in region. If a negative arg is
given, prefix is placed after whitespace in beginning of line."
  (interactive  "p\nMPrepend: ")
  (if (< arg 0)
	  (to-line-helper "^\\([[:space:]]*\\)" (concat "\\1" prefix))
	(to-line-helper "^" prefix)))

(defun append-to-lines (arg suffix)
  "Append suffix to all lines in region. Suffix is placed before
whitespace at end of line, unless negative arg is given."
  (interactive  "p\nMAppend: ")
  (if (< arg 0)
	  (to-line-helper "$" suffix)
	(to-line-helper "[[:space:]]*$" suffix)))

;; Replace what with with in n
(defun to-line-helper (pattern replacement)
  (if mark-active
	  (save-excursion
		(let ((reg-beg (region-beginning))
			  (reg-end (region-end)))
		  (replace-regexp
		   pattern replacement nil
		   (progn (goto-char reg-beg) (line-beginning-position))
		   (progn (goto-char reg-end) (line-end-position)))))
    (message "Mark not active")))

(defun wrap-lines (arg how)
  (interactive "p\nMWrap: ")
  (let ((re-replacement (replace-regexp-in-string "|" "\\\\1" (regexp-quote how))))
	(message (regexp-quote how))
	(message re-replacement)
	(if (< arg 0)
	  (to-line-helper  re-replacement)
	(to-line-helper "^[[:space:]]*\\(.*?\\)[[:space:]]*$" re-replacement))))

(defun nuke-all-buffers (arg)
  "Kill all buffers except current, leaving *scratch* only. If
arg is negative, also kill current."
  (interactive "p")
  (mapcar (lambda (x)
			(when (or (eq arg -1) (not (eq x (current-buffer))))
			  (kill-buffer x)))
		  (buffer-list))
  (delete-other-windows))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ())) ad-do-it))

;; TODOOIt would be nice if it would notice that last command is
;; zap-to-char and that we want to zap again.
(defun zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."


  ;; TODO: if (eq last-command 'zap-to-char)) ~ use last char ~


  ;; Does not work, says that last-command is kill-region, quite
  ;; correct.
  (interactive (list current-prefix-arg
					 (if (eq last-command 'zap-to-char)
						 arvid-zap-last-char
					  (message (symbol-name last-command))
					  (read-char "Zap to char: "))))

  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (setq arvid-zap-last-char char)
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
	(setq char (or (aref translation-table-for-input char) char))))
  ;; If point is already at char, remove char, otherwise, remove up
  ;; until next char, excluding char.
  (kill-region (point)
			   (progn
				 (if (= (char-after) char)
					 (1+ (point))
				   (search-forward (char-to-string char) nil nil arg)
				   (backward-char)
				   (point)))))

;; Inserting file names
(defun insert-relative-path ()
  (interactive)
  (let ((path (read-file-name "File: "))
		;; TODO Should only complete on dir but do not know how!
		(rel-path (if (not current-prefix-arg)
					  (read-file-name "Relative dir: ")
					default-directory)))
	(insert (file-relative-name path rel-path))))

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

;; Movement

;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or goal-column, or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
	(if goal-column
        (move-to-column goal-column)
	  (back-to-indentation))
	(and (= oldpos (point))
		 (beginning-of-line))))


;; http://geosoft.no/development/emacs.html
(defun scroll-down-keep-cursor ()
   "Scroll the text one line down while keeping the cursor."
   (interactive)
   (scroll-down 4))

(defun scroll-up-keep-cursor ()
   "Scroll the text one line up while keeping the cursor."
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

;; Various

;; TODO only make comment if not already in comment. see fontlock or somtf. font-lock-comment-face
(defun arvid-add ()
  "Insert a comment with my name and date, for creating a comment about modifications"
  (interactive)
  (insert "Arvid add ")
  (insert-current-date)
  (insert ": ")
  (comment-region (line-beginning-position) (line-end-position)))

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

;; Various defuns for setting keybinding
(defun report-intelligence-level ()
  "Reports the current users level of intelligence in an user friendly manner."
  (interactive)
  (message "Idiot!"))

(defun make-inserter (str)
  `(lambda () (interactive) (insert ,str)))

(defun global-set-keys (bindings)
  "Globally set all BINDINGS."
  (dolist (binding bindings)
    (let ((key (car binding)) (command (cadr binding)))
      (global-set-key (read-kbd-macro key) command))))

(defun global-unset-keys (keys)
  "Globally unset all BINDINGS."
  (dolist (key keys)
    (global-unset-key (read-kbd-macro key))))

(defun define-keys (map bindings)
  (dolist (binding bindings)
    (let ((key (car binding)) (command (cadr binding)))
	  (define-key map (read-kbd-macro key) command))))

;; From http://www.emacswiki.org/emacs/KeyboardMacros#toc5
(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to
use. If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
		 (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
				  (read-from-minibuffer prompt))))
	(unless (string= "" input) (insert input))))

(defun open-line-and-indent ()
  "Splits the current line using open-line, then indents."
  (interactive)
  (save-excursion
	(open-line 1)
	(next-line)
	(indent-according-to-mode)))

;; TODO make more nice
(defun toggle-booleans-in-region-or-line ()
  "Replaces each true with false. If no true is found, do the reverse."
  (interactive)
  (save-excursion
	(let ((reg-beg (region-beginning))
		  (reg-end (region-end))
		  (found nil))
	  (goto-char reg-beg)
	  (while (re-search-forward "true" reg-end t)
		(setq found t)
		(replace-match "false" nil nil)
		(setq reg-beg (1+ reg-beg)))
	  (unless found
		(message "trying other way")
		(goto-char reg-beg)
		(while (re-search-forward "false" reg-end t)
		  (replace-match "true" nil nil)
		  (setq reg-beg (1- reg-beg)))))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (let ((p (point)))
	(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
	(goto-char p)))

(defun narrow-to-paragraph ()
  (interactive)
  "Narrow to paragraph (or its visible portion)."
  (narrow-to-region (save-excursion (backward-sentence) (point))
					(save-excursion (forward-paragraph) (point))))

(defun buffer-char-frequency ()
  (interactive)
  (save-excursion
	(goto-char (point-min))
	(let ((char-freq-table (make-hash-table :test 'eql :size 256))
		  (current-char (following-char)))
	  (while (> current-char 0)
		  (puthash current-char
				   (1+ (gethash current-char char-freq-table 0))
				   char-freq-table)
		  (forward-char)
		  (setq current-char (following-char)))

	  ;; Hash is ready. Format each value, and sort by frequency
	  ;; descending.
	  (let ((frequency-list (hash-to-list char-freq-table)))
		;; Sort by frequency
		(setq frequency-list
			  (sort frequency-list
					(lambda (a b) (> (cadr a) (cadr b)))))

		;; Format
		(print (mapcar (lambda (el)
						 ;; (print el)
						 (let ((char (car el)))
						   `(,(if enable-multibyte-characters
								  (if (< char 128)
									  (single-key-description char)
									(buffer-substring-no-properties (point) (1+ (point))))
								(single-key-description char))
							 ,(cadr el))))
					   frequency-list))))))

(defun hash-to-list (hashtable)
  "Return a list that represent the HASHTABLE."
  (let (mylist)
	(maphash (lambda (kk vv) (setq mylist (cons (list kk vv) mylist))) hashtable)
	mylist))

(defun spawn-shell ()
  (interactive)
  (start-process
   "spawn-shell-proc"
   nil
   "urxvt" "-bg" "black" "-fg" "white" "-cd" default-directory))

(defun arvid-occur (regexp &optional nlines)
  (interactive (arvid-occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

(defun arvid-occur-read-primary-args ()
  (list (read-regexp "List lines matching regexp"
		     (arvid-occur-get-default-regexp))
	(when current-prefix-arg
	  (prefix-numeric-value current-prefix-arg))))

(defun arvid-occur-get-default-regexp ()
  (cond
   ((region-active-p) (buffer-substring-no-properties (region-beginning) (region-end)))
   ((symbol-name (symbol-at-point)))
   (t (car regexp-history))))


(defun arvid-query-replace-html-entitities ()
  (interactive)
  (let ((old-case case-fold-search))
	(setq case-fold-search nil)
	(query-replace "ä" "&auml;")
	(beginning-of-buffer)
	(query-replace "å" "&aring;")
	(beginning-of-buffer)
	(query-replace "ö" "&ouml;")
	(beginning-of-buffer)
	(query-replace "Ä" "&Auml;")
	(beginning-of-buffer)
	(query-replace "Å" "&Auml;")
	(beginning-of-buffer)
	(query-replace "Ö" "&Ouml;")
	(beginning-of-buffer)
	(setq case-fold-search old-case)))

(defun get-ticket-nr-from-branch () 
  ""
  (interactive)
  (let* ((branch (magit-get-current-branch))
		 (matches (string-match "^t\\(.*?\\)-" branch))
		 (tnr (match-string 1 branch)))
	(insert (concat (get-current-project-code) "-" tnr))))

(defun get-current-project-code ()
  ""
  (interactive)
  (let ((dir default-directory))
	(cond
	 ((string-match "public_html/weback" dir) "WEBACK")
	 ((or (string-match "typo3" dir)
		  (string-match ".*core-magenta-2.0-git.*" dir)) "MAGENTA"))))
