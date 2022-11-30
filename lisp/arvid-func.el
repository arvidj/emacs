;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

;; TODO: perhaps it is better to user basename of filename since this
;; does not handle uniquified buffernames very nicely.
(defun kill-ring-save-buffername ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new (buffer-name)))

;; TODO: perhaps it is better to user basename of filename since this
;; does not handle uniquified buffernames very nicely.
(defun kill-ring-save-pwd ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new default-directory))


;; TODO: perhaps it is better to user basename of filename since this
;; does not handle uniquified buffernames very nicely.
(defun kill-ring-save-path ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new (concat default-directory (buffer-name))))


(defun ninjaword-word-at-point ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (browse-url (concat
			   "http://ninjawords.com/?q="
			   (downcase (current-word t)))))

(defun fr-wiktionary-word-at-point ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (let ((coding-system-for-write 'utf-8) (coding-system-for-read 'utf-8))
	(browse-url (concat
				 "http://fr.wiktionary.org/wiki/"
				 (downcase (current-word t))))))

(defun fr-wordreference-word-at-point ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (let ((coding-system-for-write 'utf-8) (coding-system-for-read 'utf-8))
	(browse-url (concat
                 "http://www.wordreference.com/fren/"
                 (downcase (current-word t))))))

(defun fr-wr-conjugate-word-at-point ()
  "Conjugate word at point using WordReference."
  (interactive)
  (let ((coding-system-for-write 'utf-8) (coding-system-for-read 'utf-8))
    (dump-url (concat
                 "http://www.wordreference.com/conj/FrVerbs.aspx?v="
                 (downcase (current-word t))))))

(defun dump-url (url)
  (shell-command (concat "w3m " "'" url "'")))


(defun fren-linguee (word)
  "Looks up word in point using Ninjawords."
  (interactive "MLook up in Linguee (en-fr): ")
  (dump-url (concat "https://www.linguee.fr/francais-anglais/search?source=auto&query=" word)))

(defun fren-linguee-dwim ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (let ((word (cond
               (mark-active (buffer-substring-no-properties (region-beginning) (region-end)))
               ((current-word t))
               )))
    (message "Lookin up: %s" word)
    (if word (fren-linguee word) (fren-linguee))))


;; (defun fr-wordreference-word-at-point ()
;;   "Looks up word in point using Wordreference."
;;   (interactive)
;;   (let* ((word (asciify-string (downcase (current-word t)))))
;; 	(async-shell-command-to-string
;; 	 (concat "wr.sh " word)
;; 	 (lambda (s)
;; 	   (save-excursion
;; 		 (set-buffer (get-buffer-create "*wr*"))
;; 		 (erase-buffer)
;; 		 (insert s)
;; 		 (goto-char (point-min))
;; 		 (display-buffer "*wr*" t))))))

(defun google-word-at-point ()
  "Looks up word in point using Ninjawords."
  (interactive)
  (let ((coding-system-for-write 'utf-8) (coding-system-for-read 'utf-8))
	(browse-url (concat
                 "https://www.google.com/search?hl=en&q="
                 (downcase (current-word t))))))



(require 'cl)

(defun async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background.

Return the temporary output buffer which command is writing to
during execution.

When the command is finished, call CALLBACK with the resulting
output as a string."
  (lexical-let
      ((output-buffer (generate-new-buffer " *temp*"))
       (callback-fun callback))
    (set-process-sentinel
	 (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))


(defun asciify-string (string)
  "Convert STRING to ASCII string.
For example:
“passé” becomes “passe”"
  ;; Code originally by Teemu Likonen
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) "iconv" t t nil "--to-code=ISO_8859-1")
    (buffer-substring-no-properties (point-min) (point-max))))



(defun xah-asciify-string ()
	""
  ;; (interactive)
  (with-temp-buffer
    (insert string)
    (xah-asciify-text)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun xah-asciify-text (&optional @begin @end)
  "Remove accents in some letters and some
Change European language characters into equivalent ASCII ones, e.g. “café” ⇒ “cafe”.
When called interactively, work on current line or text selection.

URL `http://ergoemacs.org/emacs/emacs_zap_gremlins.html'
Version 2018-11-12"
  (interactive)
  (let (($charMap
         [
          ["ß" "ss"]
          ["á\\|à\\|â\\|ä\\|ā\\|ǎ\\|ã\\|å\\|ą\\|ă\\|ạ\\|ả\\|ả\\|ấ\\|ầ\\|ẩ\\|ẫ\\|ậ\\|ắ\\|ằ\\|ẳ\\|ặ" "a"]
          ["æ" "ae"]
          ["ç\\|č\\|ć" "c"]
          ["é\\|è\\|ê\\|ë\\|ē\\|ě\\|ę\\|ẹ\\|ẻ\\|ẽ\\|ế\\|ề\\|ể\\|ễ\\|ệ" "e"]
          ["í\\|ì\\|î\\|ï\\|ī\\|ǐ\\|ỉ\\|ị" "i"]
          ["ñ\\|ň\\|ń" "n"]
          ["ó\\|ò\\|ô\\|ö\\|õ\\|ǒ\\|ø\\|ō\\|ồ\\|ơ\\|ọ\\|ỏ\\|ố\\|ổ\\|ỗ\\|ộ\\|ớ\\|ờ\\|ở\\|ợ" "o"]
          ["ú\\|ù\\|û\\|ü\\|ū\\|ũ\\|ư\\|ụ\\|ủ\\|ứ\\|ừ\\|ử\\|ữ\\|ự"     "u"]
          ["ý\\|ÿ\\|ỳ\\|ỷ\\|ỹ"     "y"]
          ["þ" "th"]
          ["ď\\|ð\\|đ" "d"]
          ["ĩ" "i"]
          ["ľ\\|ĺ\\|ł" "l"]
          ["ř\\|ŕ" "r"]
          ["š\\|ś" "s"]
          ["ť" "t"]
          ["ž\\|ź\\|ż" "z"]
          [" " " "]       ; thin space etc
          ["–" "-"]       ; dash
          ["—\\|一" "--"] ; em dash etc
          ])
        $begin $end
        )
    (if (null @begin)
        (if (use-region-p)
            (setq $begin (region-beginning) $end (region-end))
          (setq $begin (line-beginning-position) $end (line-end-position)))
      (setq $begin @begin $end @end))
    (let ((case-fold-search t))
      (save-restriction
        (narrow-to-region $begin $end)
        (mapc
         (lambda ($pair)
           (goto-char (point-min))
           (while (search-forward-regexp (elt $pair 0) (point-max) t)
             (replace-match (elt $pair 1))))
         $charMap)))))


;; no longer working, the underlying service is not active
(defun quick-wikipedia-summary-for-word-at-point ()
  "Opens the a quick Wikipedia summary of the word at point in a new window."
  (interactive)
  ;; dig +short txt <keyword>.wp.dg.cx
  (let ((word (replace-regexp-in-string
			   "^[,\.\?[:space:]]*\\(.*?\\)[,\.\?[:space:]]*$"
			   "\\1"
			   (current-word t))))
	(shell-command (concat "dig +short txt " word ".wp.dg.cx"))))


(defun dict-for-word-at-point ()
  "Runs dict on the word at point in a new window."
  (interactive)
  (let* ((dbs '(("fr" . "fd-fra-eng")
                (nil . "\\*")))
         (lang (when (and (boundp 'guess-language-current-language)
                          guess-language-current-language)
                 (symbol-name guess-language-current-language)))
         (db (cdr (assoc lang dbs))))
    (dict-for-word (current-word t) db))
  )

(defun dict-for-word (word &optional db string)
    ""
  (interactive "MWord: ")
  (let* ((db (or db "\\*"))
         (cmd (concat "dict -d " db " " word)))
    (if string
        (shell-command-to-string cmd)
      (shell-command cmd)
      )))


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


;; Inserting file names
(defun arvid-tex-input ()
  (interactive)
  (let ((path (f-no-ext (read-file-name "File: ")))
		;; TODO Should only complete on dir but do not know how!
        (rel-path (if TeX-master (f-dirname TeX-master) ".")))
    (insert (concat "\\input{" (file-relative-name path rel-path) "}"))))


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

(defun arvid-xdebug-break-occur () 
  ""
  (interactive)
  (multi-occur-in-matching-buffers ".*\\.php$" "xdebug_break"))


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


(defun get-u500-ticket-nr-from-branch ()
  ""
  (interactive)
  (let* ((branch (magit-get-current-branch))
	 (matches (string-match "\\(atvcm.*?\\)_" branch))
		 (tnr (match-string 1 branch)))
     tnr))

(defun get-ticket-nr-from-branch ()
  ""
  (interactive)
  (let* ((branch (magit-get-current-branch))
		 (matches (string-match "^t\\(.*?\\)-" branch))
		 (tnr (match-string 1 branch)))
	(insert (concat (get-current-project-code) "-" tnr " "))))

(defun get-current-project-code ()
  ""
  (interactive)
  (let ((dir default-directory))
	(cond
	 ((string-match "public_html/weback" dir) "WEBACK")
	 ((string-match "public_html/wwoof_community" dir) "WWOOF")
	 ((or (string-match "typo3" dir)
		  (string-match ".*core-magenta-2.0-git.*" dir)
		  (string-match ".*core-.*" dir)
		  (string-match "core-sitespace-4.5" dir)) "MAGENTA"))))

(defun arvid-cua-exchange-point-and-mark (arg)
  "Exchanges point and mark, but don't activate the mark.
Activates the mark unless a prefix argument is given."
  (interactive "P")
  (when (not arg) (setq mark-active t))
  (let (mark-active)
	(exchange-point-and-mark)

	;; (if cua--rectangle
	;; 	(cua--rectangle-corner 0))

))

(defun arvid-show-file ()
  ""
  (interactive)
  (let ((file (buffer-file-name)))
	(kill-new file)
	(message "File: %s" file)))

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

(define-minor-mode longlines-mode
  "replacement for deprecated longlines-mode"
  :lighter " ll"
  (if longlines-mode
      (progn
        (visual-line-mode t)
        (set-visual-wrap-column 80))
    (visual-line-mode nil)
    (set-visual-wrap-column 0)))


(define-minor-mode frdict-mode
  "eldoc mode for french dictionary"
  :lighter " frd"
  (if frdict-mode
      (set (make-local-variable 'eldoc-documentation-function)
           'frdict-eldoc-function)
    (when (and (local-variable-p eldoc-documentation-function)
               (eq eldoc-documentation-function frdict-eldoc-function))
      (setq frdict-eldoc-function nil))))

(defun frdict-eldoc-function ()
  ""
  (interactive)
  (when (current-word t)
    (replace-in-string "\n" " // " (trim-string (shell-command-to-string
                                                 (concat "dict -d fd-fra-eng " (shell-quote-argument (current-word t)) " | grep '/'"))))))




(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun create-change-log ()
  ""
  (interactive)
  (save-excursion (replace-string "Task" "<span class=\"label label-info\">Task</span>" nil (region-beginning) (region-end)))
  (save-excursion (replace-string "Bug" "<span class=\"label label-warning\">Bug</span>"	nil (region-beginning) (region-end)))
  (save-excursion (replace-string "New Feature" "<span class=\"label label-success\">New Feature</span>" nil (region-beginning) (region-end)))
  (save-excursion (replace-string "Improvement" "<span class=\"label label-success\">Improvement</span>" nil (region-beginning) (region-end)))
  (save-excursion (replace-regexp "[[:space:]]+BLOOM-[[:digit:]]+[[:space:]]+" " "  nil (region-beginning) (region-end)))
  (save-excursion (prepend-to-lines 1 "<li>"))
  (save-excursion (append-to-lines 1 "</li>")))

;; In response to http://stackoverflow.com/a/31966206/87129
;; (defun read-file-name-nodots (prompt &optional dir default-filename mustmatch initial predicate)
;;   "Like read-file-name-default but does not complete on ./ and ../"
;;   (let ((predicate (or predicate (lambda (f) (not (or (string= f "./") (string= f "../")))))))
;; 	(read-file-name-default prompt dir default-filename mustmatch initial predicate)))
;; (setq read-file-name-function 'read-file-name-nodots)


;; (defadvice file-name-all-completions (around expand activate)
;;   "Testing"
;;   ;; (message "test")
;;   (let ((files ())) ad-do-it
;; 	   ;; (message files)
;; 	   (cons "foobar" files)
;; 	   '("foo" "bar" "baz")
;; 	   ))

;; TODO: handle case with replace-match
(defun arvid-swap-true-false ()
	""
  (interactive)
  (save-excursion
	(er/mark-symbol)

	(let* ((beg (region-beginning))
		   (end (region-end))
		   (selection (buffer-substring-no-properties beg end)))
	  (if (string= selection "true")
		  (progn
			(delete-region beg end)
			(insert "false")))
	  (if (string= selection "false")
		  (progn
			(delete-region beg end)
            (insert "true")))
      (if (string= selection "False")
		  (progn
			(delete-region beg end)
            (insert "True")))
      (if (string= selection "True")
		  (progn
			(delete-region beg end)
            (insert "False")))
)))

(defun arvid-fill-paragraph-line ()
    ""
  (interactive)
  (save-excursion
    (setq bol (line-beginning-position))
    (end-of-line)
    (setq eol (point))
    (fill-region bol eol)))


(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun profiled-save-buffer ()
    ""
  (interactive)
  ;; (profiler-start 'cpu)
  ;; (shell-command (concat "/c/Program\\ Files\\ \\(x86\\)/handle.exe " (buffer-name)) "*save-info-before*")
  (measure-time
   (save-buffer)
  ;; (profiler-report)
  ;; (profiler-stop)
   )
  ;; (shell-command (concat "/c/Program\\ Files\\ \\(x86\\)/handle.exe " (buffer-name)) "*save-info-after*")


  ;; (select-window (previous-window))
)

(global-set-key (kbd "C-x C-s") 'profiled-save-buffer)

(defun open-file ()
  (interactive)
  (let ((path (read-file-name "File: ")))
	;; TODO Should only complete on dir but do not know how!
    (shell-command
     (concat (if (eq system-type 'gnu/linux)
                 "xdg-open "
               "start ") path))))

;; (insert (file-relative-name path rel-path)))

(defun file-browser-here ()
  (interactive)
  (shell-command
   (if (eq system-type 'gnu/linux)
       "xdg-open ."
     "start .")))


(defun terminal-here ()
    ""
  (interactive)
  (shell-command
   (if (eq system-type 'gnu/linux)
       (concat "xfce4-terminal --working-directory=\"" default-directory "\" &")
     "")))

(defun rotate-imagefile-at-point-left ()
  "Calls mogrify -rotate -90 at on the filname at point"
  (interactive)
  (shell-command (concat "mogrify -rotate -90 " (thing-at-point 'filename))))

(defun org-to-latex-error ()
  ""
  (interactive)
  (find-file (f-swap-ext (buffer-file-name) "tex"))
  (read-only-mode t)

  (TeX-next-error))

(defun org-screenshot-at-point ()
  "Interactively pick a region to screen shot,
    save it to the current folder and insert an org-link to it"
  (interactive)
  (let ((file-name (trim-string (shell-command-to-string "tempfile -d . -p 'ss-' -s '.png'"))))
    (shell-command (concat "import " file-name))
    (insert (concat "[["  file-name "]]"))))

(defun org-google-scholar-heading ()
  ""
  (interactive)
  (let ((heading (nth 4 (ignore-errors (org-heading-components)))))
    (browse-url (concat "https://scholar.google.com/scholar?hl=en&q=" heading))))

(defun date-arvid ()
  "Runs the `date` command"
  (interactive)
  (shell-command "date"))


(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun arvid-org-toggle-noexport-tag ()
  ""
  (interactive)
  (let ((tags (org-get-tags)))
    (if (member "noexport" tags)
        (org-set-tags-to (delete "noexport" tags))
      (org-set-tags-to (cons "noexport" (if (string= (car tags) "") nil tags))))))

(defun insert-tezos-unicode-symbol ()
	""
  (interactive)
  (insert "ꜩ"))

(defun yank-replace (from to)
  (interactive
   (if (>= (length query-replace-history) 2)
       (cons (nth 1 query-replace-history) '(nil))
     '(nil nil)))
  (yank)
  (exchange-point-and-mark)
  (if (not from)
      (call-interactively 'replace-string)
    (let ((to (read-string (format "Replace %s with: " from))))
      (replace-string from to nil (region-beginning) (region-end)))))

(defun highlight-all-logs ()
  ""
  (interactive)
  (hi-lock-mode t)
  (save-excursion
    (let ((end-of-window (progn (move-to-window-line -1) (end-of-line) (point)))
          (hi-lock-auto-select-face t))
      (move-to-window-line 0)
      (while (and (re-search-forward "^.* - \\(.*?:\\) .*$")
                  (< (point) end-of-window))
        (let ((color (let ((hi-lock-auto-select-face t))
                       (hi-lock-read-face-name)))
              (color_alt (subseq (secure-hash 'md5 (match-string 1)) 0 6)))
          (highlight-regexp (regexp-quote (match-string 1)) color)
                  )
                  )
        ;; (message (match-string 1))
        )))

(defun howdoi (query &optional n)
	""
  (interactive "MQuery: \np")
  (let ((buf (format "*howdoi: %s" query)))
    (shell-command (format "howdoi -n %d %s &" n (shell-quote-argument query))
                   buf)
    (display-buffer buf)
    )
  )

(provide 'arvid-func)
;;; arvid-func.el ends here

