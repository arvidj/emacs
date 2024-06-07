;; Custom library functions

(defun aj/global-set-keys (bindings)
  "Globally set all BINDINGS."
  (dolist (binding bindings)
    (let ((key (car binding))
          (command (cadr binding)))
      (global-set-key (read-kbd-macro key) command))))

(defun aj/global-unset-keys (keys)
  "Globally unset all BINDINGS."
  (dolist (key keys)
    (global-unset-key (read-kbd-macro key))))

(defun aj/define-keys (map bindings)
  (dolist (binding bindings)
    (let ((key (car binding))
          (command (cadr binding)))
      (define-key map (read-kbd-macro key) command))))

(defun aj/yank-or-pop ()
  "Yanks the first time called, pops otherwise."
  (interactive)
  (if (eq last-command 'yank)
      (yank-pop)
    (yank)))

(defun aj/yank-pop-forwards (arg)
  "Pop forward in kill-ring."
  (interactive "p")
  (yank-pop (- arg)))

;; From https://emacs.stackexchange.com/questions/19461/insert-lines-when-yanking-rectangle-rather-than-inserting-among-following-lines
(defun aj/insert-rectangle-push-lines ()
  "Yank a rectangle as if it was an ordinary kill."
  (interactive "*")
  (when (and (use-region-p) (delete-selection-mode))
    (delete-region (region-beginning) (region-end)))
  (save-restriction
    (narrow-to-region (point) (mark))
    (yank-rectangle)))

;; TODO: perhaps it is better to user basename of filename since this
;; does not handle uniquified buffernames very nicely.
(defun aj/copy-buffername ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new (buffer-name)))

(defun aj/copy-pwd ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new default-directory))

(defun aj/copy-path ()
  "Saves the current buffer-name in kill-ring."
  (interactive)
  (kill-new (concat default-directory (buffer-name))))

(defun aj/show-path ()
  ""
  (interactive)
  (let ((file (buffer-file-name)))
    (kill-new file)
    (message "File: %s" file)))

;; Prepending / appending to lines
(defun aj/prepend-to-lines (arg prefix)
  "Prepend prefix to all lines in region. If a negative arg is
given, prefix is placed after whitespace in beginning of line."
  (interactive "p\nMPrepend: ")
  (if (< arg 0)
      (aj/to-line-helper "^\\([[:space:]]*\\)" (concat "\\1" prefix))
    (aj/to-line-helper "^" prefix)))

(defun aj/append-to-lines (arg suffix)
  "Append suffix to all lines in region. Suffix is placed before
whitespace at end of line, unless negative arg is given."
  (interactive "p\nMAppend: ")
  (if (< arg 0)
      (aj/to-line-helper "$" suffix)
    (aj/to-line-helper "[[:space:]]*$" suffix)))

;; Replace what with with in n
(defun aj/to-line-helper (pattern replacement)
  (if mark-active
      (save-excursion
        (let ((reg-beg (region-beginning))
              (reg-end (region-end)))
          (replace-regexp pattern replacement
                          nil
                          (progn
                            (goto-char reg-beg)
                            (line-beginning-position))
                          (progn
                            (goto-char reg-end)
                            (line-end-position)))))
    (message "Mark not active")))

(defun aj/wrap-lines (arg how)
  (interactive "p\nMWrap: ")
  (let ((re-replacement
         (replace-regexp-in-string "|" "\\\\1" (regexp-quote how))))
    (message (regexp-quote how))
    (message re-replacement)
    (if (< arg 0)
        (aj/to-line-helper re-replacement)
      (aj/to-line-helper
       "^[[:space:]]*\\(.*?\\)[[:space:]]*$" re-replacement))))

(defun aj/nuke-all-buffers (arg)
  "Kill all buffers except current, leaving *scratch* only. If
arg is negative, also kill current."
  (interactive "p")
  (mapcar
   (lambda (x)
     (when (or (eq arg -1) (not (eq x (current-buffer))))
       (kill-buffer x)))
   (buffer-list))
  (delete-other-windows))

(defadvice save-buffers-kill-emacs
    (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (let ((process-list ()))
    ad-do-it))

;; TODO: It would be nice if it would notice that last command is
;; zap-to-char and that we want to zap again.
(defun aj/zap-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."


  ;; TODO: if (eq last-command 'zap-to-char)) ~ use last char ~


  ;; Does not work, says that last-command is kill-region, quite
  ;; correct.
  (interactive (list
                current-prefix-arg
                (if (eq last-command 'zap-to-char)
                    aj/zap-last-char
                  (message (symbol-name last-command))
                  (read-char "Zap to char: "))))

  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (setq aj/zap-last-char char)
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char
              (or (aref translation-table-for-input char) char))))
  ;; If point is already at char, remove char, otherwise, remove up
  ;; until next char, excluding char.
  (kill-region
   (point)
   (progn
     (if (= (char-after) char)
         (1+ (point))
       (search-forward (char-to-string char) nil nil arg)
       (backward-char)
       (point)))))

;; Inserting file names
(defun aj/insert-relative-path ()
  (interactive)
  (let ((path (read-file-name "File: "))
        ;; TODO Should only complete on dir but do not know how!
        (rel-path
         (if (not current-prefix-arg)
             (read-file-name "Relative dir: ")
           default-directory)))
    (insert (file-relative-name path rel-path))))

;; Never understood why Emacs doesn't have this function.
;; http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun aj/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (let ((new-name (read-file-name "New name: " filename)))
      (cond
       ((get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name))
       (t
        (when (and filename (file-exists-p filename))
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil))))))

;; Movement

;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun aj/smart-beginning-of-line ()
  "Move point to first non-whitespace character or goal-column, or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (if goal-column
        (move-to-column goal-column)
      (back-to-indentation))
    (and (= oldpos (point)) (beginning-of-line))))


;; http://geosoft.no/development/emacs.html
(defun aj/scroll-down-keep-cursor ()
  "Scroll the text one line down while keeping the cursor."
  (interactive)
  (scroll-down 4))

(defun aj/scroll-up-keep-cursor ()
  "Scroll the text one line up while keeping the cursor."
  (interactive)
  (scroll-up 4))

;;;;;;;;;;;;;;;;;;;;;;;
;; Insert date and time

(defvar aj/current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `aj/insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar aj/current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-date-time' func
See help of `format-time-string' for possible replacements")

(defvar aj/current-time-format "%a %H:%M:%S"
  "Format of date to insert with `aj/insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun aj/insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `aj/current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  ;       (insert (let () (comment-start)))
  (insert
   (format-time-string aj/current-date-time-format (current-time)))
  (insert "\n"))

(defun aj/insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string aj/current-time-format (current-time)))
  (insert "\n"))

;; Various

(defun aj/insert-current-date ()
  "Insert the current date into the current buffer."
  (interactive)
  (insert (format-time-string aj/current-date-format (current-time))))

;; From rejeep
;; http://github.com/rejeep/emacs
(defun aj/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg
        end
        (origin (point)))
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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun aj/unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun aj/make-inserter (str)
  `(lambda ()
     (interactive)
     (insert ,str)))


;; From http://www.emacswiki.org/emacs/KeyboardMacros#toc5
(defun aj/my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to
use. If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt
          (if arg
              (read-from-minibuffer "PROMPT: ")
            "Input: "))
         (input
          (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
            (read-from-minibuffer prompt))))
    (unless (string= "" input)
      (insert input))))

(defun aj/open-line-and-indent ()
  "Splits the current line using open-line, then indents."
  (interactive)
  (save-excursion
    (open-line 1)
    (next-line)
    (indent-according-to-mode)))

(defun aj/sudo-edit (&optional arg)
  (interactive "p")
  (let ((p (point)))
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))
    (goto-char p)))

(defun aj/narrow-to-paragraph ()
  (interactive)
  "Narrow to paragraph (or its visible portion)."
  (narrow-to-region
   (save-excursion
     (backward-sentence)
     (point))
   (save-excursion
     (forward-paragraph)
     (point))))

(defun aj/narrow-to-scope ()
  ""
  (interactive)
  (save-excursion
    (backward-up-list)
    (save-excursion
      (beginning-of-line)
      (setq start (point)))
    (forward-list)
    (end-of-line)
    (narrow-to-region start (point))))

(defun aj/occur (regexp &optional nlines)
  (interactive (aj/occur-read-primary-args))
  (occur-1 regexp nlines (list (current-buffer))))

(defun aj/occur-read-primary-args ()
  (list
   (read-regexp "List lines matching regexp"
                (aj/occur-get-default-regexp))
   (when current-prefix-arg
     (prefix-numeric-value current-prefix-arg))))

(defun aj/occur-get-default-regexp ()
  (cond
   ((region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((symbol-name (symbol-at-point)))
   (t
    (car regexp-history))))

(defun aj/query-replace-html-entitities ()
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

(defun aj/replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in))

(defun aj/fill-paragraph-line ()
  ""
  (interactive)
  (save-excursion
    (setq bol (line-beginning-position))
    (end-of-line)
    (setq eol (point))
    (fill-region bol eol)))

(defun aj/open-file ()
  (interactive)
  (let ((path (read-file-name "File: ")))
    ;; TODO Should only complete on dir but do not know how!
    (shell-command
     (concat
      (if (eq system-type 'gnu/linux)
          "xdg-open "
        "start ")
      path))))

(defun aj/file-browser-here ()
  (interactive)
  (shell-command
   (if (eq system-type 'gnu/linux)
       "xdg-open ."
     "start .")))

(defun aj/terminal-here ()
  ""
  (interactive)
  (shell-command
   (concat
    "gnome-terminal --working-directory=\"" default-directory "\"")))

(defun aj/date-arvid ()
  "Runs the `date` command"
  (interactive)
  (shell-command "date"))


(defun aj/reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list
                  (query-replace-read-to
                   (reb-target-binding
                    reb-regexp)
                   "Query replace" t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun aj/insert-tezos-unicode-symbol ()
  ""
  (interactive)
  (insert "ꜩ"))

(defun aj/highlight-all-logs ()
  ""
  (interactive)
  (hi-lock-mode t)
  (save-excursion
    (let ((end-of-window
           (progn
             (move-to-window-line -1)
             (end-of-line)
             (point)))
          (hi-lock-auto-select-face t))
      (move-to-window-line 0)
      (while (and (re-search-forward "^.* - \\(.*?:\\) .*$")
                  (< (point) end-of-window))
        (let ((color
               (let ((hi-lock-auto-select-face t))
                 (hi-lock-read-face-name)))
              (color_alt
               (subseq (secure-hash 'md5 (match-string 1)) 0 6)))
          (highlight-regexp (regexp-quote (match-string 1)) color)))
      ;; (message (match-string 1))
      )))

(defun aj/howdoi (query &optional n)
  ""
  (interactive "MQuery: \np")
  (let ((buf (format "*howdoi: %s" query)))
    (shell-command (format "howdoi -n %d %s &"
                           n
                           (shell-quote-argument query))
                   buf)
    (display-buffer buf)))

(defun aj/capitalize-first (string)
  ""
  (if (string= string "")
      ""
    (concat
     (capitalize (substring string 0 1)) (substring string 1))))

(defun aj/dup-and-comment ()
  ""
  (interactive)
  (aj/duplicate-current-line-or-region 1)
  (previous-line)
  (comment-or-uncomment-current-line-or-region)
  (next-line))

(defun aj/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p
   (selected-window) (not (window-dedicated-p (selected-window)))))

(provide 'arvid-lib)
