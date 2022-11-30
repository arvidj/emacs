s;; timeclock

;; package does not exist...
(use-package timeclock-x
  :ensure t
  :config
  ;; TODO: Only set prefix key once.
  ;; TODO: Make helper for defining a lot of bindings for one prefix.
  (global-set-keys
   `(("C-c ti" timeclock-in)
     ("C-c too" timeclock-out)

     ;; Most common reasons I have for clocking out.
     ("C-c tof" ,(make-timeclock-out "fika"))
     ("C-c tol" ,(make-timeclock-out "lunch"))
     ("C-c toh" ,(make-timeclock-out "hem"))
     ("C-c top" ,(make-timeclock-out "paus"))

     ("C-c tc" timeclock-change)
     ("C-c tr" timeclock-reread-log)
     ("C-c tu" timeclock-update-modeline)
     ("C-c tw" timeclock-when-to-leave-string)
     ("C-c tf" timeclock-visit-timelog)))

  (add-hook 'timeclock-in-hook '(lambda () (set-face-background 'mode-line "#444488")))
  (add-hook 'timeclock-out-hook '(lambda () (set-face-background 'mode-line "#555753")))

  (timeclock-initialize))

;; TODO:
;;  * Log time to basecamp?
;;  * A way of distinguishing support projects
;;  * Suggest project by checking timelog, also use ido for selection
;;  * Update mode-line when rereading log (defadvice the reread-log function?)
;;  * Generate report. Check the report-generation already there.
;;  * When using timeclock-change, one is prompted for comments twice.
;;  * Set fill-prefix to ">> " when editing timelog.

(defun timeclock-out (&optional arg reason find-reason)
  "Clock out like normal but add minutes spent to the end of the
log-line."
  (interactive "P")
  (or timeclock-last-event
	  (error "You haven't clocked in!"))
  (if (equal (downcase (car timeclock-last-event)) "o")
	  (error "You've already clocked out!")
	(timeclock-log
	 (if arg "O" "o")
	 (concat (or reason
				 (and timeclock-get-reason-function
					  (or find-reason (interactive-p))
					  (funcall timeclock-get-reason-function)))
			 " | Minutes spent: "
			 (number-to-string
			  (ceiling (/ (timeclock-last-period) 60)))))
	(run-hooks 'timeclock-out-hook)
	(if arg
		(run-hooks 'timeclock-done-hook))))


(defmacro make-timeclock-out (reason)
  `(lambda () (interactive) (timeclock-out nil ,reason)))


(defadvice timeclock-visit-timelog (after my-timeclock-visit-timelog () activate)
  "Advice the timelog timeclock-visit-timelog function so that the
timelog will be re-read after each save."
  (add-hook 'after-save-hook 'timeclock-reread-log nil t))

(defadvice timeclock-reread-log (after my-timeclock-reread-log () activate)
  (set-face-background
   'mode-line
   (if (equal (car timeclock-last-event) "i")
	   "#444488"
	 "#555753")))

(defun arvid-timeclock-put-time (&optional project time)
  ""
  (interactive)
  (let* ((project (or project (and timeclock-get-project-function
								  (called-interactively-p 'interactive)
				(funcall timeclock-get-project-function))))
		 (minutes-string (or time (read-from-minibuffer "Minutes: ")))
		 (minutes (string-to-number minutes-string))
		 (end-time (current-time))
		 (start-time (time-subtract end-time (seconds-to-time (* 60 minutes)))))
	(arvid-timeclock-log-aux "i" project start-time)
	(timeclock-reread-log)
	(timeclock-out-safe)
 ))

(defun arvid-timeclock-log-aux (code &optional project time)
  "Log the event CODE to the timeclock log, at the time of call.
If PROJECT is a string, it represents the project which the event is
being logged for.  Normally only \"in\" events specify a project."
  (let ((extant-timelog (find-buffer-visiting timeclock-file)))
    (with-current-buffer (find-file-noselect timeclock-file t)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-max))
	  (if (not (bolp))
	      (insert "\n"))
	  (let ((now (or time (current-time))))
	    (insert code " "
		    (format-time-string "%Y/%m/%d %H:%M:%S" now)
		    (or (and (stringp project)
			     (> (length project) 0)
			     (concat " " project))
			"")
		    "\n")
	    (if (equal (downcase code) "o")
		(setq timeclock-last-period
		      (- (timeclock-time-to-seconds now)
			 (timeclock-time-to-seconds
			  (cadr timeclock-last-event)))
		      timeclock-discrepancy
		      (+ timeclock-discrepancy
			 timeclock-last-period)))
	    (setq timeclock-last-event (list code now project)))))
      (save-buffer)
      (unless extant-timelog (kill-buffer (current-buffer)))))
  (run-hooks 'timeclock-event-hook))

;; (defun timeclock-ask-for-project ()
;;   ""
;;   (interactive)
;;   ;; TODO, pretty print todo item
;;   (concat "TODO:" (basecamp-get-projects-todolist-todo)))

(defun arvid-timeclock-to-one-line ()
  ""
  (interactive)

  )

(provide 'arvid-timeclock)
