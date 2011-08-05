;; timeclock
(require 'timeclock-x)

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

(timeclock-initialize)

(defadvice timeclock-visit-timelog (after my-timeclock-visit-timelog () activate)
  "Advice the timelog timeclock-visit-timelog function so that the
timelog will be re-read after each save."
  (add-hook 'after-save-hook 'timeclock-reread-log nil t))

(provide 'arvid-timeclock)
