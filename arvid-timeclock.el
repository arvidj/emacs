;; timeclock
(require 'timeclock-x)

;; TODO:
;;  * Log time to basecamp?
;;  * A way of distinguishing support projects
;;  * Suggest project by checking timelog, also use ido for selection
;;  * Update mode-line when rereading log (defadvice the reread-log function?)
;;  * Generate report. Check the report-generation already there.
;;  * When using timeclock-change, one is prompted for comments twice.

(defmacro make-timeclock-out (reason)
  `(lambda () (interactive) (timeclock-out nil ,reason)))

(global-set-key (kbd "C-c ti") 'timeclock-in)
(global-set-key (kbd "C-c too") 'timeclock-out)
(global-set-key (kbd "C-c tof") (make-timeclock-out "fika"))
(global-set-key (kbd "C-c tol") (make-timeclock-out "lunch"))
(global-set-key (kbd "C-c toh") (make-timeclock-out "hem"))

(global-set-key (kbd "C-c tc") 'timeclock-change)
(global-set-key (kbd "C-c tr") 'timeclock-reread-log)
(global-set-key (kbd "C-c tu") 'timeclock-update-modeline)
(global-set-key (kbd "C-c tw") 'timeclock-when-to-leave-string)
(global-set-key (kbd "C-c tf") 'timeclock-visit-timelog)

(add-hook 'timeclock-in-hook '(lambda () (set-face-background 'mode-line "#444488")))
(add-hook 'timeclock-out-hook '(lambda () (set-face-background 'mode-line "#555753")))

(timeclock-initialize)

(provide 'arvid-timeclock)
