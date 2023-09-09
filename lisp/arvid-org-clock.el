(setq org-clock-continuously nil)

(defun aj/time-today-at (hour minutes &optional base)
  ""
  (encode-time
   (append
    (list 0 minutes hour)
    (seq-drop (decode-time (or base (current-time))) 3))))

(defun aj/org-clock-in-out ()
  (interactive)
  (let* ((now-string (format-time-string "%H:%M" (current-time)))
         (at-time-string
          (read-string "Clock out at time: "
                       (concat now-string " -- " now-string))))
    (if (string-match
         "^\\([[:digit:]]+\\):\\([[:digit:]]+\\) -- \\([[:digit:]]+\\):\\([[:digit:]]+\\)$"
         at-time-string)
        (let ((in-time
               (aj/time-today-at
                (string-to-number (match-string 1 at-time-string))
                (string-to-number (match-string 2 at-time-string))))
              (out-time
               (aj/time-today-at
                (string-to-number (match-string 3 at-time-string))
                (string-to-number (match-string 4 at-time-string)))))
          (org-clock-in nil in-time)
          (org-clock-out nil nil out-time)
          (message "Clocked in between: %s -- %s"
                   (format-time-string "%F %H:%M" in-time)
                   (format-time-string "%F %H:%M" out-time))))))


(setq aj/time-format "%F %H:%M")

(defun aj/format-time (&optional time)
  ""
  (format-time-string aj/time-format time))

(aj/format-time)
(aj/format-time (current-time))


(defun aj/parse-time (at-time-string)
  (if (string-match
       "^\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" at-time-string)
      (let ((my-at-time
             (aj/time-today-at
              (string-to-number (match-string 1 at-time-string))
              (string-to-number (match-string 2 at-time-string)))))
        my-at-time)
    (error
     "Expected string on format HH:MM, not: %s" at-time-string)))

(aj/format-time (aj/parse-time "11:30"))

(defun aj/read-time (prompt)
  ""
  (let* ((at-time-string
          (read-string prompt
                       (format-time-string "%H:%M" (current-time)))))
    (if (string-match
         "^\\([[:digit:]]+\\):\\([[:digit:]]+\\)$" at-time-string)
        (let ((my-at-time
               (aj/time-today-at
                (string-to-number (match-string 1 at-time-string))
                (string-to-number (match-string 2 at-time-string)))))
          my-at-time)
      (error
       "Expected string on format HH:MM, not: %s" at-time-string))))

;; (defadvice read-string (around dummy-read-string (prompt &optional initial))
;;   "11:30")
;; (unwind-protect
;;     (progn
;;       (ad-enable-advice 'read-string 'around 'dummy-read-string)
;;       (ad-activate 'read-string)
;;       (aj/read-time))
;;   (ad-disable-advice 'read-string 'around 'dummy-read-string)
;;   (ad-activate 'read-string))

(defun aj/org-clock-out-at ()
  (interactive)
  (org-clock-out nil nil (aj/read-time "Clock out at: ")))

;; does not work as expected ??
(defun aj/org-clock-in-at ()
  (interactive)
  (org-clock-in nil (aj/read-time "Clock in at: ")))


;; The following snippet uses a gnome shell extension to write the
;; current active clock in the status bar.

;; Code from https://github.com/freddez/gnome-shell-simple-message
(defun current-task-to-status ()
  (interactive)
  (if (fboundp 'org-clocking-p)
      (if (org-clocking-p)
          (progn
            (call-process
             "dconf"
             nil
             nil
             nil
             "write"
             "/org/gnome/shell/extensions/simple-message/message"
             (concat "'👷🏽 " (org-clock-get-clock-string) "'"))
            (set-face-background 'mode-line "#444488"))
        (call-process
         "dconf"
         nil
         nil
         nil
         "write"
         "/org/gnome/shell/extensions/simple-message/message"
         "'⚠️ No active clock'")
        (set-face-background 'mode-line "#555753"))))

(run-with-timer 0 60 'current-task-to-status)
(add-hook 'org-clock-in-hook 'current-task-to-status)
(add-hook 'org-clock-out-hook 'current-task-to-status)
(add-hook 'org-clock-cancel-hook 'current-task-to-status)
(add-hook 'org-clock-goto-hook 'current-task-to-status)


;; todo: aj/org-clock-change-end-time ()
;; todo: aj/org-clock-change-start-time ()


(provide 'arvid-org-clock)
