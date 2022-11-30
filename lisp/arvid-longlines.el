;; Replacement for deprecated longlines-mode

(defun aj/set-visual-wrap-column (new-wrap-column &optional buffer)
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
	(add-hook 'window-configuration-change-hook 'aj/update-visual-wrap-column nil t)
	(let ((windows (get-buffer-window-list)))
	  (while windows
		(when (window-live-p (car windows))
		  (with-selected-window (car windows)
			(aj/update-visual-wrap-column)))
        (setq windows (cdr windows))))))


(defun aj/update-visual-wrap-column ()
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
        (aj/set-visual-wrap-column 80))
    (visual-line-mode nil)
    (aj/set-visual-wrap-column 0)))

(provide 'arvid-longlines)
