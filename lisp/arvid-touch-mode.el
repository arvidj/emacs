(define-minor-mode arvid-touch-mode
  "Toggle Touch mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Touch mode is enabled, the current buffer is touched every n
seconds"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Touch"
  ;; The minor mode bindings.
  nil
  :group 'touch


  (if arvid-touch-mode
      (setq-local arvid-touch-timer
                  (run-with-timer
                   arvid-touch-timer-timeout
                   arvid-touch-timer-timeout 'touch))
    (cancel-timer arvid-touch-timer)))

(defcustom arvid-touch-timer-timeout 0.1
  "Touching delay")

(defun touch ()
  "updates mtime on the file for the current buffer"
  (interactive)
  (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
  (clear-visited-file-modtime))
