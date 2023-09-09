
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

(provide 'arvid-lib)
