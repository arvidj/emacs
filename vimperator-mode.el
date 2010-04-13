(defvar vimperator-mode-keymap nil
  "Keymap for vimperator mode.")

(defun vimperator-commit () 
  "Doc" 
  (interactive)
  (save-buffer)
  (delete-frame))

(if vimperator-mode-keymap 
    nil
  (progn
    (setq vimperator-mode-keymap (make-sparse-keymap))
    (define-key vimperator-mode-keymap (kbd "C-c C-c") 'vimperator-commit)))


(define-minor-mode vimperator-mode "Minor mode for editing inputs from vimperator."
  :lighter " vimp"
  :keymap vimperator-mode-keymap
  (message "vimperator mode init"))

(provide 'vimperator-mode)