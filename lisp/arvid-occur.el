(defun aj/kill-occurrence-at-point ()
  "Kills the occurence at point"
  (interactive)
  ;; Store the point here. It seems like save-excursion does not work
  ;; across a revert.
  (let ((current-point (point)))
    (save-window-excursion
      (occur-mode-goto-occurrence)
      (kill-whole-line))
    (revert-buffer)
    ;; current-point could now be larger than the buffer, but
    ;; goto-char doesnt care. It
    (goto-char current-point)))
(define-key occur-mode-map (kbd "C-k") 'aj/kill-occurrence-at-point)

(provide 'arvid-occur)
