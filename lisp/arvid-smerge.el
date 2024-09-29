(defun aj/smerge-keep-mine-all ()
  ""
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (ignore-errors
             'user-error
             (progn
               (smerge-next)
               t))
      (smerge-keep-mine))))

(defun aj/smerge-keep-other-all ()
  ""
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (ignore-errors
             'user-error
             (progn
               (smerge-next)
               t))
      (smerge-keep-other))))

(eval-after-load 'smerge-mode
  (lambda ()
    (define-key smerge-mode-map (kbd "C-c m") smerge-basic-map)))

(provide 'arvid-smerge)
