
(defun wordreference-retrieve-data (word)
  (interactive)  
  (let* ((url (format "http://www.wordreference.com/definicion/%s" word))
         (buffer (url-retrieve-synchronously url))
         definitions)
    (with-current-buffer buffer
      (while (search-forward-regexp 
              "<h3>\\(.*? \\)</h3><ol.*?>\\(.*?\\)</ol>" nil t)
        (setq definitions (cons (list (match-string 1) 
                                      (match-string 2))
                                definitions)))
      (reverse definitions))))

(defun wordreference-clear-buffer ()
  (interactive) 
  (clipboard-kill-region 1 (point-max))
  (goto-char (point-min)))

(defun wordreference-replace-string (str rep)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward str nil t)
      (replace-match rep))))

(defun wordreference-replace-regexp (regexp rep)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match rep))))

(defun wordreference-define-word (word)
  (interactive "MWord: ")
  (let ((defs (wordreference-retrieve-data word)))
    (split-window-vertically)
    (windmove-down)
    (switch-to-buffer "* wordreference *")
    (wordreference-clear-buffer)
    (goto-char (point-min))
    (save-excursion
      (mapc (lambda (s) 
              (insert (format "%s\n%s\n\n" (car s) (nth 1 s) (current-buffer))))
            defs))
    (recode-region (point-min) (point-max) 'utf-8-unix 'utf-8-unix)
    (wordreference-replace-string "<br>" "\n")
    (wordreference-replace-regexp "[ ]*<li>[ ]*" "\n* ")
    (wordreference-replace-regexp "<span.*?class=i.*?>\\(.*?\\)</span>" "\t\\1")
    (wordreference-replace-regexp "<span.*?>\\(.*?\\)</span>" "\\1"))
  nil)





(defun wordreference-define-current-word ()
  (interactive)
  (wordreference-define-word (current-word)))
