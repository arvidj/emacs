;; flyspell-tex-batch.el
(defun flyspell-tex-batch (dir)
  (let ((buf (get-buffer-create "*flyspell-tex-batch*"))
        (tex-files (directory-files dir 'full "\\.tex\\'")))
    (switch-to-buffer buf)
    (dolist (file tex-files)
      ;; Print a header for each file.
      (insert "# " (file-name-nondirectory file) "\n\n")
      ;; Open the file and run flyspell.
      (with-current-buffer (find-file-noselect file)
        (flyspell-mode 1)
        (flyspell-buffer)
        ;; Look at all flyspell overlays and extract the words.
        (let* ((overlays (overlays-in (point-min) (point-max)))
               (words
                (mapcar
                 (lambda (o)
                   (when (flyspell-overlay-p o)
                     (buffer-substring-no-properties
                      (overlay-start o) (overlay-end o))))
                 overlays)))
          ;; Insert the words in the main spell check buffer.
          (with-current-buffer buf
            (mapcar (lambda (w) (insert "-   " w "\n")) words)
            (insert "\n")))
        ;; Close the tex file buffer
        (kill-buffer)))
    (message (buffer-string))))
