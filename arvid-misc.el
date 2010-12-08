;;; Check out these modes for handling whitespace in a more ordered fashion.
;; http://github.com/glasserc/ethan-wspace
;; http://gist.github.com/452824

(setq require-final-newline t)

;; I want to be able to just discard / revert buffers that I have left
;; changes in.
(add-to-list
 'save-some-buffers-action-alist
 '(?k kill-buffer "discard this buffer"))
(add-to-list
 'save-some-buffers-action-alist
 '(?r
   (lambda (buf)
	 (save-current-buffer
	   (set-buffer buf)
	   (revert-buffer)))
   "revert this buffer"))

(defun arvid-kill-occurrence-at-point ()
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
(define-key occur-mode-map (kbd "C-k") 'arvid-kill-occurrence-at-point)

(provide 'arvid-misc)
