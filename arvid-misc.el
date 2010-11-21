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

(provide 'arvid-misc)
