;;; Check out these modes for handling whitespace in a more ordered fashion.
;; http://github.com/glasserc/ethan-wspace
;; http://gist.github.com/452824

(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'arvid-misc)
