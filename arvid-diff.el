;; TODO Overides M-k for hunk-kill.

(define-keys diff-mode-map
  '(("M-n" scroll-up-keep-cursor)
	("M-p" scroll-down-keep-cursor)
	("M-k" next-line)
	("RET" diff-goto-source)))

(provide 'arvid-diff)
