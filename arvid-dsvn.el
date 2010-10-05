(require 'dsvn)

;; TODO: bind "g" to re-run diff in svn output
;; TODO: make it nicer to apply / re-apply hunks. run update directly after applying hunk.

(global-set-key (kbd "C-x g") 'svn-status)

(provide 'arvid-dsvn)
