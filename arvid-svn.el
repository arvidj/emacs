(require 'psvn)

;; TODO: bind "g" to re-run diff in svn output
;; TODO: make it nicer to apply / re-apply hunks. run update directly after applying hunk.

(global-set-key (kbd "C-x g") 'svn-status)

;; Add spell-checking when writing commit messages.
(add-hook 'svn-log-edit-mode-hook flyspell-mode)

(provide 'arvid-svn)
