;;; Ido-mode
(require 'ido)

(ido-mode t)
(setq ido-case-fold t)
(ido-everywhere t)
(global-set-key "\M-." 'my-ido-find-tag)

(defun my-ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names
        (def (find-tag-default)))
    (mapc (lambda (x)
	    (unless (integerp x)
	      (push (prin1-to-string x t) tag-names)))
	  tags-completion-table)
    (find-tag (ido-completing-read "Find tag: " tag-names nil nil nil nil def))))

(provide 'arvid-ido)
