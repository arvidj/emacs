(require 'psvn)

;; TODO: bind "g" to re-run diff in svn output. psvn does this ;)
;; TODO: make it nicer to apply / re-apply hunks. run update directly after applying hunk.
;; TODO: i get the feeling that psvn is much slower than dsvn. why?



;; psvn just checks if there exists a .svn sub-directory.
;; ((svn-dir (format "%s%s"
;;                          (file-name-as-directory dir)
;;                          (svn-wc-adm-dir-name)))
;;         (cvs-dir (format "%sCVS" (file-name-as-directory dir))))
;;     (cond
;;      ((file-directory-p svn-dir)
;; magit uses (magit-get-top-dir dir)

(global-set-key (kbd "C-x g") 'svn-status)

;; Add spell-checking when writing commit messages.
(add-hook 'svn-log-edit-mode-hook 'flyspell-mode)

(provide 'arvid-svn)
