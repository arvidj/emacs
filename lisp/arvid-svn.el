(require 'psvn)

;; TODO: bind "g" to re-run diff in svn output. psvn does this ;)
;; TODO: make it nicer to y / re-apply hunks. run update directly after applying hunk.
;; TODO: i get the feeling that psvn is much slower than dsvn. why?

;; psvn just checks if there exists a .svn sub-directory.
;; ((svn-dir (format "%s%s"
;;                          (file-name-as-directory dir)
;;                          (svn-wc-adm-dir-name)))
;;         (cvs-dir (format "%sCVS" (file-name-as-directory dir))))
;;     (cond
;;      ((file-directory-p svn-dir)
;; magit uses (magit-get-top-dir dir)

(global-set-key (kbd "C-x G") 'svn-status)

;; Add spell-checking when writing commit messages.
(add-hook 'svn-log-edit-mode-hook 'flyspell-mode)
(setq svn-status-default-diff-arguments '("--diff-cmd" "diff" "-x" "-wbBu"))

(defconst arvid-svn-resolve-resolution-accept-types
  '("working" "theirs-full" "base" "mine-full")
  "Arguments possible to svn resolve --accept")

(defun arvid-svn-resolve-conflict-at-point ()
  (interactive)
  (catch :error
	(let* ((resolution (ido-completing-read
						"Resolve how? "
						arvid-svn-resolve-resolution-accept-types
						nil
						t)))
	  (svn-status-create-arg-file svn-status-temp-arg-file "" (svn-status-marked-files) "")
	  (svn-run
	   t t 'resolve
	   "resolve" "--accept" resolution "--targets" svn-status-temp-arg-file)
	  (svn-status-update))))

(require 'f)
(defun arvid-svn-find-alternative-file ()
    ""
  (interactive)
  (let* ((root (f-traverse-upwards
               (lambda (path)
                 (f-exists? (f-expand ".svn" path)))
               "."))
        (src-branch (f-base (f-canonical ".")))
        (tgt-branch "branches/tech-report")
        (src-file (f-filename (f-this-file)))
        (tgt-file (f-join "../" tgt-branch src-file)))
    (find-file-other-window tgt-file)
    (message tgt-file)
    ;; (message file)
))

(provide 'arvid-svn)
