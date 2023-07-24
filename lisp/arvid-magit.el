;; Functions for magit.

;;; Code:

(defun aj/magit-trigger-ci () "Trigger the tezos/tezos CI." (interactive) (magit-git-command-topdir "git trigger-ci"))
(defun aj/magit-rebase-tezos-master () "Trigger the tezos/tezos CI." (interactive) (magit-git-command-topdir "git rb"))
(defun aj/magit-assign-marge () "Trigger the tezos/tezos CI." (interactive) (magit-git-command-topdir "git assign-marge"))
(defun aj/magit-resolve-all-theirs () "Trigger the tezos/tezos CI." (interactive) (magit-git-command-topdir "git resolve-all --theirs --all"))
(defun aj/magit-resolve-all-ours () "Trigger the tezos/tezos CI." (interactive) (magit-git-command-topdir "git resolve-all --ours --all"))

(defun aj/git-commit-mode-hook ()
  "Add spell-checking when writing commit messages."
  (flyspell-mode)

  (setq fill-column 70)
  (setq whitespace-line-column 70)
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face lines-tail))
  (display-fill-column-indicator-mode)

  (whitespace-mode))

(defun aj/git-rebase-mode-hook ()
  (interactive)
  "Add display fill column indicator mode."
  ;; Approximately: the size of [pick ][deadbeef00 ] [Commit message ...]
  (setq fill-column (+ 70 4 1 10 1))
  (display-fill-column-indicator-mode)
  )


(use-package magit
 :ensure t
 :commands magit-mode magit-status
 :config
 (global-set-key (kbd "C-x g") 'magit-status)
 (add-hook 'git-commit-mode-hook 'aj/git-commit-mode-hook)
 (add-hook 'git-rebase-mode-hook 'aj/git-rebase-mode-hook)
 (define-key magit-process-mode-map (kbd "C-c C-o") 'browse-url-at-point)

 (transient-append-suffix 'magit-run "!"
   '("t" "trigger CI" aj/magit-trigger-ci))

 (transient-append-suffix 'magit-run "!"
   '("r" "rebase on tezos/master" aj/magit-rebase-tezos-master))

 (transient-append-suffix 'magit-run "!"
   '("n" "resolve all conflicts keeping --theirs" aj/magit-resolve-all-theirs))

 (transient-append-suffix 'magit-run "!"
   '("m" "resolve alls conflict keeping --ours" aj/magit-resolve-all-ours))

 (transient-append-suffix 'magit-run "!"
   '("u" "assign to marge" aj/magit-assign-marge))

 (transient-append-suffix 'magit-push "-u"
   '(1 "=c" "Set push option" "--push-option=merge_request.create"))
 )

(provide 'arvid-magit)


