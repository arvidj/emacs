(defun aj/magit-trigger-ci () "Trigger the tezos/tezos CI" (interactive) (magit-git-command-topdir "git trigger-ci"))
(defun aj/magit-rebase-tezos-master () "Trigger the tezos/tezos CI" (interactive) (magit-git-command-topdir "git rb"))
(defun aj/magit-resolve-all-theirs () "Trigger the tezos/tezos CI" (interactive) (magit-git-command-topdir "git resolve-all --theirs --all"))
(defun aj/magit-resolve-all-ours () "Trigger the tezos/tezos CI" (interactive) (magit-git-command-topdir "git resolve-all --ours --all"))


(use-package magit
 :ensure t
 :config
 (global-set-key (kbd "C-x g") 'magit-status)
 (add-hook 'git-commit-mode-hook 'my-git-commit-mode-hook)
 (define-key magit-process-mode-map (kbd "C-c C-o") 'browse-url-at-point)

 (transient-append-suffix 'magit-run "!"
   '("t" "trigger CI" aj/magit-trigger-ci))

 (transient-append-suffix 'magit-run "!"
   '("r" "rebase on tezos/master" aj/magit-rebase-tezos-master))

 (transient-append-suffix 'magit-run "!"
   '("n" "resolve all conflicts keeping --theirs" aj/magit-resolve-all-theirs))

 (transient-append-suffix 'magit-run "!"
   '("m" "resolve alls conflict keeping --ours" aj/magit-resolve-all-ours))


 ;; enables magit-absorb. requires `cargo install install git-absorb'
 (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
   '("x" "Absorb changes" magit-commit-absorb))

 ;; (add-hook 'git-commit-setup-hook 'my-magit-add-cc-no)
 )

;; TODO: Modify magit-visit-item so that (magit-visit-item -1) visits
;; item in other window.

;; (add-hook 'magit-mode-hook
;;           (lambda()
;;             (set-face-foreground 'magit-diff-add "green3")
;; 			(set-face-foreground 'magit-diff-del "red3")
;; 			(turn-on-magit-svn)))
;; (setq magit-mode-hook nil)


(defun my-magit-add-cc-no ()
  "Add cc no to the start of commit messages"
  (interactive)
  (insert (concat (get-u500-ticket-nr-from-branch) " ")))

(defun my-git-commit-mode-hook ()
  "Add spell-checking when writing commit messages."
  (flyspell-mode)

  (setq fill-column 70)
  (setq whitespace-line-column 70)
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode))

;; ssh://git@bitbucket.org/gluteuscreationsab/weback.git
;; https://bitbucket.org/gluteuscreationsab/weback/
;;    src/3ac8f57/static/functions.php#cl-24
(defun open-bitbucket-link ()
  ""
  (interactive)
  (let* ((rev (magit-rev-parse "HEAD"))
		 (remote (magit-get "remote" (magit-get-current-remote) "url"))
		 (url (concat
			   "https://"
			   (replace-regexp-in-string "^ssh://git@\\|.git$" "" remote)
			   "/src/"
			   rev
			   "/"
			   (magit-filename buffer-file-name)
			   "#cl-"
			   (number-to-string (line-number-at-pos))
			   )))
	(browse-url url)))



(provide 'arvid-magit)
