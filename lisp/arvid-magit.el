;; Functions for magit.

;;; Code:

(defun aj/magit-trigger-ci ()
  "Trigger the tezos/tezos CI."
  (interactive)
  (magit-git-command-topdir "git trigger-ci"))
(defun aj/magit-rebase-tezos-master ()
  "Trigger the tezos/tezos CI."
  (interactive)
  (magit-git-command-topdir "git rb"))
(defun aj/magit-assign-marge ()
  "Trigger the tezos/tezos CI."
  (interactive)
  (magit-git-command-topdir "git assign-marge"))
(defun aj/magit-resolve-all-theirs ()
  "Trigger the tezos/tezos CI."
  (interactive)
  (magit-git-command-topdir "git resolve-all --theirs --all"))
(defun aj/magit-resolve-all-ours ()
  "Trigger the tezos/tezos CI."
  (interactive)
  (magit-git-command-topdir "git resolve-all --ours --all"))
(defun aj/magit-rebase-merge-interactive ()
  "Interactive rebase on the last merge commit."
  (interactive)
  (magit-git-command-topdir "git rebase-merge-interactive"))

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
  (display-fill-column-indicator-mode))


;; It's convenient to define some shell alias that are available in the '!' (`magit-run`) transient in `magit-status`. Here we define to functions that are pertinent in `tezos/tezos` repos:

(defun aj/run-ciao ()
  ""
  (interactive)
  (magit-shell-command-topdir "make -s -C ci"))

(defun aj/run-manifest ()
  ""
  (interactive)
  (magit-shell-command-topdir "make -s -C manifest"))

(defun aj/minibuffer-insert-current-branch ()
  ""
  (interactive)
  (when-let (b (magit-get-current-branch))
    (insert b)))

(defun aj/minibuffer-insert-branch-prefix ()
  ""
  (interactive)
  (insert "arvid@"))

(use-package
 magit
 :ensure t
 :commands
 magit-mode
 magit-status
 :config
 (global-set-key (kbd "C-x g") 'magit-status)
 (add-hook 'git-commit-mode-hook 'aj/git-commit-mode-hook)
 (add-hook 'git-rebase-mode-hook 'aj/git-rebase-mode-hook)
 (define-key
  magit-process-mode-map (kbd "C-c C-o") 'browse-url-at-point)

 ;; Run transient
 (transient-append-suffix
  'magit-run "!" '("t" "trigger CI" aj/magit-trigger-ci))

 (transient-append-suffix
  'magit-run "!"
  '("r" "rebase on tezos/master" aj/magit-rebase-tezos-master))

 (transient-append-suffix
  'magit-run "!"
  '("n"
    "resolve all conflicts keeping --theirs"
    aj/magit-resolve-all-theirs))

 (transient-append-suffix
  'magit-run "!"
  '("m"
    "resolve alls conflict keeping --ours"
    aj/magit-resolve-all-ours))

 (transient-append-suffix
  'magit-run "!"
  '("u" "assign to marge" aj/magit-assign-marge))

 (transient-append-suffix
  'magit-run "S" '("c" "run CIAO" aj/run-ciao))

 (transient-append-suffix
  'magit-run "c" '("m" "run manifest" aj/run-manifest))

 ;; For more details on GitLab push options, see
 ;; https://docs.gitlab.com/ee/user/project/push_options.html
 (transient-append-suffix
  'magit-push "-u"
  '(1
    "=c"
    "Create merge request"
    "--push-option=merge_request.create"))

 (transient-append-suffix
  'magit-push "-u"
  '(1 "=s" "Set [ci.skip]" "--push-option=ci.skip"))

 ;; This doesn't make any sense because the label 'ci--docker' doesn't
 ;; have any meaning right now.
 ;;
 ;; (transient-append-suffix
 ;;  'magit-push "-u"
 ;;  '(1
 ;;    "=d"
 ;;    "Add label [ci--docker]"
 ;;    "--push-option=merge_request.label=ci--docker"))

 ;; Rebase transient
 (transient-append-suffix
  'magit-rebase "u"
  '("M" "latest merge-commit" aj/magit-rebase-merge-interactive))
 
 ;; Show color in magit-process (convenient for pre-commit hook)
 ;; https://www.reddit.com/r/emacs/comments/15gjjs4/magit_process_buffer_shows_ansi_codes_instead_of/
 (setq magit-process-finish-apply-ansi-colors t)

 ;; Make it easier to create branch names
 (define-key minibuffer-mode-map (kbd "C-x C-p") #'aj/minibuffer-insert-branch-prefix)
 (define-key minibuffer-mode-map (kbd "C-x C-b") #'aj/minibuffer-insert-current-branch))

(use-package forge :ensure t :after magit)

(provide 'arvid-magit)
