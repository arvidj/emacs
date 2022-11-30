;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; (toggle-debug-on-error)

(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

(add-to-list 'load-path "~/.emacs.d/lisp")

(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

;; sets up package, use-package and repositories
(require 'arvid-package)


(require 'arvid-misc)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(require 'arvid-func)

(require 'arvid-machine-specific) 

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(load "arvid-backup-autosave.el")

;; For opening recently opened files
(require 'arvid-recentf)

(require 'arvid-editserver)

;; Programming, etc
(require 'arvid-haskell)
(require 'arvid-coq)
;; (require 'arvid-flymake)
;; (require 'arvid-sql)
;; (require 'arvid-lisp)
;; (require 'arvid-js)
;; (require 'arvid-js-comint)
(require 'arvid-python)
;; (require 'arvid-css)
;; (require 'arvid-autopair)
(electric-pair-mode)
;; (require 'arvid-c)
;; (require 'arvid-php)
;; (require 'arvid-sgml)
;; (require 'arvid-ts)
(require 'arvid-programming)
(require 'arvid-sh)
;; (require 'arvid-etest)
;; (require 'arvid-ruby)



;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(require 'arvid-ocaml)
;; (require 'arvid-ada)
;; (require 'coffee-mode)
;; (require 'rust-mode)
;; (require 'php-comint)
;; (require 'phpunit)
(require 'arvid-michelson)
;; (require 'arvid-mligo)
(require 'arvid-ligo)

(require 'arvid-mr)
(require 'arvid-worklog)
(require 'arvid-docker)
(require 'arvid-typescript)

(setq inferior-php-program-command "/opt/local/bin/phpsh")
;; 

;; (add-hook 'after-init-hook #'global-flycheck-mode)
(load "arvid-flycheck.el")


;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(load "arvid-keys.el")
;; (require 'arvid-bashmarks)


;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(load "arvid-gui.el")
(require 'arvid-windows)
;; (require 'fill-column-indicator)
;; (setq fci-style 'rule)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. modes
(require 'arvid-org-mode)
(require 'arvid-org-clock)
;; (require 'arvid-helm-bibtex)
(require 'arvid-ebib)
(require 'arvid-yasnippet)
(require 'arvid-yaml)
(require 'arvid-gitlab)
(require 'arvid-gitlab-ci)
;; (load "arvid-mk-project.el")
(require 'arvid-projectile)
;; (require 'arvid-auto-complete)
(require 'arvid-ido)
(require 'arvid-helm)
;; TODO It would be nice if we have a command vc-status that checks if
;; default-dir is under svn or git and calls svn-status or
;; magit-status.
;; (require 'arvid-svn)
(require 'arvid-magit)
(require 'arvid-smerge)
(require 'arvid-ibuffer)
(require 'arvid-iedit)
(require 'arvid-uniquify)
(require 'use-package)
(use-package browse-kill-ring :ensure t)

;; Must load after org-mode
(require 'arvid-ott)


;; package does not exist
;; (use-package kill-ring-ido :ensure t)
(require 'arvid-dired)
;; (require 'arvid-jira)
(require 'rejeep-comment)
;; Cant find the timeclock in cli mode? 
;; (require 'arvid-timeclock)
(use-package wrap-region :ensure t)
(require 'wrap-region)
(wrap-region-mode t)
;; (require 'vimperator-mode)
;; (require 'arvid-smex)
(require 'arvid-diff)
(require 'arvid-drag-stuff)
;; (require 'arvid-anything)
(require 'arvid-shell)
(require 'arvid-conf)
;; (require 'arvid-fic)
;; (require 'arvid-command-frequency)
;; (require 'arvid-google-translate)
(require 'arvid-wspace)
;; (require 'arvid-sublain)
(require 'arvid-flyspell)
;; (require 'arvid-w3m)
;; (require 'arvid-smart-scan)
;; (require 'emacs-spotify)
;; (require 'arvid-todostack)
;; (require 'arvid-ace-jump-mode)

(require 'arvid-calendar)
;; (require 'arvid-evil)
;; (require 'arvid-org-jira)
(use-package expand-region :ensure t)
;; (require 'expand-region)

(global-set-key (kbd "M-9") 'er/expand-region)

;; (load "plugins/yacc/yacc.el")

(require 'arvid-multiple-cursors)

(require 'arvid-dir-vars)

;; (require 'timelog-mode)

(require 'arvid-ack)

;; for working with ETE scenarios
(require 'arvid-ete-scenario-mode)
(add-to-list 'nxml-mode-hook 'maybe-load-scenario-mode)
(defun maybe-load-scenario-mode ()
  (if (string-match-p "scenario\\.xml$" (buffer-name))
      (ete-scenario-mode)))


(load "arvid-markdown.el")
(load "arvid-latex.el")
(require 'arvid-rst)
(require 'arvid-nomadic-labs)
(require 'arvid-bashmarks)

(use-package
  dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))

;; (require 'arvid-langtool)

(defvar my-do-not-capitalize-words '("bsp" "bsplib" "with")
  "My personal list of words that doesn't get capitalized in titles.")
(require 'title-capitalization)


;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;; (load "arvid-encodings.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Activate functions
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Customization
(load "arvid-custom.el") 
	

;; rest of your .emacs goes here

;; (message "My .emacs loaded in %ds"
;; 		 (destructuring-bind (hi lo ms) (current-time)
;; 		   (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
(put 'scroll-left 'disabled nil)


;; (use-package magit-svn :ensure t)

;; (require 'howdoi)

;; (require 'arvid-windows-nt)
(put 'narrow-to-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'narrow-to-page 'disabled nil)
