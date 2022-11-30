(toggle-debug-on-error)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

;; sets up package, use-package and repositories
(require 'arvid-package)
(require 'arvid-lib)
(require 'arvid-misc)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(require 'arvid-func)

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(require 'arvid-backup-autosave)
(require 'arvid-recentf) ; For opening recently opened files
(require 'arvid-editserver)
;; Programming, etc
(require 'arvid-haskell)
(require 'arvid-coq)
(require 'arvid-lisp)
(require 'arvid-python)
(require 'arvid-css)
(electric-pair-mode)
;; (require 'arvid-c)
;; (require 'arvid-sgml)
(require 'arvid-sh)
(require 'arvid-yaml)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(require 'arvid-ocaml)
(require 'arvid-michelson)
(require 'arvid-ligo)
(require 'arvid-docker)
(require 'arvid-typescript)

;; Nomadic Labs specific
(provide 'arvid-nomadic-labs)

(require 'arvid-flycheck)


;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(require 'arvid-keys)
;; (require 'arvid-bashmarks)

;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(require 'arvid-gui)
(require 'arvid-windows)
;; (require 'fill-column-indicator)
;; (setq fci-style 'rule)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. modes
(load "server") ; https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
(unless (server-running-p) (server-start))
(require 'arvid-org-mode)
(require 'arvid-org-clock)
(require 'arvid-yasnippet)
(require 'arvid-gitlab)
(require 'arvid-gitlab-ci)
(require 'arvid-projectile)
(require 'arvid-ido)
(require 'arvid-helm)
(require 'arvid-magit)
(use-package browse-at-remote :ensure t)
(require 'arvid-smerge)
(require 'arvid-ibuffer)
(require 'arvid-iedit)
(require 'arvid-uniquify)
(use-package browse-kill-ring :ensure t)
(require 'arvid-dired)

;; Must load after org-mode
(require 'arvid-ott)

(require 'rejeep-comment)
(require 'arvid-diff)
(require 'arvid-drag-stuff)
(require 'arvid-conf)
(require 'arvid-wspace)
(require 'arvid-flyspell)

(require 'arvid-calendar)
(use-package expand-region
  :ensure t
  :bind ("M-9" . 'er/expand-region))

(require 'arvid-multiple-cursors)
(require 'arvid-dir-vars)
(require 'arvid-markdown)
(require 'arvid-latex)
(require 'arvid-rst)
(require 'arvid-bashmarks)

(require 'arvid-nomadic-labs)

;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;; (require 'arvid-encodings)

;;;;;;;;;;;;;;;;;;;;;;
;; Customization
(require 'arvid-custom) 

;;;;;;;;;;;;;;;;;;;;;;
;; Activate functions
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'scroll-left 'disabled nil)
