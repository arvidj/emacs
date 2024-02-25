;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (toggle-debug-on-error)

(setq aj/time (current-time))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; sets up package, use-package and repositories
(require 'arvid-package)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(require 'arvid-lib)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. configuration
(require 'arvid-misc)

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(require 'arvid-backup-autosave)
(require 'arvid-recentf)
;; TODO: install package from elpa instead
;; see https://github.com/stsquad/emacs_chrome
(require 'arvid-editserver)

;; Generalities for programming
(require 'arvid-flycheck)
(require 'arvid-company)
(require 'arvid-lsp)

;; Programming, etc
(require 'arvid-lisp)
(require 'arvid-sh)
(require 'arvid-yaml)
(require 'arvid-ocaml)
(require 'arvid-cram)
(require 'arvid-docker)
(require 'arvid-typescript)
(require 'arvid-sql)
(require 'arvid-nix)

(require 'arvid-compile)

;; Nomadic Labs specific
(require 'arvid-nomadic-labs)

;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(require 'arvid-keys)
(require 'arvid-editing)

;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(require 'arvid-gui)
;; (require 'fill-column-indicator)
;; (setq fci-style 'rule)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. modes
(load "server") ; https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
(unless (server-running-p)
  (server-start))
(require 'arvid-org-mode)
(require 'arvid-org-clock)
(require 'arvid-org-present)
(require 'arvid-yasnippet)
(require 'arvid-gitlab-ci)
(require 'arvid-projectile)
(require 'arvid-vertico)
(require 'arvid-magit)
(use-package browse-at-remote :ensure t)
(require 'arvid-smerge)
(require 'arvid-ibuffer)
(require 'arvid-iedit)
(require 'arvid-uniquify)
(use-package browse-kill-ring :ensure t)
(require 'arvid-dired)
(require 'arvid-shell)
(require 'arvid-tramp)
(require 'arvid-occur)

(require 'rejeep-comment)
(require 'arvid-diff)
(require 'arvid-drag-stuff)
;; Setup for conf-mode for editing configuration files such as [.gitconfig].
(require 'arvid-conf)
(require 'arvid-ws-butler)
(require 'arvid-flyspell)

(require 'arvid-calendar)
(use-package
 expand-region
 :ensure t
 :bind ("M-9" . 'er/expand-region))

(require 'arvid-multiple-cursors)
(require 'arvid-dir-vars)
(require 'arvid-markdown)
(require 'arvid-latex)
(require 'arvid-rst)
(require 'arvid-bashmarks)
(require 'arvid-auto-insert)
(require 'arvid-ffap)
(require 'arvid-chatgpt-shell)
(use-package visual-regexp :ensure t)

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

;; Start up time

;; 1. before any changes: 6.3s
;;
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract
                      after-init-time before-init-time)))
            gcs-done)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Used in i3 to make sure that Emacs communicates with the keychain
;; program to automatically unlock ssh keys.
(use-package
 keychain-environment
 :ensure t
 :config (keychain-refresh-environment))

(require 'arvid-direnv)

;; 2. after GC change: Emacs ready in 4.50 seconds with 18 garbage collections
(put 'magit-clean 'disabled nil)

(setq comint-output-filter-functions '(comint-osc-process-output))
