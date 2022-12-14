;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; (toggle-debug-on-error)

(setq aj/time (current-time))
(defun profile-package (package)
  ""
  (message "%s: after loading package %s, with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract (current-time) aj/time)))
           package
           gcs-done)
  (setq aj/time (current-time)))

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/vendor")

;; sets up package, use-package and repositories
(require 'arvid-package) (profile-package "arvid-package")
(require 'arvid-lib) (profile-package "'arvid-lib")
(require 'arvid-misc) (profile-package "'arvid-misc")

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(require 'arvid-func) (profile-package "'arvid-func")

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(require 'arvid-backup-autosave) (profile-package "'arvid-backup-autosave")
(require 'arvid-recentf) (profile-package "'arvid-recentf") ; For opening recently opened files
(require 'arvid-editserver) (profile-package "'arvid-editserver")

;; Generalities for programmign
(require 'arvid-flycheck) (profile-package "'arvid-flycheck")
(require 'arvid-company) (profile-package "'arvid-company")

;; Programming, etc
(require 'arvid-haskell) (profile-package "'arvid-haskell")
(require 'arvid-coq) (profile-package "'arvid-coq")
(require 'arvid-lisp) (profile-package "'arvid-lisp")
(require 'arvid-python) (profile-package "'arvid-python")
(require 'arvid-css) (profile-package "'arvid-css")
(electric-pair-mode)
;; (require 'arvid-c) (profile-package "'arvid-c")
;; (require 'arvid-sgml) (profile-package "'arvid-sgml")
(require 'arvid-sh) (profile-package "'arvid-sh")
(require 'arvid-yaml) (profile-package "'arvid-yaml")
(require 'arvid-ocaml) (profile-package "'arvid-ocaml")
(require 'arvid-michelson) (profile-package "'arvid-michelson")
(require 'arvid-ligo) (profile-package "'arvid-ligo")
(require 'arvid-docker) (profile-package "'arvid-docker")
(require 'arvid-typescript) (profile-package "'arvid-typescript")

(require 'arvid-compile) (profile-package "'arvid-compile")

;; Nomadic Labs specific
(require 'arvid-nomadic-labs) (profile-package "'arvid-nomadic-labs")

;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(require 'arvid-keys) (profile-package "'arvid-keys")
;; (require 'arvid-bashmarks) (profile-package "'arvid-bashmarks")

;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(require 'arvid-gui) (profile-package "'arvid-gui")
(require 'arvid-windows) (profile-package "'arvid-windows")
;; (require 'fill-column-indicator) (profile-package "'fill-column-indicator")
;; (setq fci-style 'rule)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. modes
(load "server") ; https://stackoverflow.com/questions/5570451/how-to-start-emacs-server-only-if-it-is-not-started
(unless (server-running-p) (server-start))
(require 'arvid-org-mode) (profile-package "'arvid-org-mode")
(require 'arvid-org-clock) (profile-package "'arvid-org-clock")
(require 'arvid-yasnippet) (profile-package "'arvid-yasnippet")
(require 'arvid-gitlab) (profile-package "'arvid-gitlab")
(require 'arvid-gitlab-ci) (profile-package "'arvid-gitlab-ci")
(require 'arvid-projectile) (profile-package "'arvid-projectile")
(require 'arvid-ido) (profile-package "'arvid-ido")
(require 'arvid-helm) (profile-package "'arvid-helm")
(require 'arvid-magit) (profile-package "'arvid-magit")
(use-package browse-at-remote :ensure t)
(require 'arvid-smerge) (profile-package "'arvid-smerge")
(require 'arvid-ibuffer) (profile-package "'arvid-ibuffer")
(require 'arvid-iedit) (profile-package "'arvid-iedit")
(require 'arvid-uniquify) (profile-package "'arvid-uniquify")
(use-package browse-kill-ring :ensure t)
(require 'arvid-dired) (profile-package "'arvid-dired")

;; Must load after org-mode
(require 'arvid-ott) (profile-package "'arvid-ott")

(require 'rejeep-comment) (profile-package "'rejeep-comment")
(require 'arvid-diff) (profile-package "'arvid-diff")
(require 'arvid-drag-stuff) (profile-package "'arvid-drag-stuff")
(require 'arvid-conf) (profile-package "'arvid-conf")
(require 'arvid-wspace) (profile-package "'arvid-wspace")
(require 'arvid-flyspell) (profile-package "'arvid-flyspell")

(require 'arvid-calendar) (profile-package "'arvid-calendar")
(use-package expand-region
  :ensure t
  :bind ("M-9" . 'er/expand-region))

(require 'arvid-multiple-cursors) (profile-package "'arvid-multiple-cursors")
(require 'arvid-dir-vars) (profile-package "'arvid-dir-vars")
(require 'arvid-markdown) (profile-package "'arvid-markdown")
(require 'arvid-latex) (profile-package "'arvid-latex")
(require 'arvid-rst) (profile-package "'arvid-rst")
(require 'arvid-bashmarks) (profile-package "'arvid-bashmarks")

;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;; (require 'arvid-encodings) (profile-package "'arvid-encodings")

;;;;;;;;;;;;;;;;;;;;;;
;; Customization
(require 'arvid-custom) (profile-package "'arvid-custom") 

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
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; 2. after GC change: Emacs ready in 4.50 seconds with 18 garbage collections

