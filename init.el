(add-to-list 'load-path "~/.emacs.d/")
(require 'arvid-loadpaths)

;; For opening recently opened files
(load "arvid-recentf.el")

;; Programming, etc
(require 'arvid-haskell)
(require 'arvid-flymake)
(require 'arvid-sql)
(load "arvid-programming.el")
(load "arvid-sh.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
(load "arvid-keys.el")


;;;;;;;;;;;;;;;;;;;;;;
;; Interface
(load "arvid-gui.el")
(require 'arvid-windows)

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. modes
(require 'arvid-org-mode)
(load "arvid-yasnippet.el")
(load "arvid-mk-project.el")
(require 'arvid-auto-complete)
(require 'arvid-ido)
(require 'arvid-psvn)
(require 'arvid-magit)
(require 'arvid-ibuffer)
(require 'arvid-iedit)
(require 'arvid-uniquify)
(require 'arvid-dired)
(require 'arvid-jira)
(require 'rejeep-comment)
(require 'arvid-timeclock)
(require 'wrap-region)
(wrap-region-mode t)

;;;;;;;;;;;;;;;;;;;;;;
;; Other
(load "arvid-backup-autosave.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;; (load "arvid-encodings.el")

;; Activate functions
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;
;; Customization
(load "arvid-custom.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(load "arvid-func.el")