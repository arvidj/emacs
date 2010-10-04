;; TODO: typo3-clear-cache should be able to figure out which cache to
;; clear by checking the current filename. It would also be nice to
;; have an option for only clearing configuration.

(add-to-list 'load-path "~/.emacs.d/")
(require 'arvid-loadpaths)

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(load "arvid-backup-autosave.el")

;; For opening recently opened files
(require 'arvid-recentf)

;; Programming, etc
(require 'arvid-haskell)
(require 'arvid-flymake)
(require 'arvid-sql)
(require 'arvid-sql-gluteus)
(require 'arvid-lisp)
(require 'arvid-js)
(require 'arvid-python)
(require 'arvid-css)
(require 'arvid-autopair)
;; (require 'rainbow-mode)
(require 'arvid-c)
(require 'arvid-php)
(require 'arvid-sgml)
(require 'arvid-ts)
(load "arvid-programming.el")
(load "arvid-sh.el")
(require 'arvid-etest)
(require 'arvid-ruby)

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
(require 'arvid-yasnippet)
(load "arvid-mk-project.el")
(require 'arvid-auto-complete)
(require 'arvid-ido)
(require 'arvid-dsvn)
(require 'arvid-magit)
(require 'arvid-ibuffer)
(require 'arvid-iedit)
(require 'arvid-uniquify)
(require 'browse-kill-ring)
(require 'arvid-dired)
(require 'arvid-jira)
(require 'rejeep-comment)
(require 'arvid-timeclock)
(require 'wrap-region)
(wrap-region-mode t)
(require 'vimperator-mode)
(require 'arvid-misc)
(require 'arvid-smex)
(require 'arvid-diff)
(require 'arvid-drag-stuff)
(require 'arvid-anything)
(require 'arvid-shell)


;;;;;;;;;;;;;;;;;;;;;;
;; Encodings
;; (load "arvid-encodings.el")

;;;;;;;;;;;;;;;;;;;;;;
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
