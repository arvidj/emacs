;; TODO: typo3-clear-cache should be able to figure out which cache to
;; clear by checking the current filename. It would also be nice to
;; have an option for only clearing configuration.
;; 
;; TODO: order by importance so that features that are essential for
;; editing are loaded even if something breaks. This means that
;; keybindings and elisp stuff goes first, since I need that to fix
;; emacs ;)
;;
;; However, in order to have bindings I need the functions that are
;; defined in the various mode right? Or is this a indication that
;; mode specific bindings should be in their own file?

(add-to-list 'load-path "~/.emacs.d/")
(require 'arvid-loadpaths)

;; TODO:
;;;   Minor mode for calculating the width / height of selections. 

;;;;;;;;;;;;;;;;;;;;;;
;; Misc. defuns
(load "arvid-func.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Move backup and autosave files.
(load "arvid-backup-autosave.el")

;; For opening recently opened files
(require 'arvid-recentf)

;; Programming, etc
(require 'arvid-haskell)
(require 'arvid-flymake)
(require 'arvid-sql)
(require 'arvid-lisp)
(require 'arvid-js)
(require 'arvid-python)
(require 'arvid-css)
(require 'arvid-autopair)
(require 'arvid-c)
(require 'arvid-php)
(require 'arvid-sgml)
(require 'arvid-ts)
(require 'arvid-programming)
(require 'arvid-sh)
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
(require 'arvid-svn)
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
(require 'arvid-conf)
(require 'arvid-fic)
(require 'arvid-command-frequency)
(require 'arvid-google-translate)

(require 'arvid-machine-specific)

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
