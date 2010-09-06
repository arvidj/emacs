(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("org"
                (mode . org-mode))
               ("emacs"
                (filename . ".emacs.d/"))
               ("goodiebag-live"
                (filename . "remote/live/goodiebag"))
               ("goodiebag-dev"
                (filename . "remote/dev/goodiebag"))
               ("framtidensmat-live"
                (filename . "remote/live/framtidensmat"))
               ("framtidensmat-dev"
                (filename . "remote/dev/framtidensmat"))
               ("wwoof-dev"
                (filename . "remote/dev/wwoof_community"))
			   ("core-arvid-local"
                (filename . "public_html/core-arvid-local"))
			   ("core-arvid-local"
                (filename . "public_html/core-magenta-2.0-git"))
			   ("magit"
                (name . "^\\*magit"))
			   ("dired"
                (mode . dired-mode))
               ("svn"
                (name . "svn"))
               ("sql"
                (name . "*SQL*"))
               ))))

(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          (lambda ()
			(ibuffer-auto-mode 1)
			(ibuffer-switch-to-saved-filter-groups "default")))

(define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-toggle-filter-group)
;; TODO: in ibuffer, use ido to switch file but it should default to dir of buffer on point
;; (define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-unset-key (kbd "C-x C-b"))

(provide 'arvid-ibuffer)



