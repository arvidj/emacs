(require 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("org"
                (mode . org-mode))
               ("emacs"
                (filename . ".emacs.d/"))
               
			   ;; Various typo projects
			   ("goodiebag-live"
				(or (filename . "remote/live/goodiebag")
					(filename . "remote/live/gc_gb")))
               ("goodiebag-dev"
                (filename . "remote/dev/goodiebag"))
               ("goodiebag-local"
                (or (filename . "local/gc_gb")
					(filename . "local/goodiebag")
					(filename . "public_html/goodiebag")))
               ("framtidensmat-live"
                (filename . "remote/live/framtidensmat"))
               ("framtidensmat-dev"
                (filename . "remote/dev/framtidensmat"))
               ("wwoof-dev"
                (filename . "remote/dev/wwoof_community"))
			   ("wwoof-live"
                (filename . "remote/live/wwoof_community"))
			   ("quinyx"
                (filename . "remote/dev/quinyx/"))
			   ("pulsteknik"
                (filename . "remote/dev/pulsteknik/"))
			   

			   ("core-live"
                (filename . "remote/live/core-live/"))
			   
			   ;; Local cure trunk
			   ("core-arvid-local"
                (or (filename . "public_html/core-arvid-local")
					(filename . "local/core-arvid-local")))

			   ;; Local magenta 2.0 development
			   ("ssfe"
                (or (filename . "public_html/core-magenta-2.0-git")
					(filename . "local/gc_system")
					(filename . "local/core-magenta-2.0-git")))
			   
			   
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



