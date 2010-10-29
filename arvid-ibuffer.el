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
					(filename . "remote/live/gc_gb")
					(filename . "bgluteus_remote_dev/magenta/goodiebag/")))
               ("goodiebag-dev"
                (or (filename . "remote/dev/goodiebag")
					(filename . "bgluteus_remote_dev/magenta_dev/goodiebag/")))
               ("goodiebag-local"
                (or (filename . "local/gc_gb")
					(filename . "local/goodiebag")
					(filename . "public_html/goodiebag")))
               ("framtidensmat-live"
                (filename . "remote/live/framtidensmat"))
               ("framtidensmat-dev"
                (filename . "remote/dev/framtidensmat"))
			   ("framtidensmat-local"
                (or (filename . "public_html/framtidensmat")
					(filename . "local/framtidensmat")
					(filename . "local/gc_fm")))
               ("wwoof-dev"
                (filename . "remote/dev/wwoof_community"))
			   ("wwoof-live"
                (filename . "remote/live/wwoof_community"))
			   ("quinyx"
                (filename . "remote/dev/quinyx/"))
			   ("pulsteknik"
                (filename . "remote/dev/pulsteknik/"))

			   ("gc_collectors_social-local"
                (or (filename . "local/gc_collectors_social/")
					(filename . "quinyx/typo3conf/ext/gc_collectors_social/")))

			   ("gc_veguestbook_hooks-local"
                (or (filename . "local/gc_veguestbook_hooks/")
					(filename . "quinyx/typo3conf/ext/gc_veguestbook_hooks/")))

			   ("core-live"
                (filename . "remote/live/core-live/"))

			   ;; Local core trunk
			   ("core-arvid-local"
                (or (filename . "public_html/core-arvid-local")
					(filename . "local/core-arvid-local")))

			   ;; Local magenta 2.0 development
			   ("ssfe"
                (or (filename . "public_html/core-magenta-2.0-git")
					(filename . "local/gc_system")
					(filename . "local/core-magenta-2.0-git")))

			   ("ssfe-remote"
				(or (filename . "bgluteus_remote_dev/magenta_dev/core-magenta2")))

			   ;; Some other projects
			   ("nevarforget"
                (or (filename . "dev/nevarforget")))

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

(provide 'arvid-ibuffer)

