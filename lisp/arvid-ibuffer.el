(require 'ibuffer)

;; TODO if not a class, then how do you fallback to another column?
;; The name column is defined in ebuffer.el but I don't want to copy
;; it :S
(define-ibuffer-column php-class-name-col
  (:name "Class")
  (let ((name (buffer-name buffer)))
	(if (string-match "class.tx_\\(.*\\).php" name)
		(concat "*" (match-string 1 name) "*")
	  "")))

(setq ibuffer-formats
	  '((mark modified read-only " "
			  (name 18 18 :left :elide)
			  " "
			  (size 9 -1 :right)
			  " "
			  (mode 16 16 :left :elide)
			  " " filename-and-process)
		(mark modified read-only " "
			  (name 18 18 :left :elide)
			  " "
			  (php-class-name-col 30 30 :left :elide)
			  " "
			  (mode 16 16 :left :elide)
			  " " filename-and-process)
		(mark " "
			  (name 16 -1)
			  " " filename)))

(setq ibuffer-saved-filter-groups
	  '(("default"
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
		 ("wwoof-local"
		  (filename . "public_html/wwoof_community"))
		 ("wwoof-live"
		  (or (filename . "remote/live/wwoof_community")
			  (filename . "bgluteus_remote_dev/magenta/wwoof_community")))
		 ("quinyx"
		  (filename . "remote/dev/quinyx/"))
		 ("quinyx-live"
		  (filename . "remote/sslive/quinyx/"))
		 ("pulsteknik"
		  (filename . "remote/dev/pulsteknik/"))

		 ("gc_collectors_social-local"
		  (or (filename . "local/gc_collectors_social/")
			  (filename . "quinyx/typo3conf/ext/gc_collectors_social/")))

		 ("gc_veguestbook_hooks-local"
		  (or (filename . "local/gc_veguestbook_hooks/")
			  (filename . "quinyx/typo3conf/ext/gc_veguestbook_hooks/")))

		 ("gc_orienthus-local"
		  (or (filename . "public_html/orienthus/typo3conf/ext/gc_orienthus/")))

		 ("gc_timglaset-local"
		  (or (filename . "public_html/timglaset/typo3conf/ext/gc_timglaset/")))

		 ("timglaset-local"
		  (or (filename . "public_html/timglaset/")))

		 ("timglaset-remote"
		  (or (filename . "remote/ssdev/timglaset/")))

		 ("orienthus-local"
		  (or (filename . "public_html/orienthus/")))

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
		  (or (filename . "bgluteus_remote_dev/magenta_dev/core-magenta2")
			  (filename . "remote/dev/core-magenta2")))

		 ("ssfe-remote2"
		  (or (filename . "remote/ssdev/core-magenta2/")
			  (filename . "bgluteus_remote_dev/sitespace_dev/core-magenta2/")))

		 ("emacs-source"
		 (filename . "usr/share/emacs/"))

		 ;; Chrome sitespace / magenta extension
		 ("chrome-magenta-ext"
		  (or (filename . "dev/ext/chrome-magenta-ext/")))

		 ;; Some other projects
		 ("nevarforget"
		  (or (filename . "dev/nevarforget")))

		 ("booksurfing"
		  (or (filename . "dev/booksurfing")))

		 ("weback-live"
		  (filename . "bgluteus_remote_dev/sys/weback"))

		 ("weback-sysdev"
		  (filename . "bgluteus_remote_dev/sys_dev/weback"))

		 ("weback-local"
		  (filename . "public_html/weback"))

		 ("ucms-remote"
		  (filename . "bgluteus_remote_dev/sitespace_dev/ucms/"))

		 ("csr-local"
		  (filename . "public_html/csr"))

		 ("csr-remote"
		  (filename . "bgluteus_remote_dev/sitespace_dev/csr/"))

		 ("magit"
		  (name . "^\\*magit"))

		 ("dired"
		  (mode . dired-mode))

		 ("svn"
		  (name . "svn"))

		 ("sql"
		  (name . "*SQL*"))
		 )))

(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          (lambda ()
			(ibuffer-auto-mode 1)
			(ibuffer-switch-to-saved-filter-groups "default")))

(aj/define-keys ibuffer-mode-map
  '(("TAB" ibuffer-toggle-filter-group)
	("M-j" backward-char)
	("G" (lambda () (interactive) (ibuffer-switch-to-saved-filter-groups "default")))
	))

(define-key ibuffer-mode-map (kbd "TAB") 'ibuffer-toggle-filter-group)

(provide 'arvid-ibuffer)
