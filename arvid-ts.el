;; TODO: It would be nice to have a mode for trying out TS.
;; TODO: "//" is not a comment in typoscript, but /* */ is.


;;; TypoScript-mode
(autoload 'ts-mode "ts-mode")

(let ((ts-files-re (regexp-opt '(".ts"
								 "/ext_typoscript_setup.txt"
								 "/ext_typoscript_constants.txt"
								 "/tsconfig_tinymce_common.txt"
								 "/tsconfig_tinymce_fe.txt"
								 "/tsconfig_tinymce_be.txt"
								 "/typoscript_setup.txt"
								 "/tsconfig_page.txt"
								 "/fileadmin/templates/typoscript/"))))
  ;; What is up with the back tick monstrosity :(
  (add-to-list 'auto-mode-alist `(,ts-files-re . ts-mode)))

(add-hook 'ts-mode-hook 'my-ts-mode-hook)

(defun my-ts-mode-hook ()
  (setq ts-newline-function 'newline)
  (setq tab-width 2)
  (c-subword-mode))

(defun typo3-clear-cache ()
  (interactive)


  (let ((local (directory-files dir))
		(remote (find-remote-installations)))

	))

  ;; (let (let (let
  ;; (shell-command "php /home/arvid/public_html/goodiebag/typo3/cli_dispatch.phpsh gc_system cache 2> /dev/null")
  ;; ;; (shell-command "php /home/arvid/public_html/timglaset/typo3/cli_dispatch.phpsh gc_system cache 2> /dev/null")
  ;; ;; (shell-command
  ;; ;;  (concat "ssh b \"/usr/bin/php magenta/collector/typo3/cli_dispatch.phpsh gc_system cache 2> /dev/null\""))
  ;; )

(defun find-remote-installations ()
  ""
  (interactive)
  ;; (save-window-excursion
  (let ((output-buffer "*find-remote-installations*"))
	(shell-command "ssh b ls magenta magenta_dev sitespace sitespace_dev"
				   output-buffer)
	(switch-to-buffer output-buffer)
	(let ((output (buffer-string)))
	  (message output)
	  (bury-buffer)
	  )))
;; )

(defun find-typo3-root-dir (&optional root)
  "find-typo3-root-dir shall traverse the file tree upwards until an
typo3-root dir is found. If it is found, it is returned, otherwise
nil."
  (let ((root (or root default-directory)))
	(cond
	 ;; Is this root?
	 ((typo3-root-dir-p root)
	  (directory-file-name (expand-file-name root)))
	 ;; If at /, quitp
	 ((equal (expand-file-name root) "/") nil)
	 ;; Otherwise go upwards
	 (t (find-typo3-root-dir (arvid-parent-dir root))))))

(defun typo3-root-dir-p (dir)
  "Returns t if `DIR` is the root directory of a TYPO3 installation."

  ;; This could be solved with an intersection operation, but I don't know
  ;; how to set the equailty predicate.
  ;;
  ;; In haskell I would use (and (map typo3-files (\x -> x `memberOf` directory-files)))
  ;; or something similar.
  (let ((files (directory-files dir))
		(match t))
	(dolist (i '("typo3" "global_localconf.php" "t3lib"))
	  (unless (member i files) (setq match nil)))
	match))

(defun find-typo3-site-root-dir (&optional root)
  "find-typo3-root-dir shall traverse the file tree upwards until an
typo3-root dir is found. If it is found, it is returned, otherwise
nil."
  (let ((root (or root default-directory)))
	(cond
	 ;; Is this root?
	 ((typo3-site-root-dir-p root)
	  (directory-file-name (expand-file-name root)))
	 ;; If at /, quitp
	 ((equal (expand-file-name root) "/") nil)
	 ;; Otherwise go upwards
	 (t (find-typo3-site-root-dir (arvid-parent-dir root))))))

(defun typo3-site-root-dir-p (dir)
  "Returns t if `DIR` is the root directory of a TYPO3 installation."

  ;; This could be solved with an intersection operation, but I don't know
  ;; how to set the equailty predicate.
  ;;
  ;; In haskell I would use (and (map typo3-files (\x -> x `memberOf` directory-files)))
  ;; or something similar.
  (let ((files (directory-files dir))
		(match t))
	(dolist (i '("typo3conf" "typo3" "t3lib" "index.php"))
	  (unless (member i files) (setq match nil)))
	match))

(defun find-typo3-extension-root-dir (&optional root)
  "find-typo3-root-dir shall traverse the file tree upwards until an
typo3-root dir is found. If it is found, it is returned, otherwise
nil."
  (let ((root (or root default-directory)))
	(cond
	 ;; Is this root?
	 ((typo3-extension-root-dir-p root)
	  (directory-file-name (expand-file-name root)))
	 ;; If at /, quitp
	 ((equal (expand-file-name root) "/") nil)
	 ;; Otherwise go upwards
	 (t (find-typo3-extension-root-dir (arvid-parent-dir root))))))

(defun typo3-extension-root-dir-p (dir)
  ""
  (interactive)
  (member
   (file-name-nondirectory (arvid-parent-dir dir))
   '("ext" "sysext")))

(defun create-test-class-for-typo3-class ()
  (interactive)
  ;; Find extension root.

  ;; Go up until grand-parent dir is typo3/ext/ / typo3conf/ext/ or
  ;; typo3/sysext.

  (let* ((name (buffer-file-name))
		 (root (find-typo3-extension-root-dir))
		 (class-name (progn (string-match "/class.\\(.*\\).php$" name)
							(match-string 1 name)))
		 (test-file (concat root "/Tests/class." class-name "_Test.php")))
	(pop-to-buffer (find-file-noselect test-file))))

(defun arvid-parent-dir (dir)
  (directory-file-name (file-name-directory (expand-file-name dir))))


;; Given a site, return path to all extensions.
;; Should return structure ((name => 'asdf, type => 'local/global/sys', path => '')
(defun typo3-extensions (site-dir)
  (let* ((ext-dirs '("typo3conf/ext" "typo3/ext" "typo3/sysext"))
		 (exts (mapcan (lambda (ext-dir) (directory-files (concat site-dir "/" ext-dir))) ext-dirs))
		 (message exts)
		))
  )




;; (defun textmate-find-project-root (&optional root)
;;   "Determines the current project root by recursively searching for an indicator."
;;   (when (null root) (setq root default-directory))
;;   (cond
;;    ((root-matches root *textmate-project-roots*)
;; 		(expand-file-name root))
;;    ((equal (expand-file-name root) "/") nil)
;;    (t (textmate-find-project-root (concat (file-name-as-directory root) "..")))))


;; (defun typo3-clear-cache (dir)
;; (interactive "MProject directory: ")
;; (message dir)
;; (shell-command (concat "ssh b /usr/bin/php " dir "/typo3/cli_dispatch.phpsh gc_system cache")))

  ;; (shell-command (concat "php /home/arvid/public_html/labs/typo3/cli_dispatch.phpsh gc_system cache"))


  ;; Quinyx remote
  ;; (shell-command "ssh bdev \"/usr/bin/php magenta_dev/quinyx/typo3/cli_dispatch.phpsh gc_system cache 2> /dev/null\"")

  ;; Goodiebag local
  ;; (shell-command "php /home/arvid/public_html/goodiebag/typo3/cli_dispatch.phpsh gc_system cache")

  ;; Quinyx local



;; (shell-command (concat "ssh b \"/usr/bin/php magenta_dev/orienthus/typo3/cli_dispatch.phpsh gc_system cache\"")))
;; (shell-command (concat "php /home/arvid/public_html/goodiebag/typo3/cli_dispatch.phpsh gc_system cache"))
;; (shell-command (concat "ssh bdev \"/usr/bin/php magenta_dev/wwoof_community/typo3/cli_dispatch.phpsh gc_system cache 2> /dev/null\"")))

(provide 'arvid-ts)
