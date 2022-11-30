(require 'mk-project)

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file-ido) ; or 
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

;; TODO: There should be a way to specify typo3 projects.  It should
;; be possible to specify what extensions to index and which to ignore. 
;; 
;; * It should not matter if the project dirs does not exist.
;; * Tags, open files list and files list should be stored in the same
;;   dir by default.
;; * Should be different read-only settings for different
;;   projects. some projects lock the core by default, some lock
;;   everything outside extension, etc

(project-def "goodiebag-dev"
	     '((basedir          "~/remote/dev/goodiebag/")
	       (src-patterns     ("*.php" "*.js" "*.css" "*.html" "*.xml" ))
	       (ignore-patterns  ("*.png" "*.jpg" "*.gif" "*.cache" "*CACHED*"))
           (ignore-dirs      
            ("typo3" "typo3temp" "typo3_src" 
             "typo3conf/ext/captcha"
             "typo3conf/ext/gc_gb.old"
             "typo3conf/ext/gc_gb.exp"
             "typo3conf/ext/kickstarter"
             "typo3conf/ext/news_author_rel"
             "typo3conf/ext/sms_firephp"
             ))

           (tags-cmd         "etags-php.sh --recurse=no")
	       (tags-file        "Users/arvidjakobsson/.projects/goodiebag-dev/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/goodiebag-dev/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/goodiebag-dev/open-files")
	       (vcs              svn)
	       (compile-cmd      "ant")
	       (ack-args         "--php")
	       (startup-hook     goodiebag-project-startup)
	       (shutdown-hook    nil)))

(project-def "goodiebag-live"
	     '((basedir          "~/remote/live/goodiebag/")
	       (src-patterns     ("*.php" "*.js" "*.css" "*.html" "*.xml" ))
	       (ignore-patterns  ("*.png" "*.jpg" "*.gif" "*.cache"))
           (ignore-dirs      
            ("typo3" "typo3temp" "typo3_src" "fileadmin.exp"
             "typo3conf/ext/captcha"
             "typo3conf/ext/gc_gb.old"
             "typo3conf/ext/kickstarter"
             "typo3conf/ext/news_author_rel"
             "typo3conf/ext/sms_firephp"
             ))

	       (tags-cmd         "etags-php.sh --recurse=no")
	       (tags-file        "Users/arvidjakobsson/.projects/goodiebag-live/TAGS")
		   (file-list-cache	 "Users/arvidjakobsson/.projects/goodiebag-live/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/goodiebag-live/open-files")
	       (vcs              svn)
	       (compile-cmd      "ant")
	       (ack-args         "--php")
	       (startup-hook     goodiebag-project-startup)
	       (shutdown-hook    nil)))

(project-def "framtidensmat-dev"
	     '((basedir          "~/remote/dev/framtidensmat/")
	       (src-patterns     ("*.php" "*.js" "*.css" "*.html" "*.xml" ))
	       (ignore-patterns  ("*.png" "*.jpg" "*.gif" "*.cache"))
           (ignore-dirs      
            ("typo3" "typo3temp" "typo3_src" "_old" "uploads"
             "typo3conf/ext/captcha"
             "typo3conf/ext/gc_fm.bak"
             "typo3conf/ext/gsi_feuser_list"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/kickstarter"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/_old"
             "typo3conf/ext/simulatebe"
             "typo3conf/ext/sms_firephp"
             "typo3conf/l10n"
             ))

	       (tags-file        "Users/arvidjakobsson/.projects/framtidensmat-dev/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/framtidensmat-dev/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/framtidensmat-dev/open-files")
	       (vcs              svn)
	       (compile-cmd      "ant")
	       (ack-args         "--php")
	       (startup-hook     nil)
		   (shutdown-hook    nil)))



(defun goodiebag-project-startup ()
  (setq c-basic-offset 4))

(project-def "lsh"
	     '((basedir          "~/skola/opsys/lab1/lsh")
	       (src-patterns     ("*.c" "*.h" "Makefile"))
	       (ignore-patterns  ("*.out"))
	       (tags-cmd         nil)
	       (tags-file        "Users/arvidjakobsson/.projects/lsh/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/lsh/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/lsh/open-files")
	       (vcs              git)
	       (compile-cmd      "make -k")
	       (ack-args         "--cc")
	       (startup-hook     lsh-project-startup)
	       (shutdown-hook    nil)))

(defun lsh-project-startup ()
  (find-file "~/skola/opsys/lab1/lsh/lsh.c"))

(project-def "mostcommon"
	     '((basedir          "~/programmering/mostcommon/")
	       (src-patterns     ("*.c" "*.h" "Makefile"))
	       (ignore-patterns  ("*.out"))
	       (tags-file        "Users/arvidjakobsson/.projects/mostcommon/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/mostcommon/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/mostcommon/open-files")
	       (vcs              git)
	       (compile-cmd      "make -k")
	       (ack-args         "--cc")
	       (startup-hook     mostcommon-project-startup)
	       (shutdown-hook    nil)))

(defun mostcommon-project-startup ()
  (find-file "~/programmering/mostcommon/mostcommon.c"))

(project-def "wwoof-dev"
	     '((basedir          "~/remote/dev/wwoof_community/")
	       (src-patterns     ("*.php" "*.js" "*.css" "*.html" "*.xml" "*.txt"))
		   (ignore-patterns  ("*.png" "*.jpg"))
           (ignore-dirs      
            ("typo3" "typo3temp" "typo3_src" 
             "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
             "typo3conf/ext/sms_firephp"
             ))

		   (tags-file        "Users/arvidjakobsson/.projects/wwoof-dev/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/wwoof-dev/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/wwoof-dev/open-files")
	       (vcs              svn)
	       (compile-cmd      "ant")
	       (ack-args         "--cc")
		   (shutdown-hook    nil)))

(project-def "wwoof-local"
		 '((basedir          "~/public_html/wwoof_community/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
           (ignore-dirs      
			("typo3temp"
			 "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
			 "typo3conf/ext/sms_firephp"))

		   (tags-file        "/Users/arvidjakobsson/.projects/wwoof-local/TAGS")
	       (file-list-cache  "/Users/arvidjakobsson/.projects/wwoof-local/files")
		   (open-files-cache "/Users/arvidjakobsson/.projects/wwoof-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))

(project-def "framtiden-local"
		 '((basedir          "~/public_html/framtidensmat/")
		   (src-patterns     ("*.php"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
           (ignore-dirs      
			("typo3temp"
			 "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
			 "typo3conf/ext/sms_firephp"))

		   (tags-file        "Users/arvidjakobsson/.projects/framtiden-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/framtiden-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/framtiden-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
	       (shutdown-hook    nil)))

(project-def "javalette"
	     '((basedir          "~/skola/compcons/javalette/")
	       (src-patterns     ("*.hs" "*.x" "*.y" "*.cf"))
		   (ignore-patterns  ("*.hi" "*.o"))
		   (tags-file        "Users/arvidjakobsson/.projects/javalette/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/javalette/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/javalette/open-files")
	       (vcs              git)
	       (compile-cmd      "make")
	       (ack-args         "--haskell")
	       (shutdown-hook    nil)))

(project-def "sitespace-local"
		 '((basedir          "~/public_html/core-magenta-2.0-git/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
		   (ignore-dirs ())
		   (tags-file        "Users/arvidjakobsson/.projects/sitespace-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/sitespace-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/sitespace-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))

(project-def "weback-local"
		 '((basedir          "~/public_html/weback/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
		   (ignore-dirs ())
		   (tags-file        "Users/arvidjakobsson/.projects/weback-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/weback-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/weback-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))

(project-def "emacs"
	     '((basedir          "~/.emacs.d/")
	       (src-patterns     ("*.el" "*.yasnippet"))
		   (ignore-patterns  ("*.elc"))
		   (tags-file        "Users/arvidjakobsson/.projects/emacs/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/emacs/files")
	       (open-files-cache "Users/arvidjakobsson/.projects/emacs/open-files")
	       (vcs              git)
	       (compile-cmd      "make")
	       (ack-args         "--type-add elisp=.el --elisp")
		   (shutdown-hook    nil)))

(project-def "timglaset-local"
		 '((basedir          "~/public_html/timglaset/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
           (ignore-dirs      
			("typo3temp"
			 "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
			 "typo3conf/ext/sms_firephp"))

		   (tags-file        "Users/arvidjakobsson/.projects/timglaset-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/timglaset-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/timglaset-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))

(project-def "goodiebag-local"
		 '((basedir          "~/public_html/goodiebag/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
           (ignore-dirs      
			("typo3temp"
			 "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
			 "typo3conf/ext/sms_firephp"))

		   (tags-file        "Users/arvidjakobsson/.projects/goodiebag-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/goodiebag-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/goodiebag-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))

(project-def "csr-local"
		 '((basedir          "~/public_html/csr/")
		   (src-patterns     ("*.php" "*.js"))
		   (ignore-patterns  ("*.png" "*.jpg" "*.cache" "*.JPG" "*.gif" "*.db" "*.pdf"))
           (ignore-dirs      
			("typo3temp"
			 "typo3conf/ext/captcha"
             "typo3conf/ext/gc_femail"
             "typo3conf/ext/kb_md5fepw"
             "typo3conf/ext/llxmltranslate"
             "typo3conf/ext/mm_forum"
             "typo3conf/ext/newloginbox"
			 "typo3conf/ext/sms_firephp"))

		   (tags-file        "Users/arvidjakobsson/.projects/csr-local/TAGS")
	       (file-list-cache  "Users/arvidjakobsson/.projects/csr-local/files")
		   (open-files-cache "Users/arvidjakobsson/.projects/csr-local/open-files")
		   (vcs              git)
	       (compile-cmd      "ant")
		   (ack-args         "")
		   (shutdown-hook    nil)))


(defun ack-dir (&optional phrase from-current-dir)
	"Run ack from project's basedir, using the `ack-args' configuration.
With C-u prefix, start ack from the current directory."
	(interactive)
	(let* ((wap (word-at-point))
		   (regex (or phrase
					  (if wap (read-string (concat "Ack dir for (default \"" wap "\"): ") nil nil wap)
						(read-string "Ack dir for: "))))
		   (whole-cmd (concat (mk-proj-ack-cmd regex) " | cut -b1-1000"))
		   (confirmed-cmd (read-string "Ack command: " whole-cmd nil whole-cmd))
		   (default-directory (read-file-name "Relative dir: " default-directory "" t)))
	  (compilation-start confirmed-cmd 'ack-mode)))
