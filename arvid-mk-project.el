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
	       (tags-file        "/home/arvid/.projects/goodiebag-dev/TAGS")
	       (file-list-cache  "/home/arvid/.projects/goodiebag-dev/files")
	       (open-files-cache "/home/arvid/.projects/goodiebag-dev/open-files")
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
	       (tags-file        "/home/arvid/.projects/goodiebag-live/TAGS")
		   (file-list-cache	 "/home/arvid/.projects/goodiebag-live/files")
		   (open-files-cache "/home/arvid/.projects/goodiebag-live/open-files")
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

	       (tags-file        "/home/arvid/.projects/framtidensmat-dev/TAGS")
	       (file-list-cache  "/home/arvid/.projects/framtidensmat-dev/files")
	       (open-files-cache "/home/arvid/.projects/framtidensmat-dev/open-files")
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
	       (tags-file        "/home/arvid/.projects/lsh/TAGS")
	       (file-list-cache  "/home/arvid/.projects/lsh/files")
	       (open-files-cache "/home/arvid/.projects/lsh/open-files")
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
	       (tags-file        "/home/arvid/.projects/mostcommon/TAGS")
	       (file-list-cache  "/home/arvid/.projects/mostcommon/files")
	       (open-files-cache "/home/arvid/.projects/mostcommon/open-files")
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

		   (tags-file        "/home/arvid/.projects/wwoof-dev/TAGS")
	       (file-list-cache  "/home/arvid/.projects/wwoof-dev/files")
	       (open-files-cache "/home/arvid/.projects/wwoof-dev/open-files")
	       (vcs              svn)
	       (compile-cmd      "ant")
	       (ack-args         "--cc")
	       (shutdown-hook    nil)))

(project-def "javalette"
	     '((basedir          "~/skola/compcons/javalette/")
	       (src-patterns     ("*.hs" "*.x" "*.y" "*.cf"))
		   (ignore-patterns  ("*.hi" "*.o"))
		   (tags-file        "/home/arvid/.projects/javalette/TAGS")
	       (file-list-cache  "/home/arvid/.projects/javalette/files")
	       (open-files-cache "/home/arvid/.projects/javalette/open-files")
	       (vcs              git)
	       (compile-cmd      "make")
	       (ack-args         "--haskell")
	       (shutdown-hook    nil)))


(project-def "emacs"
	     '((basedir          "~/.emacs.d/")
	       (src-patterns     ("*.el" "*.yasnippet"))
		   (ignore-patterns  ("*.elc"))
		   (tags-file        "/home/arvid/.projects/emacs/TAGS")
	       (file-list-cache  "/home/arvid/.projects/emacs/files")
	       (open-files-cache "/home/arvid/.projects/emacs/open-files")
	       (vcs              git)
	       (compile-cmd      "make")
	       (ack-args         "--type-add elisp=.el --elisp")
	       (shutdown-hook    nil)))
