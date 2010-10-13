;; Add plugins
(let ((plugins '("magit" "timeclock-x" "emacs-soap-client" "auto-complete"
				 "auto-complete-etags" "zencoding" "hs-lint" "yasnippet"
				 "mk-project" "remember-el" "org-mode" "color-theme"
				 "color-theme/themes" "color-theme-tango-2" "twilight-emacs"
				 "php-mode" "wrap-region" "ts-mode" "espresso"
				 "highlight-parentheses" "dsvn" "browse-kill-ring" "rainbow"
				 "autopair" "color-theme-tangotango" "color-theme-zenburn"
				 "smex" "drag-stuff" "anything" "anything-config" "iedit"
				 "etest" "fic-mode")))
  (dolist (plugin plugins)
	(add-to-list 'load-path (concat "~/.emacs.d/plugins/" plugin))))

(provide 'arvid-loadpaths)
