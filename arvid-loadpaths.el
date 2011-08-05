;; TODO there are smarter ways of doing this, see
;; http://stackoverflow.com/questions/221365/emacs-lisp-how-to-add-a-folder-and-all-its-first-level-sub-folders-to-the-load-p

;; Add plugins
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (let (
;;       (plugins '("magit" "timeclock-x" "emacs-soap-client" "auto-complete"
;; 				 "auto-complete-etags" "zencoding" "hs-lint" "yasnippet"
;; 				 "mk-project" "remember-el" "org-mode" "color-theme"
;; 				 "color-theme/themes" "color-theme-tango-2" "twilight-emacs"
;; 				 "php-mode" "wrap-region" "ts-mode" "espresso"
;; 				 "highlight-parentheses" "dsvn" "browse-kill-ring" "autopair"
;; 				 "color-theme-tangotango" "color-theme-zenburn"
;; 				 "smex" "drag-stuff" "anything" "anything-config" "iedit"
;; 				 "etest" "fic-mode" "command-frequency" "arvid-google-translate"
;; 				 "ethan-wspace/lisp" "ws-trim" "js-comint" "sublain"
;; 				 "mozrepl/chrome/content" "php-doc" "tree-mode" "windata"
;; 				 "fill-column-indicator" "loadstack" 
;; 				 )))
;;   (dolist (plugin plugins)
;; 	(add-to-list 'load-path (concat "~/.emacs.d/plugins/" plugin))))

(provide 'arvid-loadpaths)


