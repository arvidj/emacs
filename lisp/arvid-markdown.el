(require 'f)

(setq arvid-last-screenshot-name nil)
(defun arvid-markdown-fetch-latest-screenshot (new-name)
  (interactive (list (read-string "Filename: " arvid-last-screenshot-name)))
  ;; 1) Prompt name (def: last-name + 1), rename and move it to image folder
  ""
  (setq arvid-last-screenshot-name new-name)
  (let* ((latest (aj-fetch-latest "/Users/arvidjakobsson/Pictures/screenshots"))
		 (root (locate-dominating-file (buffer-file-name) "composer.json"))
		 (new-path (f-join root "docs/img" (concat new-name "." (f-ext latest))))
		 (new-path-rel (file-relative-name new-path root))
		 (root-rel (file-relative-name (f-join root "docs") (f-dirname buffer-file-name))))

	;; 2) Find last screen shot
	(copy-file latest new-path)

	;; 3) Pop open image
	(display-buffer (find-file-noselect new-path) t)

	;; 4) Insert image code
	(insert (concat "![](" (f-join root-rel new-path-rel) ")"))
	))

(defun aj-fetch-latest (path)
  (let ((e (f-entries path)))
	(car (sort e (lambda (a b)
				   (not (time-less-p (aj-mtime a)
									 (aj-mtime b))))))))

(defun aj-atime (f) (let ((attrs (file-attributes f)) (nth 4 attrs))))
(defun aj-mtime (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))
(defun aj-ctime (f) (let ((attrs (file-attributes f))) (nth 8 attrs)))

(defun arvid-markdown-mode-hook ()
	""
  (interactive)
  (flyspell-mode)
  ;; (with-timeout (2) (flyspell-buffer))

  (make-local-variable 'electric-pair-pairs)
  (add-to-list 'electric-pair-pairs '(?\` . ?\`))
  (add-to-list 'electric-pair-pairs '(?* . ?*))

  (define-key markdown-mode-map (kbd "M-n") nil)
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key markdown-mode-map (kbd "C-c ;") 'nl-select-gitlab-ident)
  (define-key markdown-mode-map (kbd "C-c C-;") 'nl-select-gitlab-ident)


  (let ((css-base64-url (concat "data:text/css;base64,"
                             (base64-encode-string "@import url(https://fonts.googleapis.com/css?family=Inconsolata|Inter&display=swap);body{margin:40px auto;max-width:900px;line-height:1.6;font-size:16px;color:#444;padding:0 10px;font-family:Inter,sans-serif}h1,h2,h3{line-height:1.2;font-family:Inter,sans-serif}img{width:700px;border-radius:10px}pre{font-family:Inconsolata,monospace}::selection{color:#fff;background:#ff4081}" t))))
    (setq markdown-command (concat "pandoc -s --css=" css-base64-url " -f markdown -t html --metadata pagetitle=\""
                                   (buffer-name)
                                   "\"")))
  )

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-to-list 'markdown-mode-hook 'arvid-markdown-mode-hook)

(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun arvid-markdown-cheat-sheet ()
	""
  (interactive)
  (dump-url "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet"))

(provide 'arvid-markdown)
