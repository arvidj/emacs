(use-package f :ensure t)

(defun aj/markdown-insert-backtick-gfm ()
  ""
  (interactive)
  (insert "```\n\n")
  (insert "```\n")
  (forward-line -2))

(defun aj/markdown-mode-hook ()
  ""
  (interactive)
  ;; (flyspell-mode)
  ;; (with-timeout (2) (flyspell-buffer))

  (make-local-variable 'electric-pair-pairs)
  (add-to-list 'electric-pair-pairs '(?\` . ?\`))
  (add-to-list 'electric-pair-pairs '(?* . ?*))

  (define-key markdown-mode-map (kbd "M-n") nil)
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key
   markdown-mode-map (kbd "C-c ;") 'aj/nl-select-gitlab-ident)
  (define-key
   markdown-mode-map (kbd "C-c C-;") 'aj/nl-select-gitlab-ident)

  (define-key
   markdown-mode-map (kbd "C-c `") 'aj/markdown-insert-backtick-gfm)

  (let
      ((css-base64-url
        (concat
         "data:text/css;base64,"
         (base64-encode-string
          "@import url(https://fonts.googleapis.com/css?family=Inconsolata|Inter&display=swap);body{margin:40px auto;max-width:900px;line-height:1.6;font-size:16px;color:#444;padding:0 10px;font-family:Inter,sans-serif}h1,h2,h3{line-height:1.2;font-family:Inter,sans-serif}img{width:700px;border-radius:10px}pre{font-family:Inconsolata,monospace}::selection{color:#fff;background:#ff4081}"
          t))))
    (setq markdown-command
          (concat
           "pandoc -s --css="
           css-base64-url
           " -f markdown -t html --metadata pagetitle=\""
           (buffer-name)
           "\""))))

(defun aj/markdown-cheat-sheet ()
  ""
  (interactive)
  (dump-url
   "https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet"))

(use-package
 markdown-mode
 :ensure t
 :config

 (add-to-list 'markdown-mode-hook 'aj/markdown-mode-hook)
 (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
 (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
 (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(provide 'arvid-markdown)
