(add-to-list 'rst-mode-hook 'arvid-rst/rst-mode-hook)

(defun arvid-rst/rst-mode-hook ()
  ""
  (interactive)
  (define-key rst-mode-map (kbd "C-c C-t h") 'rst-make-header)
  ;; use regular compile
  (define-key rst-mode-map (kbd "C-c C-c C-c") 'compile)

  (define-key
   rst-mode-map (kbd "C-c C-c C-p") 'arvid-rst/tezos-docs-open)

  (let* ((file (buffer-file-name))
         (root (locate-dominating-file file "conf.py")))
    (when (and file root))
    (setq
     compile-command
     (concat
      "cd "
      root
      " && PYENV=tezos ~/dev/nomadic-labs/tezos/scripts/sphinx-build.sh "
      file
      " DUMMY"))))

(defun arvid-rst/tezos-docs-open ()
  ""
  (interactive)
  (browse-url
   (concat
    "file://"
    (f-swap-ext
     (s-replace "/docs/" "/docs/_build/" buffer-file-name) "html"))))

(defun arvid-rst/make-header (char)
  ""
  (interactive (progn
                 (let* ((default
                         (save-excursion
                           (forward-line)
                           (beginning-of-line)
                           (if (looking-at "\"+\\|=+\\|-+\\|~+\n")
                               (format " (default: '%c')"
                                       (char-after))
                             ""))))
                   (list
                    (read-char
                     (format "Header character%s: " default))))))
  (save-excursion
    (end-of-line)
    (let ((len (current-column)))
      (forward-line)
      (beginning-of-line)
      (if (looking-at "\"+\\|=+\\|-+\\|~+\n")
          (progn
            (end-of-line)
            (insert
             (s-repeat
              (- len (current-column))
              (substring (match-string 0) 0 1))))
        (open-line 1)
        (insert (s-repeat len (char-to-string char)))))))


(defun arvid-rst/check-links ()
  ""
  (interactive)
  (compile
   (format "cd %s/docs ; linkchecker -r1 %s"
           (projectile-project-root)
           (string-replace
            (concat (projectile-project-root) "docs/") "_build/"
            (f-swap-ext buffer-file-name "html")))))

(provide 'arvid-rst)
