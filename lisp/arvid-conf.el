(let ((conf-files-re (regexp-opt '("/.htaccess" "/_.htaccess" "/.gitconfig" "/.gitmodules"))))
  (add-to-list 'auto-mode-alist `(,conf-files-re . conf-mode)))
(provide 'arvid-conf)
