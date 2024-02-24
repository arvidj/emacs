;; Colorize in compilation buffers
(use-package
 ansi-color
 :config
 (defun colorize-compilation-buffer ()
   (ansi-color-apply-on-region compilation-filter-start (point)))
 (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(provide 'arvid-compile)
