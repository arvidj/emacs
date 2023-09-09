;; The firephp-chrome projects does not use tabs for indentation.

(dir-locals-set-class-variables
 'firephp-chrome '((nil . ((indent-tabs-mode . nil)))))

(dir-locals-set-directory-class
 (expand-file-name "~/dev/firephp-chrome") 'firephp-chrome)
