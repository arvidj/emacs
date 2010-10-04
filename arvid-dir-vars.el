;;; TODO: When in gluteus live directory, switch color of modeline and
;;; set files as readonly.

(dir-locals-set-class-variables
 'gluteus
 '((nil . ((indent-tabs-mode t)
           (default-tab-width . 4)))))

(dir-locals-set-directory-class 
 (expand-file-name "~/dev/chrome-magenta-ext") 'gluteus)
