;;; TODO: When in gluteus live directory, switch color of modeline and
;;; set files as readonly.

(let ((tabs-okay '(remove 'tabs ethan-wspace-errors)))
  (dir-locals-set-class-variables
   'gluteus
   `((nil . ((indent-tabs-mode . t)
			 (ethan-wspace-errors . ,tabs-okay)
			 (tab-width . 4)))))

  (dir-locals-set-class-variables
   'booksurfing
   '((nil . ((indent-tabs-mode . nil)
			 (tab-width . 4)
			 (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))))))
  (dir-locals-set-class-variables
   'emacs
   '((nil . ((indent-tabs-mode . nil)
			 (tab-width . 4)
			 (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))))))
)

(dir-locals-set-directory-class 
 (expand-file-name "~/dev/chrome-magenta-ext") 'gluteus)
(dir-locals-set-directory-class
 (expand-file-name "~/dev/booksurfing") 'booksurfing)
