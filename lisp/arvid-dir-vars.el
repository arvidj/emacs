;;; TODO: When in gluteus live directory, switch color of modeline and
;;; set files as readonly.

(let ((tabs-okay '(remove 'tabs ethan-wspace-errors)))
  (dir-locals-set-class-variables
   'gluteus
   `((nil
      .
      ((indent-tabs-mode . t)
       (ethan-wspace-errors . ,tabs-okay)
       (tab-width . 4)))))

  (dir-locals-set-class-variables
   'booksurfing
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 4)
       (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))))))
  (dir-locals-set-class-variables
   'emacs
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 4)
       (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))))))
  (dir-locals-set-class-variables
   'enseirb-poo
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 2)
       (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 2)))))
  (dir-locals-set-class-variables
   'enserib-tpcpp
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 2)
       (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 2)))))
  (dir-locals-set-class-variables
   'php-zend
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 4)
       (ethan-wspace-errors . (tabs eol no-nl-eof many-nls-eof))
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 4)))))
  (dir-locals-set-class-variables
   'enseirb-projet-comp
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 4)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 4)))))
  (dir-locals-set-class-variables
   'enseirb-pfa
   '((nil
      .
      ((indent-tabs-mode . t)
       (tab-width . 4)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 4)
       (require-final-newline . nil)
       (compile-command . "cd build && cmake .. && make && cd ..")))))
  (dir-locals-set-class-variables
   'enseirb-projet-sysd
   '((nil
      .
      ((indent-tabs-mode . t)
       (tab-width . 2)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 2)
       (require-final-newline . nil)))))
  (dir-locals-set-class-variables
   'enseirb-projet-projres-java
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 4)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 4)
       (require-final-newline . nil)))))
  (dir-locals-set-class-variables
   'enseirb-projet-projres-c
   '((nil
      .
      ((indent-tabs-mode . t)
       (tab-width . 4)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 4)
       (require-final-newline . nil)))))
  (dir-locals-set-class-variables
   'weback '((nil . ((require-final-newline . nil)))))
  (dir-locals-set-class-variables
   'cea-c
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 2)
       (ws-trim-method-hook . (ws-trim-trailing))
       (c-basic-offset . 2)))))
  (dir-locals-set-class-variables
   'cea-ml
   '((nil
      .
      ((indent-tabs-mode . nil)
       (tab-width . 8)
       (ws-trim-method-hook . (ws-trim-trailing)))))))

(dir-locals-set-directory-class
 (expand-file-name "~/dev/chrome-magenta-ext") 'gluteus)
(dir-locals-set-directory-class
 (expand-file-name "~/dev/booksurfing") 'booksurfing)
(dir-locals-set-directory-class
 (expand-file-name "~/skola/ensierb/poo") 'enseirb-poo)

(dir-locals-set-directory-class
 (expand-file-name "~/skola/ensierb/poo/td3/src/tec/") 'enseirb-poo)

(dir-locals-set-directory-class
 (expand-file-name "~/skola/ensierb/poo/td3-chloe") 'enseirb-poo)
(dir-locals-set-directory-class
 (expand-file-name "~/skola/ensierb/poo/td") 'enseirb-poo)

(dir-locals-set-directory-class
 (expand-file-name
  "~/skola_nonpub/enseirb/compilation/projet-compilation-2013")
 'enseirb-projet-comp)

(dir-locals-set-directory-class
 (expand-file-name "~/public_html/projet-sgbd-2012") 'php-zend)

(dir-locals-set-directory-class
 (expand-file-name "~/Dropbox/TP_CPP/tp2/") 'enserib-tpcpp)

(dir-locals-set-directory-class
 (expand-file-name "~/dev/Muziko/MuzicoDefense/") 'enseirb-pfa)

(dir-locals-set-directory-class
 (expand-file-name "~/dev/MuzicoDefense//") 'enseirb-pfa)

(dir-locals-set-directory-class
 (expand-file-name "~/dev/MuzikoDefense/") 'enseirb-pfa)


(dir-locals-set-directory-class
 (expand-file-name "~/public_html/weback/") 'weback)


(dir-locals-set-directory-class
 (expand-file-name
  "~/dev/threadmanagerlibrary/")
 'enseirb-projet-sysd)


(dir-locals-set-directory-class
 (expand-file-name
  "~/dev/projres-908/router")
 'enseirb-projet-projres-java)

(dir-locals-set-directory-class
 (expand-file-name
  "~/dev/projres-908/controller")
 'enseirb-projet-projres-c)

(dir-locals-set-directory-class
 (expand-file-name "~/dev/e-acsl/tests/") 'cea-c)
(dir-locals-set-directory-class
 (expand-file-name "~/dev/e-acsl/share/") 'cea-c)
(dir-locals-set-directory-class
 (expand-file-name "~/dev/e-acsl-examples/") 'cea-c)

(dir-locals-set-directory-class
 (expand-file-name "~/dev/e-acsl/") 'cea-ml)

(provide 'arvid-dir-vars)
