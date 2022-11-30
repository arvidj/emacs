(require 'arvid-mr)
(require 'arvid-worklog)

(defun nl-select-gitlab-ident ()
  ""
  (interactive)
  
  (let* ((name (apply-partially 'nth 2))
         (gitlab-ident (apply-partially 'nth 3))
         (person-table (with-temp-buffer
                         (insert-file-contents "~/Dropbox/Jobb/Nomadic_Labs/people2.org")
                         (cddr (org-table-to-lisp))))
         (user (ido-completing-read "User: " (mapcar name person-table)))
         (data (alist-get
                user
                (mapcar (lambda (x)
                          (cons (funcall name x)
                                (funcall gitlab-ident x)))
                        ;; (cons (nth 2 x) (gitlab-ident x))))
                        person-table)
                nil nil 'string=))
         ) (insert data)))

(provide 'arvid-nomadic-labs)
