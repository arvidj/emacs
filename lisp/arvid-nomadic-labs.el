(defun aj/nl-select-gitlab-ident ()
  ""
  (interactive)

  (let* ((name (apply-partially 'nth 2))
         (gitlab-ident (apply-partially 'nth 3))
         (person-table
          (with-temp-buffer
            (insert-file-contents
             "~/Dropbox/Jobb/Nomadic_Labs/people2.org")
            (cddr (org-table-to-lisp))))
         (user (completing-read "User: " (mapcar name person-table)))
         (data
          (alist-get user
                     (mapcar
                      (lambda (x)
                        (cons
                         (funcall name x) (funcall gitlab-ident x)))
                      ;; (cons (nth 2 x) (gitlab-ident x))))
                      person-table)
                     nil nil 'string=)))
    (insert data)))

;; (defun aj/besten-gl-jobs-sqlite (read-write)
;;   ""
;;   (interactive "P")
;;   (print read-write)
;;   (let
;;       ((default-directory
;;         "/ssh:besten_local:/home/arvid/dev/nomadic-labs/gitlab-job-scraper/master/db")
;;        (sql-database
;;         "/home/arvid/dev/nomadic-labs/gitlab-job-scraper/master/db/gl-2023-05-01--2023-06-26.db")
;;        (sql-sqlite-options
;;         (if read-write
;;             sql-sqlite-options
;;           (cons "--readonly" sql-sqlite-options))))
;;     (sql-sqlite)))


(defun aj/besten-gl-jobs-sqlite (read-write)
  ""
  (interactive "P")
  (print read-write)
  (let
      ((default-directory
        "/ssh:besten_local:/home/arvid/dev/nomadic-labs/gitlab-job-scraper/master/db")
       (sql-database
        "/home/arvid/dev/nomadic-labs/gitlab-job-scraper/master/db/gl-2023-05-01--2023-06-26.db")
       (sql-sqlite-options
        (if read-write
            sql-sqlite-options
          (cons "--readonly" sql-sqlite-options))))
    (sql-sqlite)))

(provide 'arvid-nomadic-labs)
