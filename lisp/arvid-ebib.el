(global-set-key "\C-ce" 'ebib)
(org-add-link-type "ebib"
                   'ebib-open-org-link
                   (lambda (path desc format)
                     (cond
                      ((eq format 'html)
                       (format "(<cite>%s</cite>)" path))
                      ((eq format 'latex)
                       (if (or (not desc) (equal 0 (search "cite:" desc)))
                           (format "\\cite{%s}" path)
                         (format "\\cite[%s][%s]{%s}"
                                 (cadr (split-string desc ";"))
                                 (car (split-string desc ";"))  path))))))

(org-add-link-type 
 "cite" 'ebib-open-org-link
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))


(provide 'arvid-ebib)
