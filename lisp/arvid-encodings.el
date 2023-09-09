;; ställ in encoding automatiskt i fajitamappen
(add-to-list
 'auto-mode-alist
 (cons
  (expand-file-name "~/Dropbox/FajitaBoy/")
  (lambda ()
    (interactive)
    (set-buffer-file-coding-system 'iso-latin-1-dos))))
