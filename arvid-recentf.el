(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)


(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-x M-r") 'ido-find-file-read-only)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read
			   "Choose recent file: "
			   (collapse-file-path-list recentf-list)
			   nil
			   t)))
	(when file
      (find-file file))))

(defun collapse-file-path-list (list)
  "Replaces dir with ~ in a LIST of file paths."
  (mapcar
   (lambda (path) (strip-prefix path "/home/arvid" "~"))
   list))

(defun strip-prefix (string prefix &optional replacement)
  "Strips PREFIX from STRING, optionally replaces it with REPLACEMENT."
  (replace-regexp-in-string 
   (concat "^" (regexp-quote prefix))
   (or replacement "")
   string))

(provide 'arvid-recentf)
