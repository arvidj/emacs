(require 'recentf)
(recentf-mode 1)


(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-x M-r") 'ido-find-file-read-only)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  ;; TODO: Map over recentf-list strip home dir prefix. Then re-add
  ;; it. I did some similar work in one of the project plugins.
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))
