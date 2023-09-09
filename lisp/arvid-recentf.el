(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 1000)


(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-x M-r") 'ido-find-file-read-only)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file
         (ido-completing-read
          "Choose recent file: "
          (collapse-file-path-list recentf-list)
          nil
          t)))
    (when file
      (find-file file))))

(defun collapse-file-path-list (list)
  "Replaces dir with ~ in a LIST of file paths."
  (mapcar
   (lambda (path)
     (strip-prefix path "/home/\\(arvidj\\|arvid\\)/" "~/" nil))
   list))

(defun strip-prefix (string prefix &optional replacement quote)
  "Strips PREFIX from STRING, optionally replaces it with REPLACEMENT."
  (replace-regexp-in-string
   (concat
    "^"
    (if quote
        (regexp-quote prefix)
      prefix))
   (or replacement "") string))

;; auto-save recentf list every 2 minutes
(run-at-time
 nil (* 2 60)
 (lambda ()
   (let ((inhibit-message t))
     (recentf-save-list))))

(provide 'arvid-recentf)
