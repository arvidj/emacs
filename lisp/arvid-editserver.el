;;; Turned the server off since the server directory is unsafe, unsure
;;; how to fix that.

;; For "Edit with emacs" in chrome
;; (add-to-list 'load-path "~/.emacs.d")
(when (require 'edit-server nil t)
  (unless (process-status "edit-server")
    (edit-server-start)))

(defun aj/edit-server-edit-mode-hook ()
  ""
  (interactive)
  (let ((bn (buffer-name)))
    (cond
     ((string-match
       "gitlab.com\\/.*#note.*"
       "gitlab.com/nomadic-labs/tezos/-/issues/339#note_507670903<3>")
      (message "settign markdwon mode")
      (markdown-mode)))))

;; this does not work it seems
;; (add-to-list 'edit-server-edit-mode-hook 'aj/edit-server-edit-mode-hook t)
(add-to-list 'edit-server-start-hook 'aj/edit-server-edit-mode-hook t)

(provide 'arvid-editserver)
