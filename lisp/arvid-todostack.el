(load-file "~/.emacs.d/plugins/todostack/todostack.el")
(load-file todostack-save-file)
(add-hook 'kill-emacs-hook 'todostack-save)
(add-hook 'emacs-startup-hook 'todostack-load)
(add-hook 'todostack-post-op-hook 'todostack-save)

(global-set-keys
 `(("C-c sr" todostack-rotate)
   ("C-c sp" todostack-pop)
   ("C-c su" todostack-push)
   ("C-c sq" todostack-queue)
   ("C-c ss" todostack-list)
   ("C-c sl" my-todostack-list-push-and-pro)))

(defun my-todostack-list-push-and-pro ()
  ""
  (interactive)
  (call-interactively 'todostack-push)
  (todostack-procrastinate 1))

(provide 'arvid-todostack)
