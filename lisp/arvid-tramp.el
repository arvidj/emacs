;; This ensures that tramp has access to the PATH as setup for the
;; remote user. This makes binaries handled by e.g. nix home-manager
;; available to tramp and enables e.g. using magit for the remote
;; host. See
;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-programs.html
;; for more information.
(use-package
 tramp
 :config
 (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
 (setq enable-remote-dir-locals t))

(provide 'arvid-tramp)
