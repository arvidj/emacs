(defun aj/cram-mode-compile (promote)
	""
  (interactive "p")
  (if (= promote 1)
      (cram-mode-compile)
    (cram-mode-compile-and-promote)))

(defun aj/cram-mode-hook ()
	""
  (tuareg-opam-update-env (tuareg-opam-current-compiler)))

(use-package cram-mode
  :mode "\\.t\\'"
  :bind (:map cram-mode-map
              ("C-c C-j" . aj/cram-mode-compile))
  :hook (cram-mode . aj/cram-mode-hook))

(provide 'arvid-cram)
