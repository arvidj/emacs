;; https://github.com/rlister/org-present

(defun aj/org-present-mode-hook ()
  "Stuff that happens when we enter a presentation"
  (interactive)
  (org-present-big)
  (org-display-inline-images)
  (org-present-hide-cursor)
  (org-present-read-only)
  )

(defun aj/org-present-quit-hook ()
  "When we exit it."
  (interactive)
  (org-present-small)
  (org-remove-inline-images)
  (org-present-show-cursor)
  (org-present-read-write)
)

(use-package org-present
  :ensure t
  :bind (:map org-present-mode-keymap
              ("q" . org-present-quit))
  :config
  (setq org-present-text-scale 6)
  (add-hook 'org-present-mode-hook 'aj/org-present-mode-hook)

  (add-hook 'org-present-mode-quit-hook 'aj/org-present-quit-hook))

(provide 'arvid-org-present)
