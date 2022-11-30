(require 'dired)

;; TODO: Colors per file types, check out dired+
;; TODO: Map over keybindings, see arvid-keys.el

(setq dired-listing-switches "-alh")

(define-key dired-mode-map (kbd "C-c e") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "I") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c h") #'(lambda () (interactive) (dired "~")))
(define-key dired-mode-map (kbd "C-c o") 'dired-xdg-open)

(defun dired-xdg-open ()
    ""
  (interactive)
  (shell-command (concat "setsid xdg-open " (shell-quote-argument  (dired-get-file-for-visit)) " &")))

;; http://www.emacswiki.org/emacs-en/DiredSortBySizeAndExtension

(use-package dired-icon
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'dired-icon-mode))

(provide 'arvid-dired)
