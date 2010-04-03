;;; Org-mode
(require 'remember)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; TODO: Bind ctrl-j to join-line in org-mode.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cb" 'org-iswitchb)

;;; For remember in Org.
;;; http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
(org-remember-insinuate)
(define-key global-map "\C-cr" 'org-remember)

(provide 'arvid-org-mode)