;;; Org-mode
(require 'remember)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Set C-j to join-line in org-mode
(add-hook 'org-mode-hook
		  (lambda ()
			(define-key org-mode-map (kbd "C-j") 'join-line)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cb" 'org-iswitchb)

;;; For remember in Org.
;;; http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
(org-remember-insinuate)
(define-key global-map "\C-cr" 'org-remember)

(provide 'arvid-org-mode)
