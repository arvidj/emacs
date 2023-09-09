;; (load-file "~/.emacs.d/plugin/PG/generic/proof-site.el")

(use-package
 proof-general
 :ensure t
 :config

 (add-hook 'coq-mode-hook 'my-coq-hook))

;; On proofgeneral mode load, disabled abbrevs
(defun my-coq-hook ()
  ""
  (interactive)
  ;; (opam-update-env ".")
  ;; (message "update")
  (abbrev-mode -1)
  (setq proof-imenu-enable t)
  (add-to-list 'coq-user-reserved-db "mlet")
  (define-key coq-mode-map (kbd "M-a") nil)
  (define-key coq-mode-map (kbd "M-e") nil)
  (define-key coq-mode-map (kbd "M-n") nil)
  (define-key coq-mode-map (kbd "M-p") nil)
  (define-key
   coq-mode-map
   (kbd "<M-wheel-up>")
   'proof-undo-last-successful-command)
  (define-key
   coq-mode-map
   (kbd "<M-wheel-down>")
   'proof-assert-next-command-interactive)

  (define-key
   coq-mode-map (kbd "C-c p 1")
   #'(lambda ()
       (interactive)
       (coq-printing-depth-intset 10)))
  (define-key
   coq-mode-map (kbd "C-c p 2")
   #'(lambda ()
       (interactive)
       (coq-printing-depth-intset 20)))
  (define-key
   coq-mode-map (kbd "C-c p 3")
   #'(lambda ()
       (interactive)
       (coq-printing-depth-intset 30)))
  (define-key
   coq-mode-map (kbd "C-c p 4")
   #'(lambda ()
       (interactive)
       (coq-printing-depth-intset 40)))
  (define-key
   coq-mode-map (kbd "C-c p 5")
   #'(lambda ()
       (interactive)
       (coq-printing-depth-intset 50)))


  (setq company-coq-disabled-features '(prettify-symbols))
  (company-coq-mode)

  (define-key company-coq-map (kbd "C-<return>") 'mc/edit-lines)

  (merlin-mode -1)
  (add-to-list 'coq-shell-init-cmd "Set Nested Proofs Allowed.")
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  )

;; (defun arvid-proof-goto-point (&optional arg)
;;   "Assert or retract to the command at current position.
;; Calls `proof-assert-until-point' or `proof-retract-until-point' as
;; appropriate."
;;   (interactive "p")
;;   (print arg)
;;   (if (= arg 4)
;;       (company-coq-proof-goto-point)
;;     (save-excursion (proof-retract-buffer))
;;     (company-coq-proof-goto-point)
;;     ))

;; (global-set-key (kbd "C-c <C-return>") 'arvid-proof-goto-point)

(setq proof-three-window-mode-policy 'hybrid)

(add-to-list 'completion-ignored-extensions ".vos")
(add-to-list 'completion-ignored-extensions ".vok")

(provide 'arvid-coq)
