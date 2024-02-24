;; [vertico](https://github.com/minad/vertico)
;;
;; An alternative to smex, helm, ido ...
(use-package vertico :ensure t :init (vertico-mode))

;; By default, vertico requires literal matches.
;; orderless makes the matching more fuzzy-like.
(use-package
 orderless
 :ensure t
 :custom (completion-styles '(orderless basic))
 (completion-category-overrides
  '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
;;
;; This includes e.g., documentation in M-x.
(use-package
 marginalia
 :ensure t

 ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
 ;; available in the *Completions* buffer, add it to the
 ;; `completion-list-mode-map'.
 :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

 ;; The :init section is always executed.
 :init

 ;; Marginalia must be activated in the :init section of use-package such that
 ;; the mode gets enabled right away. Note that this forces loading the
 ;; package.
 (marginalia-mode))

(provide 'arvid-vertico)
