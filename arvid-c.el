(add-hook 'c-mode-hook
	  '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

;; Create my personal style.
(defconst my-c-style
  '((c-hanging-braces-alist     . ((defun-open after)
								   (substatement-open after)))
    (c-offsets-alist            . ((case-label        . +)
				   (arglist-close     . 0)
				   (comment-intro     . 0)))
    (c-hanging-semi&comma-criteria nil))  ;; Does not work
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq c-basic-offset 4)
  (setq tab-width 4)

  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1)
  (c-subword-mode 1)
  ;; (auto-complete-mode 1)

  ;; need to unset this so that I can use it for window-mgmt
  (local-unset-key (kbd "C-M-j")) 
  (local-unset-key (kbd "C-M-k")) 

  ;; (flymake-mode 1)
  (c-toggle-electric-state 1)

  ;; Set fill width to 80 columns
  (setq fill-column 80)

  ;; Try to make auto-pair + electric mode work together.
  ;; (setq autopair-handle-action-fns '(my-test-handler))

  (define-key c-mode-map (kbd "M-a") nil)
  
  (define-key c-mode-map (kbd "ä") (lambda () (interactive (insert "$"))))
  (define-key c-mode-map (kbd "ö") (lambda () (interactive (insert ";"))))
  (define-key c-mode-map (kbd ";") (lambda () (interactive) (message "Idiot!")))
  (define-key c-mode-map (kbd "$") (lambda () (interactive) (message "Idiot!")))
  (define-key c-mode-map (kbd "M-ä") (lambda () (interactive (insert "ä")))))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (defun my-test-handler (a b c)
;;   (autopair-default-handle-action a b c)
;;   (open-line 1)
;;   (next-line)
;;   (indent-according-to-mode)
;;   (previous-line)
;;   (indent-according-to-mode))

(provide 'arvid-c-mode)
