;; Shell-script

;; TODO Fix beginning-of-defun
(add-hook 'sh-mode-hook 'arvid-sh-mode-hook)

(defun arvid-sh-mode-hook ()
  ""
  (setq
   sh-indent-comment t
   open-paren-in-column-0-is-defun-start nil)

  (flycheck-mode))

;; Requires the shftm binary (https://github.com/mvdan/sh) which can
;; be installed through apt: sudo apt-get install shfmt
(use-package shfmt
  :ensure t
  :init
  ;; Put space before redirect operators (<, >, etc)
  (setq shfmt-arguments '("-sr"))
  (add-hook 'sh-mode-hook #'shfmt-on-save-mode))

(provide 'arvid-sh)
