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
  ;; -sr: Put space before redirect operators (<, >, etc)
  ;; -i 4: Indent using 4 spaces
  (setq shfmt-arguments '("-sr" "-i" "4"))
  ;; TODO: only activate if (eq file (format file))
  ;; TODO: this can be checked with [shfmt -d]
  (add-hook 'sh-mode-hook #'shfmt-on-save-mode))

(provide 'arvid-sh)
