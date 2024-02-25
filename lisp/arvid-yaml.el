(provide 'arvid-yaml)

(defun aj/yaml-indent-left ()
  ""
  (interactive)
  (indent-rigidly (region-beginning) (region-end) -2))

(defun aj/yaml-indent-right ()
  ""
  (interactive)
  (indent-rigidly (region-beginning) (region-end) 2))

(defun aj/yaml-mode-hook ()
  ""
  (define-key yaml-mode-map (kbd "C-c <") 'aj/yaml-indent-left)
  (define-key yaml-mode-map (kbd "C-c >") 'aj/yaml-indent-right)
  (setq yaml-imenu-generic-expression
        '((nil "^\\([.[:digit:][:alpha:]_:-]+\\):$" 1)))
  (highlight-indent-guides-mode))

(use-package
 highlight-indent-guides
 :ensure t
 :custom (highlight-indent-guides-method 'bitmap))

(use-package
 yaml-mode
 :ensure t
 :config
 (eval-after-load 'yaml-mode
   '(progn
      (add-to-list 'yaml-mode-hook 'aj/yaml-mode-hook))))
