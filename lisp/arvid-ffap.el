(use-package
 ffap
 :config
 (add-to-list 'ffap-alist '(latex-mode . arvid-ffap-latex-mode) t)
 (add-to-list 'ffap-alist '(gitlab-ci-mode . arvid-ffap-gitlab-ci-mode) t))

(provide 'arvid-ffap)
