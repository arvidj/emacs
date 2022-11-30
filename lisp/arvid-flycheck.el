;;; arvid-flycheck --- Summary
;;; Commentary:
;;; custom checkers
;;; Code:
;; (require 'flycheck)
;; (global-flycheck-mode)

;; (flycheck-define-checker twig-lint
;;   "A twig linter"
;;   :command ("~/.composer/vendor/bin/twig-lint"
;; 			"lint" "--ansi" "--format=csv"
;; 			source)
;;   :error-patterns
;;   ((error line-start "\""  (file-name) "\"," line "," (message) line-end))
;;   :modes twig-mode html-mode)
;; ;; (add-to-list 'flycheck-checkers 'twig-lint)
;; (setq flycheck-checkers (cdr flycheck-checkers))
;; ;; (provide 'arvid-flycheck)

(provide 'arvid-flycheck)
;;; arvid-flycheck.el ends here

