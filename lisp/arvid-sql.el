(use-package
 sqlformat
 :ensure t
 :config
 (setq sqlformat-command 'sqlformat)
 (setq sqlformat-args '("-k" "upper"))
 (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(provide 'arvid-sql)
