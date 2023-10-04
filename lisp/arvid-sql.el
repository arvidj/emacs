(use-package
 sqlformat
 :ensure t
 :config
 (setq sqlformat-command 'sqlformat)
 (setq sqlformat-args '("-k" "upper"))
 (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(setq sql-sqlite-options '("-column"))

(provide 'arvid-sql)
