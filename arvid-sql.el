(require 'sql)

(add-hook 'sql-mode-hook 'my-sql-mode-hook) 
(defun my-sql-mode-hook () 
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

  ;; Make # start a new line comment in SQL. This is MySQL-specific
  ;; syntax.
  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))

(setq sql-mysql-options '("-P 8888"))

(provide 'arvid-sql)