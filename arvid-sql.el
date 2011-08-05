(require 'sql)

(add-hook 'sql-mode-hook 'my-sql-mode-hook) 
(defun my-sql-mode-hook () 
  (define-key sql-mode-map (kbd "RET") 'newline-and-indent)

  ;; Make # start a new line comment in SQL. This is MySQL-specific
  ;; syntax.
  (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
  (set-syntax-table sql-mode-syntax-table))

;; (setq sql-mysql-options '("-P 8888"))

(add-hook 'sql-interactive-mode-hook 'my-sqli-mode-hook)
(defun my-sqli-mode-hook ()
  ""
  (interactive)
  (define-keys
	sql-interactive-mode-map
	`(("ö" ,(make-inserter ";"))
	  ("C-ö" ,(make-inserter "\\G"))
	  ("M-ö" ,(make-inserter "ö")))))

;; Work in progress
(defun ff ()
	"temp function. Returns a string based on current regex match."
	(let (matchedText newText)
		(setq matchedText
					(buffer-substring
					 (match-beginning 0) (match-end 0)))
		(setq newText
					(upcase matchedText ))
		newText
		))

(defun mysql-upcase-keywords ()
	(interactive)
	(let ((keywords (car (car sql-mode-mysql-font-lock-keywords))))
		(goto-char (region-beginning))
		(while (re-search-forward keywords (region-end) t)
			(replace-match (ff) nil nil))))

;; Hur läsa av tabeller och databaser för completion:
;;  1) Stäng av echo av output i comint
;;  2) Använd (comint-simple-send ) för att skicka iväg describe ...
;;  3) Sätt på echo igen
;;
;;  ... 4) Spara alla data i en struktur, lägg in i auto-complete och yas.

(provide 'arvid-sql)
