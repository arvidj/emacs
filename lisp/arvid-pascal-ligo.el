;; a simple major mode, pascal-ligo-mode
(provide 'pligo-mode)


(setq pligo-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))


(setq
 pligo-font-lock-keywords
 (let* (
        ;; define several category of keywords
        (x-keywords
         '("function"
           "type"
           "is"
           "record"
           "end"
           "const"
           "var"
           "block"
           "case"
           "of"
           "else"
           "with"))
        (x-types
         '("nat"
           "map"
           "address"
           "big_map"
           "contract"
           "string"
           "bool"
           "None"
           "Some"))
        (x-constants '("False" "True"))
        (x-events '())
        (x-functions '("failwith"))

        ;; generate regex string for each category of keywords
        (x-keywords-regexp (regexp-opt x-keywords 'symbols))
        (x-types-regexp (regexp-opt x-types 'words))
        (x-constants-regexp (regexp-opt x-constants 'words))
        (x-events-regexp (regexp-opt x-events 'words))
        (x-functions-regexp (regexp-opt x-functions 'words)))

   `((,x-types-regexp . font-lock-type-face)
     (,x-constants-regexp . font-lock-constant-face)
     (,x-events-regexp . font-lock-builtin-face)
     (,x-functions-regexp . font-lock-function-name-face)
     (,x-keywords-regexp . font-lock-keyword-face)
     ;; note: order above matters, because once colored, that part won't change.
     ;; in general, put longer words first
     )))

(define-derived-mode
 pligo-mode
 fundamental-mode
 "pascal-ligo-mode"
 "major mode for editing mymath language code."
 (setq font-lock-defaults '(pligo-font-lock-keywords))
 (set-syntax-table pligo-mode-syntax-table))
