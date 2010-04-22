(require 'css-mode)

(defun css-electric-brace (arg)
  (interactive "P")
  ;; insert a brace
  (self-insert-command 1)
  ;; maybe do electric behavior
  (css-indent-line))

(define-key css-mode-map "}" 'css-electric-brace)

;; Font lock for px, em, %, url-keywords and colors. From rejeep.
(eval-after-load 'css-mode
  '(progn
     (font-lock-add-keywords 'css-mode
                             '(("#[a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\([a-fA-F0-9][a-fA-F0-9][a-fA-F0-9]\\)?" . font-lock-reference-face)
                               ("[0-9]+\\(px\\|em\\|%\\)" 1 font-lock-keyword-face)
                               ("\\(url\\)(" 1 font-lock-function-name-face)))))

(provide 'arvid-css)
