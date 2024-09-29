
(defun aj/expect-update ()
  "Evaluates the ACTUAL part of the last '(expect EXPECTED ACTUAL)' before point and replaces EXPECTED with the result."
  (interactive)
  (save-excursion
    (let ((end (point))
          (sexp (elisp--preceding-sexp))
          (start (save-excursion (backward-sexp) (point))))
      (when (and (listp sexp)
                 (= (length sexp) 3)
                 (eq (car sexp) 'expect))
        (let* ((expected (nth 1 sexp))
               (actual (nth 2 sexp))
               (actual-value (eval actual)))
          (if (equal actual-value expected)
              (message "Nothing to update in expect clause.")
            (goto-char start)
            (search-forward (format "%S" expected) end)
            (replace-match (format "%S" actual-value) t t)
            (message "Replaced %S with %S in expect clause." expected actual-value)))))))

(expectations
  (desc "aj/expect-skeleton")
  (expect _ (aj/expect-skeleton _)))

(defun aj/expect-skeleton ()
  "Finds the last defined function before point and inserts a test skeleton for it."
  (interactive)
  (save-excursion
    (let ((p (point))
          func-name)
      ;; Search backward for a function definition
      (if (re-search-backward "^(\\(cl-\\)defun \\([^ ]+\\)" nil t)
          (setq func-name (match-string 2))
        (error "No function definition found before point"))
      ;; Insert the test skeleton at the original point position
      (goto-char p)
      (insert (format "(expectations
  (desc \"%s\")
  (expect EXPECTED (%s ARG)))\n" func-name func-name))))
  (re-search-forward " ARG"))

(expectations
  (desc "aj/expect-skeleton")
  (expect EXPECTED (aj/expect-skeleton ARG)))

(use-package ert-expectations
  :ensure t)
