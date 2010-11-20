;; TODO: It would be nice to have a mode for trying out TS.


;;; TypoScript-mode
(autoload 'ts-mode "ts-mode")

(let ((ts-files-re (regexp-opt '(".ts"
								 "/ext_typoscript_setup.txt"
								 "/ext_typoscript_constants.txt"
								 "/tsconfig_tinymce_common.txt"
								 "/tsconfig_tinymce_fe.txt"
								 "/tsconfig_tinymce_be.txt"
								 "/typoscript_setup.txt"
								 "/tsconfig_page.txt"
								 "/fileadmin/templates/typoscript/"))))
  ;; What is up with the back tick monstrosity :(
  (add-to-list 'auto-mode-alist `(,ts-files-re . ts-mode)))

(add-hook 'ts-mode-hook 'my-ts-mode-hook)

(defun my-ts-mode-hook () 
  (setq tab-width 2)
  (c-subword-mode)
  (define-key ts-mode-map (kbd "C-j") 'join-line))

(provide 'arvid-ts)
