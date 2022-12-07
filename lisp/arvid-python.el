(setenv "PATH"
		(concat
         "/home/arvid/.pyenv/shims" ":"
		 (getenv "PATH")))

;; (use-package pyenv-mode :ensure t :init (pyenv-mode))

;; Does not work since emacs 28
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

(defun pytest-one-regtestreset ()
  ""
  (interactive)
  (pytest-one "--regtest-reset")
  ;; (let ((pytest-cmd-flags (concat "--regtest-reset " pytest-cmd-flags)))
  ;;   (pytest-one)
  ;;   )
  )

(defun my-python-hook ()
	""
  (interactive)
  (define-key python-mode-map (kbd "C-m") 'newline-and-indent)
  (define-key python-mode-map (kbd "M-.") 'elpy-goto-definition)
  (define-key python-mode-map (kbd "C-M-a") 'beginning-of-defun)
  (define-key python-mode-map (kbd "C-M-e" ) 'end-of-defun)
  (define-key python-mode-map (kbd "C-c t o" ) 'pytest-one)
  (define-key python-mode-map (kbd "C-c t r" ) 'pytest-one-regtestreset)
  (define-key python-mode-map (kbd "C-c t r" ) 'pytest-one-repeat)
  (define-key elpy-mode-map (kbd "C-<return>" ) 'mc/edit-lines)

  (flycheck-mode)
  

  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode))

;;; Python
(use-package python :commands python-mode
  :config
  (add-hook 'python-mode-hook 'my-python-hook))

(use-package pytest :ensure t :commands python-mode)
;; (use-package python-pytest :ensure t)

;; Uses the pyenv-mode package to set the appropriate pyenv
;; automatically:
(use-package
  pyenv-mode
  :commands pyenv-mode
  :ensure t)

;; Uses the blacken formatter to format code automatically on each
;; save.
(use-package blacken
  :commands blacken-mode
  :ensure t
  :config
  ;; Only blacken if pyprojet.toml contains black.
  (setq blacken-only-if-project-is-blackened t)
  ;; Set the line length 79
  (setq blacken-line-length 80)
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package elpy
  :after python
  :ensure t
  :config
  ;; (elpy-enable)
  )

;; (use-package jedi
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t)                 ; optional
;;   )


;; (add-to-list 'exec-path "/home/arvid/.pyenv/bin")

(defun aj/python-beginning-of-block ()
  " Goes to the start of current block, and move point to indentation."
  (interactive)
  (python-nav-beginning-of-block)
  (back-to-indentation))

(defun aj/python-end-of-block ()
  " Goes to the end of current block, and move point to end of line."
  (interactive)
  (python-nav-end-of-block)
  (move-end-of-line 0))


(use-package pydoc-info
  :after python
  :ensure t)

;; (setq python-buffer-main nil)
;; (defun load-main-python-file ()
;;   ""
;;   (interactive)
;;   (message "Run hook")
;;   (if python-buffer-main
;;       (with-current-buffer python-buffer-main
;;         (python-shell-send-buffer t))
;;     (python-shell-send-buffer t)
;;     (setq python-buffer-main (current-buffer))))

;; (defun load-or-reload-python ()
;;   ""
;;   (interactive)
;;   (let ((proc (python-shell-get-process))
;;         (inferior-python-mode-hook
;;          (cons 'load-main-python-file inferior-python-mode-hook)))
;;     (when proc (kill-process proc))
;;     (run-python)
;;     (python-shell-switch-to-shell))

;;   ;; (python-shell-send-buffer t)
;;   ;; (setq python-buffer-main (current-buffer))))


;;   ;; if python is not running
;;   ;;;; start repl
;;   ;;;; load current file
;;   ;;;; set current file as the file to load
;;   ;; else
;;   ;;; kill repl
;;   ;;; load the file to load
;;   )

;; (define-key python-mode-map (kbd "C-c C-c") 'load-or-reload-python)

(defun aj/break-string-python ()
  ""
  (interactive)
  (while (> (save-excursion (end-of-line) (current-column)) 70)
    (beginning-of-line)
    (forward-char 67)
    (insert "' +\n'")
    (python-indent-line-function)
    ))


(defun aj/re-string-re-python (re-string)
  ""
  (interactive)
  (replace-regexp-in-string
   "\\(\\\\\\)?[()|]"
   (lambda (match)
     (pcase match
       ("\\(" "(")
       ("(" "\\(")
       ("\\)" ")")
       (")" "\\)")
       ("\\|" "|")
       ("|" "\\|")
       ))
   re-string t t))

(provide 'arvid-python)
