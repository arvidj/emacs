(use-package tuareg :ensure t)
(use-package ocp-indent :ensure t)

(when (featurep 'ocp-indent)
  (add-to-list 'load-path
			   (concat
				(replace-regexp-in-string "\n$" ""
										  (shell-command-to-string "opam config var share"))
				"/emacs/site-lisp"))
  (require 'ocp-indent))

;; ocamlformat

;;; load with noerror, since depending on the state of the global
;;; switch, this file may not be available.
(use-package ocamlformat :ensure t)

(defun arvid-ocaml-compile-here (parent)
	""
  (interactive "p")
  (compile (concat "dune build "
                   (if (= parent 1)
                       "."
                     ".."))))

(defun aj/tezt-this-file ()
  ""
  (interactive)
  (compile (concat
            "cd ../../ && "
            "dune exec tezt/tests/main.exe -- --color -j 7 --file "
            (file-name-nondirectory (buffer-file-name)))))

(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun my-tuareg-mode-hook ()
  ""
  (interactive)
  (message "arvid tuareg")
  (define-key tuareg-mode-map (kbd "C-M-p") nil)
  (define-key tuareg-mode-map (kbd "C-c i") nil)
  (define-key tuareg-mode-map (kbd "C-c C-c") 'arvid-ocaml-compile-here)
  (define-key tuareg-mode-map (kbd "C-x C-r") nil)
  (define-key tuareg-mode-map (kbd "C-M-n") nil)
  (define-key tuareg-mode-map (kbd "C-q") 'tuareg-indent-phrase)
  (define-key tuareg-mode-map (kbd "C-h o") 'merlin-document)
  (define-key tuareg-mode-map (kbd "C-c y f") 'aj/tezt-this-file)
  (company-mode)
  ;; (define-key tuareg-mode-map (kbd "TAB") 'company-complete)

  
  ;; See https://github.com/ocaml/tuareg/issues/216
  (setq-local comment-style "indent")
  (setq-local comment-multi-line t)

  (setq-local whitespace-line-column 80)
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode)

  (opam-update-env
   (car (split-string (opam-shell-command-to-string "opam switch show --safe"))))

  ;; ocamlformat
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)

  ;; merlin-mode
  (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" nil t nil)

      (merlin-mode)
      (merlin-use-merlin-imenu)
      (merlin-eldoc-setup)

      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)
      ;; (add-hook 'company-mode-hook 'merlin-mode t)
      ))

  )

(add-hook 'tuareg-mode-hook 'my-tuareg-mode-hook)

;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'merlin-company-backend))

;; (require 'merlin-company)

(add-to-list 'auto-mode-alist '("\\.atd\\'" . tuareg-mode))

;; building & browsing documentation

;; (compile "dune build @doc && xdg-open `git rev-parse --show-toplevel`/_build/default/_doc/_html/`grep 'public_name ' dune | sed 's/.*public_name \\(.*\\))/\\1/'`/index.html")

;; (shell-command
;;  "echo `git rev-parse --show-toplevel`/_build/default/_doc/_html/`grep 'public_name ' dune | sed 's/.*public_name \\(.*\\))/\\1/'`/index.html")

(provide 'arvid-ocaml)

