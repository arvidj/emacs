(defun aj/dune-build-here (parent)
  ""
  (interactive "p")
  (compile
   (concat
    "dune build "
    (if (= parent 1)
        "."
      ".."))))

(defvar aj/ocaml-current-buffer nil)

(defun aj/dune-runtest-promote-revert (buf str)
  ""
  (message "[aj/dune-runtest-promote-revert] %s / %s: '%s'"
           (buffer-name)
           aj/ocaml-current-buffer
           (string-trim str))
  (remove-hook
   'compilation-finish-functions 'aj/dune-runtest-promote-revert)
  (setq aj/ocaml-current-buffer nil)
  (when (and aj/ocaml-current-buffer
             (string=
              (string-trim str) "exited abnormally with code 1"))
    (message "[aj/dune-runtest-promote-revert]: reverting %s"
             aj/ocaml-current-buffer)
    (with-current-buffer aj/ocaml-current-buffer
      (revert-buffer t t))))

(defun aj/dune-runtest-here (auto-promote)
  ""
  (interactive "p")
  (if (= auto-promote 1)
      (compile "dune runtest . ")
    ;; Run-test with --auto-promote
    (setq aj/ocaml-current-buffer (current-buffer))
    (add-hook
     'compilation-finish-functions 'aj/dune-runtest-promote-revert
     0)
    (compile "dune runtest . --auto-promote")))

(defun aj/tuareg-mode-hook ()
  (aj/define-keys
   tuareg-mode-map
   `(("C-M-p" nil)
     ("C-c i" nil)
     ("C-c C-c" aj/dune-build-here)
     ("C-c C-j" aj/dune-runtest-here)
     ("C-M-n" nil)
     ("C-q" tuareg-indent-phrase)
     ("C-h o" merlin-document)
     ("C-c y f" aj/tezt-this-file)))

  ;; (display-fill-column-indicator-mode)
  ;; (setq-local display-fill-column-indicator-column 80)
  (setq-local comment-style "indent")
  (setq-local comment-multi-line t)

  ;; Highlight lines that depass 80 columns
  (setq-local whitespace-line-column 80)
  (make-variable-buffer-local 'whitespace-style)
  (setq whitespace-style '(face lines-tail))
  (whitespace-mode))

(defun aj/tezt-this-file ()
  ""
  (interactive)
  (compile
   (concat
    "cd ../../ && "
    "dune exec tezt/tests/main.exe -- --color -j 7 --file "
    (file-name-nondirectory (buffer-file-name)))))

(use-package
 tuareg
 :custom (tuareg-opam-insinuate t)
 :hook (tuareg-mode . aj/tuareg-mode-hook)
 :config (tuareg-opam-update-env (tuareg-opam-current-compiler)))

;; It is mandatory to load 'ocp-indent' *after* 'ocamlformat', because
;; 'ocamlfomat' installs broken hooks to indent after newline.
(use-package
 ocp-indent
 :after ocamlformat
 :commands (ocp-indent-caml-mode-setup)
 :hook (tuareg-mode . ocp-indent-caml-mode-setup))

(use-package
 ocamlformat
 :after merlin
 :custom (ocamlformat-show-errors nil)
 :hook (before-save . ocamlformat-before-save))

(use-package
 merlin
 :ensure t
 :custom
 (merlin-locate-in-new-window 'never)
 (merlin-locate-preference 'ml)
 (merlin-command 'opam)
 (merlin-completion-with-doc t)
 :hook ((tuareg-mode . merlin-mode) (merlin-mode . aj/init-merlin))
 :init
 (defun aj/init-merlin ()
   (merlin-toggle-view-errors)
   (company-mode)))

;; These two lines are necessary for Merlin to display docstrings in
;; the *Quick help* buffer (that is displayed by 'company-box')
(use-package merlin-company :ensure t :demand :after merlin)

(use-package
 merlin-eldoc
 :hook (merlin-mode . merlin-eldoc-setup)
 :custom (eldoc-echo-area-use-multiline-p t))

(use-package
 flycheck-ocaml
 :ensure t
 :after merlin
 :hook (merlin-mode . init-flycheck-ocaml)
 :config
 (defun init-flycheck-ocaml ()
   (setq-local merlin-error-after-save nil)
   (flycheck-ocaml-setup)
   (flycheck-mode)))

(use-package dune :ensure t)

(use-package
 dune-format
 :ensure t
 :hook (dune-mode . dune-format-on-save-mode))

(provide 'arvid-ocaml)
