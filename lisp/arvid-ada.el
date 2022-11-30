(defvar arvid-gnat-project-file
  nil
  "Project .GPR file")


(defvar arvid-gnat-scenario-file
  nil
  "Current scenario file worked on")


;; Note sure if this works
(defun arvid-gnat-find (symbol &optional project-file)
  ""
  (interactive (list (or (symbol-name (symbol-at-point)) (read-string "Name: "))))
  (-when-let (project-file (arvid-gnat-get-project-file))
    (async-shell-command (concat "gnat find -P " project-file " " symbol)) "*Gnat Find*"))

(defun arvid-gnat-compile-file ()
  ""
  (interactive)
  (-when-let* ((project-file (arvid-gnat-get-project-file))

               (compile-command (concat "gnatmake -ws -c -u -P" project-file " -Xmode=verbose " (buffer-name))))
    (setq compilation-search-path (list default-directory))
    (compile compile-command)))

(use-package compile
  :config

  ;; for debugging
  ;; (setq compilation-error-regexp-alist nil)
  ;; (setq compilation-error-regexp-alist-alist nil)
  (add-to-list 'compilation-error-regexp-alist 'adacontrol)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(adacontrol "^\\(.*\\):\\([0-9]+\\)\\:\\([0-9]+\\)\\: \\(\\(Warning\\)\\|\\(Error\\)\\)\\: \\(.*?\\)\\: \\(.*\\)$" 1 2 3)))


(defun arvid-gnat-adacontrol-file ()
  "Runs AdaControl on the current file in a compilation buffer"
  (interactive)
  (-when-let* ((file (buffer-name))
               (unit (file-name-base file))
               (project-file (arvid-gnat-get-project-file))
               (default-directory (file-name-directory project-file))
               (compile-command (concat "gprbuild -f -ws -c -u -P" project-file " -gnatct -Xmode=verbose " file))
               (adacontrol-command (concat "adactl -F GNAT_SHORT -f Coding_rules.aru " unit " -- -FT -C1 ..\/Made\/Object\/" unit ".adt")))
    (setq compilation-search-path (list (file-name-directory (buffer-file-name))))
    (compile (concat compile-command " && " adacontrol-command))))

(defun arvid-adacontrol-ignore-current-error (style-line)
  "Adds a comment to AdaControl which ignore the error on the
current line. Use argument to control the comment style"
  (interactive "p")

  ;; 1. go to compilation buffer
  (let ((found nil) (file nil) (line nil) (col nil) (rule nil))
    (with-current-buffer (next-error-find-buffer)
      ;; 2. get info about current error
      (when (looking-at (car (cdr (assq 'adacontrol compilation-error-regexp-alist-alist))))
        (setq found t)
        (setq file (match-string 1))
        (setq line (match-string 2))
        (setq col (match-string 3))
        (setq rule (match-string 7))))
    (when found
      (save-excursion
        (if (>= style-line 0)
            (progn
              (split-line)
              (insert (concat "--## rule off " rule))
              (next-line 2)
              (back-to-indentation)
              (split-line)
              (insert (concat "--## rule on " rule))
              (indent-for-tab-command))

          (move-end-of-line nil)
          (insert (concat "--## rule line on " rule)))))))


(defun arvid-debug-scenario ()
  "Starts GDB on Ete_Executor with the current scenario"
  (interactive)
  (-when-let* ((scenario-file (arvid-gnat-get-scenario-file))
               (current-method (ada-which-function))
               (default-directory (file-name-directory scenario-file))
               (br-command (concat " -ex \"br " current-method " if g_Cycle_Counter = \""))
               (run-command (concat " -ex \"run\""))
               (default-gdb-command (concat "gdb -i=mi ete_executor.exe " br-command run-command))
               (gdb-command (read-string  "Run GDB like: " default-gdb-command)))
    (gdb gdb-command)))

(defun arvid-gnat-select-project-file ()
  ""
  (interactive)
  (setq arvid-gnat-project-file
        (read-file-name "Project file (.gpr): ")))

(defun arvid-gnat-get-project-file ()
  ""
  (interactive)
  (unless arvid-gnat-project-file (arvid-gnat-select-project-file))
  arvid-gnat-project-file)


(defun arvid-gnat-select-scenario-file ()
  ""
  (interactive)
  (setq arvid-gnat-scenario-file
        (read-file-name "Scenario file (.xml): ")))

(defun arvid-gnat-get-scenario-file ()
  ""
  (interactive)
  (unless arvid-gnat-scenario-file (arvid-gnat-select-scenario-file))
  arvid-gnat-scenario-file)


(defun arvid-ada-add-local (name type)
  "Add local variable to current declaration"
  ;; (interactive (list (identity "foo") (identity "bar")))
  (interactive
   (list (or (symbol-name (symbol-at-point)) (read-string "Name: "))
         (read-string "Type: ")))

  (save-excursion

    ;; taken from ada-wisi-goto-declaration-start
    (wisi-validate-cache (point))
    (unless (> wisi-cache-max (point))
      (error "parse failed; can't goto declarative-region-start"))

    (let ((cache (wisi-get-cache (point)))
          (done nil))
      (unless cache
        (setq cache (wisi-backward-cache)))
      ;; cache is null at bob
      (while (not done)
        (if cache
            (progn
              (setq done
                    (cl-case (wisi-cache-nonterm cache)
                      ((subprogram_body subprogram_declaration null_procedure_declaration)
                       (memq (wisi-cache-token cache) '(BEGIN)))))
              (unless done
                (setq cache (wisi-goto-containing cache nil))))
          (setq done t))
        )
      (when cache
        (open-line-and-indent)
        (insert (concat name " : " type ";"))
        (ada-indent-statement)))))

(defun arvid-ada-mode-hook ()
  "Adds ada-specific keybindings to ada-buffers"
  (local-set-key (kbd "C-c C-c") 'arvid-gnat-compile-file))

(use-package ada-mode
  :config
  ;; ada-mode sets this one to the current director if nil when
  ;; opening a adb/ads file, not sure why, but it messes with finding
  ;; the files corresponding to errors in the compilation-buffer.
  (setq compilation-search-path nil)
  (add-hook 'ada-mode-hook 'arvid-ada-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.adc\\'" . ada-mode)))

(provide 'arvid-ada)
