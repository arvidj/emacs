(require 'imenu)

;;;###autoload
(define-minor-mode ete-scenario-mode
  ""
  :init-value nil
  :lighter " Scen"
  (when ete-scenario-mode
      (ete-scenario-imenu)
      (setq compilation-read-command nil)
      (setq compilation-buffer-name-function 'ete-scenario-compilation-buffer-name-function)
      (setq compile-command "[ -f result.xml ] && rm result.xml; ete_executor.exe; python /c/bin/pp_res.py")))

;;;###autoload
(defun ete-scenario-compilation-buffer-name-function (foo)
    ""
    (concat "*run " (buffer-name) "*"))

;;;###autoload
(defun ete-scenario-imenu ()
    ""
    (setq imenu-generic-expression
          '((nil "\\(cycle=\".*\"\\)" 0)
            ("*Ports*" "\\(Input name=\".*\"\\)" 0)
            )))

(provide 'arvid-ete-scenario-mode)
