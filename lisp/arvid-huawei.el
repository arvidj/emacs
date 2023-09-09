;; Set Windows-specific preferences if running in a Windows environment.
(defun udf-windows-setup ()
  (interactive)
  ;; The variable `git-shell-path' contains the path to the `Git\bin'
  ;; file on my system. I install this in
  ;; `%USERPROFILE%\LocalAppInfo\apps\Git\bin'.
  (setq
   git-shell-path "C:\\RailsInstaller\\Git\\bin\\"
   ;; "C:\\Users\\a80048607\\AppData\\Local\\Programs\\Git\\bin\\"
   ;; "C:\\Program\\ Files\\ \\(x86\\)\\Git\\bin\\"
   ;; (concat (getenv "USERPROFILE") "\\LocalAppInfo\\apps\\Git\\bin")
   )
  (setq git-shell-executable (concat git-shell-path "bash.exe"))
  (add-to-list 'exec-path git-shell-path)
  (setenv
   "PATH"
   (concat
    "C:\\bin\\"
    ";"
    "C:\\Users\\a80048607\\bin"
    ";"
    "C:\\RailsInstaller\\Git\\bin"
    ";"
    "C:\\Coq\\bin"
    ";"
    ;; "C:\\Users\\a80048607\\AppData\\Local\\Programs\\Git\\bin\\" ";"
    ;; "C:\\dev\\u500\\ETE\\Made\\" ";"
    ;; "C:\\Python34\\" ";"
    (getenv "PATH")))

  ;; (setq explicit-shell-file-name
  ;; 		 "C:/Program Files (x86)/Git/bin/bash.exe")
  (setq explicit-shell-file-name git-shell-executable)
  (setq shell-file-name "sh.exe")
  ;; (add-to-list 'exec-path "C:/Users/100759729/AppData/Local/Programs/Git/bin/")

  ;; (setq explicit-sh.exe-args '("--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

  (message "Windows preferences set."))

(if (eq system-type 'windows-nt)
    (udf-windows-setup))

;; (setenv "PATH"
;; 		(concat
;; 		 "C:\\Program\\ Files\\ \\(x86\\)\\Git\\bin\\" ";"
;; 		 (getenv "PATH")))

;; (if (equal system-type 'windows-nt)
;;     (progn (setq explicit-shell-file-name
;; 				 "C:/Program Files (x86)/Git/bin/bash.exe")
;; 		   (setq shell-file-name "C:/Program Files (x86)/Git/bin/bash.exe")
;; 		   (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
;; 		   (setq explicit-sh.exe-args '("--login" "-i"))
;;            (setenv "SHELL" shell-file-name)
;;            (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))

(provide 'arvid-windows-nt)
