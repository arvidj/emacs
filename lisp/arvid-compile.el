
(setq pre-compilation-window-conf-reg
      (registerv-make "pre-compilation-window-conf"))

(defun my-compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (window-configuration-to-register pre-compilation-window-conf-reg)
    (call-interactively 'compile)
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 4))
    (select-window cur)))

;;  The idea seems to be to combine the window from which a
;;  compilation is launched with the compilation window in an atomic
;;  unit. However, it does not work.
;; (add-to-list display-buffer-alist '((".*compilation.*" (display-buffer-in-atom-window))))

(defun compilatation-finish (buf str)
  (if (null (string-match ".*exited abnormally.*" str))
      ;;no errors, make the compilation window go away in a few seconds
      (progn
        ;; (run-at-time
        ;;  "2 sec" nil 'delete-windows-on
        ;;  (get-buffer-create "*compilation*"))
        (run-at-time
         "2 sec" nil
         (lambda ()
           (delete-windows-on (get-buffer-create "*compilation*"))
           (jump-to-register pre-compilation-window-conf-reg)))


        (message "No Compilation Errors!"))))

; from enberg on #emacs
(add-hook 'compilation-finish-functions 'compilatation-finish)

;; Colorize in compilation buffers
(use-package ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(provide 'arvid-compile)
