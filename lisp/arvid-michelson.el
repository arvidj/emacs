(require 'michelson-mode)
(require 'albert-mode)

;; (setq michelson-client-command "/home/arvid/dev/nomadic-labs/tezos/tezos-client -d /home/arvid/.tezos-client-mockup")
;; (setq michelson-client-mode "mockup")
(setq michelson-client-command "/home/arvid/dev/nomadic-labs/tezos/master/tezos-client")
(setq michelson-client-command "/home/arvid/dev/nomadic-labs/tezos/master/tezos-client --base-dir /tmp/mockup --mode mockup --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK")
;; (setq michelson-client-mode "client")

;; (autoload 'michelson-mode "/home/arvid/dev/nomadic-labs/tezos/emacs/michelson-mode.el")

(defun michelson-documentation-at-point ()
	""
  (interactive)
  (let* ((atp (thing-at-point 'symbol))
         (type (cond ((s-uppercase? atp) "instr")
                     ((s-lowercase? atp) "type"))))
    (if (not type)
        (message "Unknown object: " atp)
      (let ((url (concat "https://arvidj.eu/michelson/#" type  "-" atp))
            (buf (get-buffer-create "*Michelson doc*")))
        (browse-url url)
        ;; (shell-command (concat "w3m " "'" url "'") buf)
        ;; (display-buffer buf)
        ;; (save-excursion
        ;;   (set-buffer buf)
        ;;   (re-search-forward (concat "^" atp ".*:.*$"))
        ;;   (recenter 0)
        ;;   )
        ;; (with-current-buffer buf)
        ))))

;; (modify-syntax-entry ?_ "w"  michelson-mode-syntax-table)

(defun my-michelson-mode-hook ()
	""
  (interactive)
  (modify-syntax-entry ?_ "."  michelson-mode-syntax-table)
  (setq comment-start "#")
  (define-key michelson-mode-map (kbd "C-c C-d C-d") 'michelson-documentation-at-point)
  (define-key michelson-mode-map (kbd "C-c C-c") 'michelson-client-run-script)

  )

(add-to-list 'auto-mode-alist '("\\.tz\\'" . michelson-mode))
(add-to-list 'auto-mode-alist '("\\.tez\\'" . michelson-mode))

(add-to-list 'michelson-mode-hook 'my-michelson-mode-hook)

;; (eval-after-load "/home/arvid/dev/nomadic-labs/tezos/emacs/michelson-mode.el"
;;   (add-to-list 'michelson-mode-hook 'my-michelson-mode-hook))


;; overwrite to disable live-editing on errors

;; (defun michelson-async-command-to-string (command callback)
;;   "Asynchronously execute `COMMAND' and call the `CALLBACK' on the resulting string."
;;   (lexical-let ((command command)
;;                 (callback-fun callback))
;;     (deferred:$
;;       (deferred:$
;;         (apply 'deferred:process command)
;;         (deferred:nextc it callback-fun))
;;       ;; TODO: make this show only the client error
;;       (deferred:error it (lambda (err)
;;                            (michelson-write-output-buffer (cadr err))
;;                            (message "Disabling live-coding")
;;                            (make-local-variable michelson-live-editing)
;;                            (michelson-toggle-live-editing )
;;                            )))))

(defun michelson-client-run-script (script storage input args)
  ""
  (interactive "fScript: \nMStorage: \nMInput: \nMArgs: ")
  (shell-command
   (concat michelson-client-command
           " run " " script " (shell-quote-argument (or script (buffer-file-name)))
           " on " " storage " (shell-quote-argument storage)
           " and " " input " (shell-quote-argument input)
           " " args)))



(defun michelson-client-typecheck-script (script &optional args)
  ""
  (interactive "fScript: \n")
  (shell-command
   (michelson-client-command
    (append (list "typecheck" "script" (shell-quote-argument (or script (buffer-file-name))))
            args))))

;; (michelson-client-typecheck-script "foo.tz" nil)

(provide 'arvid-michelson)
