;;; Ido-mode
(require 'ido)

(ido-mode t)
(setq ido-case-fold t)
(ido-everywhere t)
(setq ido-max-directory-size 100000)
(global-set-key "\M-." 'find-tag)

;; (defun my-ido-find-tag ()
;;   "Find a tag using ido"
;;   (interactive)
;;   (tags-completion-table)
;;   (let (tag-names
;;         (def (find-tag-default)))
;;     (mapc (lambda (x)
;; 	    (unless (integerp x)
;; 	      (push (prin1-to-string x t) tag-names)))
;; 	  tags-completion-table)
;; 	(find-tag (ido-completing-read "Find tag: " tag-names nil nil nil nil def))))

;; disable auto searching for files unless called explicitly
(setq ido-auto-merge-delay-time 2)
(setq ido-file-extensions-order '(".tex" ".log"))

(define-key
 ido-file-dir-completion-map (kbd "C-c C-s")
 (lambda ()
   (interactive)
   (ido-initiate-auto-merge (current-buffer))))

;; ;; TODO: this exists in newer versions of magit, remove after
;; ;; upgrading
;; (defun arvid-ido-enter-magit-status ()
;;   "Drop into `dired' from file switching."
;;   (interactive)
;;   (setq ido-exit 'fallback fallback 'magit-status)
;;   (exit-minibuffer))


(add-hook
 'ido-minibuffer-setup-hook
 (lambda ()
   (interactive)
   (define-key ido-completion-map (kbd "C-,") 'backward-kill-word)
   ;; (define-key ido-completion-map (kbd "C-x g") 'arvid-ido-enter-magit-status)
   (define-key
    ido-file-dir-completion-map (kbd "C-,") 'backward-kill-word)))


(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(provide 'arvid-ido)
