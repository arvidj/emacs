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
(setq ido-auto-merge-delay-time 99999)

(define-key ido-file-dir-completion-map (kbd "C-c C-s") 
  (lambda() 
    (interactive)
    (ido-initiate-auto-merge (current-buffer))))

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)

(provide 'arvid-ido)
