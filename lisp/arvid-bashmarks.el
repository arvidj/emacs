(defun bashmarks-alist ()
  ""
  (interactive)
  (let ((str (string-trim-right (shell-command-to-string (concat "source ~/.sdirs && env | grep ^DIR_")))))
    (mapcar (lambda (s)
              (let* ((spl (split-string s "=" ))
                     (mark (cadr (split-string (car spl) "DIR_" )))
                     (dir (cadr spl)))
                `(,mark . ,dir)
                ))
            (split-string str "\n" ))))

(defun setup-bashmarks ()
  ""
  (interactive)
  (cl-loop for (mark . dir) in (bashmarks-alist)
           do (global-set-key (kbd (concat "C-c g " mark))
                              `(lambda () (interactive)
                                 (let ((default-directory ,dir)) (ido-find-file))
                              ))))

;; (setup-bashmarks)

;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))

;; (eval `(defhydra hydra-bashmarks (global-map "<f2>") (:columns 3)
;;            "cljr"
;;            ,@(mapcar (lambda (x)
;;                        (list (car x) (lambda () (interactive) (print x)) (cdr x)))
;;                      (bashmarks-alist))))

;; (defhydra hydra-bashmarks (:columns 3)
;;            "cljr"
;;            ("r" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/michelson-reference")
;;            ("t" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/tezos")
;;            ("c" (lambda (x) (interactive) (print x)) "/home/arvid/Dropbox/Jobb/Nomadic_Labs/projects/camlcase")
;;            ("b" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/blog")
;;            ("a" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/albert")
;;            ("f" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/fmdis")
;;            ("d" (lambda (x) (interactive) (print x)) "/home/arvid/dev")
;;            ("o" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/ott")
;;            ("m" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/mi-cho-coq")
;;            ("w" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/albert-website")
;;            ("t" (lambda (x) (interactive) (print x)) "/home/arvid/dev/nomadic-labs/tezos/src/lib_cache/test"))g

;; (mapcar (lambda (x)
;;           (list (car x) (lambda () (interactive) (print x)) (cdr x)))
;;         (bashmarks-alist))

(provide 'arvid-bashmarks)
