(setq glci-top-level-definition-re "^\\([^ #].*\\):$")

(defun glci-beginning-of-defun ()
  ""
  (interactive)
  (re-search-backward glci-top-level-definition-re))

(defun glci-end-of-defun ()
  ""
  (interactive)
  (when (re-search-forward glci-top-level-definition-re
                           nil t
                           (if (looking-at
                                glci-top-level-definition-re)
                               2
                             1))
    (goto-char (+ 1 (match-beginning 0)))))

(defun glci-current-job ()
  ""
  (save-excursion
    (glci-beginning-of-defun)
    (match-string 1)))

(defun glci-find-ci-file ()
  ""
  (interactive)
  (let* ((f ".gitlab-ci.yml")
         (dom (locate-dominating-file "." f)))
    (if dom
        (concat dom f))))

(setq
 glcisk-path
 "/home/arvid/dev/nomadic-labs/glci-swiss-army-knife/_build/default/bin/main.exe")

(defvar gitlab-ci-focused-job-mode-map (make-sparse-keymap)
  "Keymap for `gitlab-ci-focused-job-mode-map'.")

(defun glcisk-get-token ()
  ""
  (secrets-get-secret "login" "gitlab access-tokens: glcisk (api)"))


(defun glcisk-focus-raw (ci-file job buffer)
  ""
  (interactive)
  (let* ((cmd
          (string-join (list
                        glcisk-path
                        "--scalar-style"
                        "plain"
                        "--gl-access-token"
                        (glcisk-get-token)
                        "--ci-file"
                        ci-file
                        "focus"
                        job
                        "--no-preamble")
                       " ")))
    (shell-command cmd buffer)))

(defun gitlab-ci-focused-job-update ()
  ""
  (interactive)
  (read-only-mode 0)
  (erase-buffer)
  (glcisk-focus-raw glci-ci-file glci-focused-job (current-buffer))
  (read-only-mode))

(define-minor-mode gitlab-ci-focused-job-mode
  "GitLab CI: Focused job"
  :global nil
  :lighter
  "glj"
  (define-key gitlab-ci-focused-job-mode-map (kbd "q") 'quit-window)
  (define-key
   gitlab-ci-focused-job-mode-map
   (kbd "g")
   'gitlab-ci-focused-job-update))

(defun glcisk-focus ()
  ""
  (interactive)
  (let* ((ci-file (glci-find-ci-file))
         (current-job (glci-current-job))
         (b
          (get-buffer-create
           (concat "*glcisk focus: " current-job "*"))))
    (glcisk-focus-raw ci-file current-job b)
    (with-current-buffer b
      (read-only-mode)
      (gitlab-ci-mode)
      (gitlab-ci-focused-job-mode)
      (setq-local glci-ci-file ci-file)
      (setq-local glci-focused-job current-job))
    (display-buffer b)))

(defun glcisk-lint ()
  ""
  (interactive)
  (let* ((ci-file (glci-find-ci-file))
         (cmd
          (string-join (list
                        glcisk-path
                        "--gl-access-token"
                        (glcisk-get-token)
                        "--ci-file"
                        ci-file
                        "lint")
                       " ")))
    (shell-command cmd)))

(defun glcisk-visualize-pipeline-raw (ci-file buffer)
  ""
  (interactive)
  (let* ((cmd
          (string-join (list
                        glcisk-path
                        "--gl-access-token"
                        (glcisk-get-token)
                        "--ci-file"
                        ci-file
                        "visualize"
                        "pipeline")
                       " ")))
    (shell-command cmd buffer)))


(defun glcisk-visualize-pipeline ()
  ""
  (interactive)
  (let* ((ci-file (glci-find-ci-file))
         (b
          (get-buffer-create (concat "*glcisk visualize pipeline*"))))
    ;; (let ((max-mini-window-height 0))
    (let ((resize-mini-windows nil))
      (glcisk-visualize-pipeline-raw ci-file b))
    (with-current-buffer b
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode)
      ;; (setq-local glci-ci-file ci-file)
      ;; (setq-local glci-focused-job current-job)
      )
    (display-buffer b)))


(defun arvid-gitlab-ci-mode-hook ()
  ""
  (setq-local beginning-of-defun-function 'glci-beginning-of-defun)
  (setq-local end-of-defun-function 'glci-end-of-defun))

(use-package
 gitlab-ci-mode
 :ensure t
 :config
 (add-to-list
  'auto-mode-alist '("\\.gitlab/.*.yml\\'" . gitlab-ci-mode))
 (add-to-list 'gitlab-ci-mode-hook 'arvid-gitlab-ci-mode-hook))

(provide 'arvid-gitlab-ci)
