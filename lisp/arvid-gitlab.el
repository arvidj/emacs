(use-package gitlab
  :commands aj/gitlab-start-review
  :ensure t)

(setq gitlab-host "https://gitlab.com"
      gitlab-token-id

      ;; See https://www.gnu.org/software/emacs/manual//html_mono/auth.html#Secret-Service-API
      ;; and http://manpages.ubuntu.com/manpages/bionic/man1/secret-tool.1.html
      (secrets-get-secret
       "login"
       "gitlab access-tokens: arvid-gitlab.el (read_api)")
      aj/gitlab-tezos-project-id 3836952
      aj/laboxy-org-file "/home/arvid/Dropbox/Jobb/Nomadic_Labs/Org/laboxy.org"
      )

(setq org-use-property-inheritance '("xy_project" "xy_category"))

(defun gitlab--get-merge-request-uri (project-id mr-id)
  "Retrieve URI to retrieve an issue.
PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (s-concat "projects/"
            (url-hexify-string
             (format "%s" project-id))
            "/merge_requests/"
            mr-id))


(defun gitlab-get-merge-request (project-id mr-id)
  "Gets a single project issue.

PROJECT-ID : The ID of a project
ISSUE-ID : The ID of a project issue"
  (perform-gitlab-request "GET"
                          (gitlab--get-merge-request-uri
                           (url-hexify-string
                            (format "%s" project-id))
                           (format "%s" mr-id))
                          nil
                          200))

;; (gitlab-get-merge-request aj/gitlab-tezos-project-id 1000)

(defun aj/laboxy-go-to-heading (heading-title)
  (outline-show-all)
  (beginning-of-buffer)
  ;; if before the first heading, go to it
  (condition-case nil
      (org-back-to-heading)
    (error (outline-next-heading)))
  (while (and
          (not (string= (arvid-wl/org-heading-title) heading-title))
          (outline-next-heading))))

(defun aj/laboxy-review-at-point ()
	""
  (let ((title (arvid-wl/org-heading-title)))
    (and (string-match "^!\\([[:digit:]]+\\)" title)
         (string-to-number (match-string 1 title)))))

(defun aj/laboxy-find-review-heading (mr-id)
	""

    (aj/laboxy-go-to-heading "Review")

    (save-excursion
      (let ((level (org-outline-level)))
        (catch 'found
          (while
              (and
               (outline-next-heading)
               (= (org-outline-level) (+ level 1)))
            (if (let ((mr-id-at-point (aj/laboxy-review-at-point)))
                   (and mr-id-at-point (= mr-id-at-point mr-id)))
                (throw 'found (point))))
          nil))))

(defun aj/gitlab-start-review (mr-id)
  (interactive "nMerge request id: ")
  (let* ((mr (gitlab-get-merge-request aj/gitlab-tezos-project-id mr-id))
         (title (assoc-default 'title mr)))
    (find-file aj/laboxy-org-file)
    (if-let ((review-point (aj/laboxy-find-review-heading mr-id)))
        (progn
          (goto-char review-point)
          (org-clock-in)
          (message "Clocked into existing review heading: %s" (org-display-outline-path nil t)))
      (aj/laboxy-go-to-heading "Review")
      (outline-get-next-sibling)
      (insert (format "** !%d - %s\n" mr-id title))
      (previous-line)
      (org-clock-in)
      (message "Clocked into new review heading: %s" (org-display-outline-path nil t)))

    ))

(defun aj/laboxy-set-proj-cat (project category)
	""
  (interactive (list nil nil))
  (let* ((project (org-read-property-value "xy_project"))
         (category (org-read-property-value "xy_category")))
    (org-set-property "xy_project" project)
    (org-set-property "xy_category" category)))


(provide 'arvid-gitlab)
