;;; arvid-magit-mr.el --- Magit plugin for manipulating GitLab merge requests  -*- lexical-binding: t -*-

;; TODO
;; - updating merge requests
;;   - updating descriptions
;;     - pressing @ brings upp the arvid-magit-mr transient
;;     - it contains an entry d (edit-description)
;;     - running this command opens a buffer containing merge request description
;;     - saving this buffer updates the merge request definition.
;;     - closing it C-c C-c updates the merge request definition and closes the buffer
;;     - closing it C-c C-k the closes the buffer without updating merge request
;; - add some kind of cache
;; - make some operations asynchronous


;; Get current branch (magit-get-current-branch)
;; Get current MR


;; (require 'ghub)
(require 'magit)
(require 'glab)
(require 'names)
(require 's)

;; Setting up a token
;; 1. set 'git config --global gitlab.user USERNAME'
;; 2. add 'machine gitlab.com/api/v4 login USERNAME^magit-mr password glpat-TOKEN' to .authinfo

(defun magit-glab/url-encode-project-id (project_id)
  "Encode a "
  (if (numberp project_id)
      (number-to-string project_id)
    (url-hexify-string project_id)))

(defun magit-glab/url-mr (project-id mr-iid)
  ""
  (concat
   "/projects/"
   (magit-glab/url-encode-project-id project-id)
   "/merge_requests/"
   (number-to-string mr-iid)))

;; (magit-glab/url-mr "tezos/tezos" 12345)

(defun magit-glab/get-mr (project-id mr-iid)
  ""
  (ghub-request
   "GET"
   (magit-glab/url-mr project-id mr-iid)
   nil
   :auth 'magit-mr
   :forge 'gitlab))

(defun magit-glab/get-mr-of-source-branch (project-id source-branch)
  ""
  (car
   (ghub-request
    "GET"
    (concat
     "/projects/"
     (magit-glab/url-encode-project-id project-id)
     "/merge_requests")
    `((source_branch . ,source-branch))
    :auth 'magit-mr
    :forge 'gitlab)))

;; (magit-glab/get-mr-of-source-branch
;;  "tezos/tezos" "arvid@ci-add-cargo-cache")

(defun magit-glab/mr-set-description (project-id mr-iid description)
  ""
  (ghub-request
   "PUT"
   (magit-glab/url-mr project-id mr-iid)
   `((description . ,description))
   :auth 'magit-mr
   :forge 'gitlab))

(defun magit-glab/mr-set-title (project-id mr-iid title)
  ""
  (ghub-request
   "PUT"
   (magit-glab/url-mr project-id mr-iid)
   `((title . ,title))
   :auth 'magit-mr
   :forge 'gitlab))

(cl-defun magit-glab/mr-set-assignees
    (project-id mr-iid assignee-ids &key callback errorback)
  ""
  (ghub-request
   "PUT"
   (magit-glab/url-mr project-id mr-iid)
   `((assignee_ids . ,assignee-ids))
   :auth 'magit-mr
   :forge 'gitlab
   :callback callback
   :errorback errorback))

;; (magit-glab/mr-set-assignees
;;  "nomadic-labs/arvid-tezos"
;;  541
;;  '(4414596)
;;  :callback
;;  (lambda (resp header status req) (message "Success")))

;; (magit-glab/mr-set-assignees
;;  "nomadic-labs/arvid-tezos"
;;  9999
;;  '(4414596)
;;  :callback
;;  (lambda (resp header status req) (message "Success"))
;;  :errorback
;;  (lambda (err header status req) (message "Error: %s" err)))


(defun magit-glab/get-user (username)
  ""
  (car
   (ghub-request
    "GET"
    "/users"
    `((username . ,username))
    :auth 'magit-mr
    :forge 'gitlab)))

;; (magit-glab/get-user "arvidnl")

(defun magit-glab/project-of-remote (remote)
  "Extract NAMESPACE/PROJECT from git URLs.

URL is a git repository URL in either of these forms:
- 'git@gitlab.com:NAMESPACE/PROJECT.git'
- 'https://gitlab.com/NAMESPACE/PROJECT.git'

Returns the 'NAMESPACE/PROJECT' part of the URL."
  (interactive)
  ;; git@gitlab.com:tezos/tezos.git
  ;; https://gitlab.com/tezos/tezos.git
  (if-let ((remote-url (magit-get (format "remote.%s.url" remote))))
    (if (string-match
         "\\(git@gitlab\.com:\\|https://gitlab\.com/\\)\\([^/]+/[^/.]+\\)\\.git"
         remote-url)
        (match-string 2 remote-url)
      (error
       "Remote URL '%s' does not match expected format" remote-url))
    (error
     "The remote '%s' has no url (remote.%s.url is not set)"
     remote
     remote)))

;; Examples of usage:
;; (magit-glab/project-of-remote "git@gitlab.com:NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (magit-glab/project-of-remote "https://gitlab.com/NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (magit-glab/project-of-remote "https://google.com")

(defun magit-glab/infer-project-id (branch)
  ""
  (if-let (remote
           (or (let ((branch_remote
                      (magit-get (format "branch.%s.remote" branch))))
                 (unless (string= branch_remote ".")
                   branch_remote))
               (let ((branch-push-remote
                      (magit-get
                       (format "branch.%s.pushRemote" branch))))
                 (unless (string= branch-push-remote ".")
                   branch-push-remote))
               (magit-get-current-remote)
               (magit-get "remote.pushDefault")))
    (magit-glab/project-of-remote remote)
    (error
     "Cannot infer GitLab project: no remote set for this branch, nor is remote.pushDefault set")))

(defvar-local magit-glab/mr nil
  "Merge request under edit in MR description buffers")

(defvar-local magit-glab/project-id nil
  "Project of the merge request under edit in MR description buffers")

(defun magit-glab/mr-save-description-buffer ()
  ""
  (interactive)
  (let ((project-id magit-glab/project-id)
        (mr magit-glab/mr))
    (message "Saving %s!%d: '%s'..."
             project-id
             (alist-get 'iid mr)
             (alist-get 'title mr))
    (magit-glab/mr-set-description
     project-id (alist-get 'iid mr) (buffer-string))
    (set-buffer-modified-p nil)
    (message "Saving %s!%d: '%s'... Done!"
             project-id
             (alist-get 'iid mr)
             (alist-get 'title mr))))

(defun magit-glab/mr-save-and-close-description-buffer ()
  ""
  (interactive)
  (magit-glab/mr-save-description-buffer)
  (magit-kill-this-buffer))

(defun magit-glab/mr-cancel-description-buffer ()
  ""
  (interactive)
  (let ((project-id magit-glab/project-id)
        (mr magit-glab/mr))
    (magit-kill-this-buffer)
    (message "Description edit of %s!%d: '%s' cancelled"
             project-id
             (alist-get 'iid mr)
             (alist-get 'title mr))))

(defun magit-glab/mr-create-description-buffer (project-id mr)
  ""
  ;; Generate a unique buffer name
  (let* ((base-name
          (format "Edit description of %s!%d: '%s'"
                  project-id
                  (alist-get 'iid mr)
                  (alist-get 'title mr)))
         (buffer-name base-name)
         (index 1))
    (while (get-buffer buffer-name)
      (setq buffer-name (format "%s<%d>" base-name index))
      (setq index (1+ index)))
    ;; Create and switch to the buffer
    (switch-to-buffer (get-buffer-create buffer-name))
    ;; Insert the specified content
    (insert (alist-get 'description mr))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    ;; Activate the specified major mode
    (markdown-mode)
    ;; Set up local keybindings for this buffer
    (use-local-map (copy-keymap (current-local-map)))
    (setq magit-glab/mr mr)
    (setq magit-glab/project-id project-id)
    (keymap-local-set
     "C-c C-c" #'magit-glab/mr-save-and-close-description-buffer)
    (keymap-local-set
     "C-c C-s" #'magit-glab/mr-save-description-buffer)
    (keymap-local-set
     "C-x C-s" #'magit-glab/mr-save-description-buffer)
    (keymap-local-set
     "C-c C-k" #'magit-glab/mr-cancel-description-buffer)
    ;; Return the newly created buffer
    buffer-name))


(defun magit-glab/strip-remote-prefix (branch-name)
  "Strip the remote prefix from BRANCH-NAME if present."
  (let ((components (split-string branch-name "/")))
    (if (> (length components) 1)
        (mapconcat 'identity (cdr components) "/") ; Rejoin the rest if more than one slash exists
      branch-name))) ; Return the original if no slash found

;; By default, get current branch or branch-at-point. If prefix is
;; given or if both those values are nil, then read a value instead.
(defun magit-glab/read-mr ()
  ""
  (let ((branch
         (or (magit-branch-at-point) (magit-get-current-branch))))
    (if (or current-prefix-arg (not branch))
        (read-string "Branch name: ")
      (magit-glab/strip-remote-prefix branch))))

(defun magit-glab/mr-edit-description (branch)
  ""
  (interactive (list (magit-glab/read-mr)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr (magit-glab/get-mr-of-source-branch project-id branch)))
    (message "Edit description of %s!%d: '%s'"
             project-id
             (alist-get 'iid mr)
             (alist-get 'title mr))
    (magit-glab/mr-create-description-buffer project-id mr)))

(defun magit-glab/mr-edit-title (branch)
  ""
  (interactive (list (magit-glab/read-mr)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr (magit-glab/get-mr-of-source-branch project-id branch)))
    (when-let (new-title
               (read-string (format "Edit title of %s!%d: "
                                    project-id
                                    (alist-get 'iid mr))
                            (alist-get 'title mr)))
      (magit-glab/mr-set-title
       project-id (alist-get 'iid mr) new-title)
      (message "Updated title of %s!%d."
               project-id
               (alist-get 'iid mr)))))

(defun magit-glab/decode-assignees (assignee-objs)
  "From assignee objects to list of usernames (strings)"
  (mapcar
   (lambda (assignee-obj) (alist-get 'username assignee-obj))
   assignee-objs))

(defun magit-glab/encode-assignees (assignee-usernames)
  "From usernames to list of numerical ids"
  (mapcar
   (lambda (assignee-username)
     (if-let* ((assignee-username
                (string-remove-prefix "@" assignee-username))
               (user (magit-glab/get-user assignee-username)))
       (alist-get 'id (magit-glab/get-user assignee-username))
       (error
        "Could not find id associated to username '%s' -- do they exist?"
        assignee-username)))
   assignee-usernames))

(defun magit-glab/mr-edit-assignees (branch)
  ""
  (interactive (list (magit-glab/read-mr)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr (magit-glab/get-mr-of-source-branch project-id branch)))
    (if (not mr)
        (error
         "Couldn't find MR for branch '%s' in project '%s'"
         branch
         project-id)
      (let ((assignees
             (magit-glab/decode-assignees (alist-get 'assignees mr))))
        (when-let
            ((new-assignees
              (read-string
               (format
                "Set assignees of %s!%d (space-separated GitLab usernames): "
                project-id (alist-get 'iid mr))
               (s-join " " assignees))))
          (magit-glab/mr-set-assignees
           project-id
           (alist-get 'iid mr)
           (magit-glab/encode-assignees (s-split " " new-assignees))
           :callback
           (lambda (_resp _header _status _req)
             (message "Updated assignees of %s!%d."
                      project-id (alist-get 'iid mr)))
           :errorback
           (lambda (err _header _status _req)
             (message
              "An error occurred when updating the assignees of %s!%d: %s"
              project-id (alist-get 'iid mr) err)))
          (message "Settings assignees..."))))))

(defun magit-glab/mr-browse (branch)
  ""
  (interactive (list (magit-glab/read-mr)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr (magit-glab/get-mr-of-source-branch project-id branch)))
    (browse-url (alist-get 'web_url mr))))

(transient-define-prefix
 magit-glab/mr () "Act on a GitLab merge request."
 [:if
  (lambda () (or (magit-branch-at-point) (magit-get-current-branch)))
  :description
  (lambda ()
    (let ((branch
           (or (magit-branch-at-point) (magit-get-current-branch))))
      (concat
       (propertize "Update GitLab merge request for "
                   'face
                   'transient-heading)
       (propertize branch 'face 'magit-branch-local))))
  ("d" "description" magit-glab/mr-edit-description)
  ("t" "title" magit-glab/mr-edit-title)
  ("v" "view on forge" magit-glab/mr-browse)
  ("a" "edit assignee(s)" magit-glab/mr-edit-assignees)])


;; Update magit-mode-map such that pressing @ opens the magit-glab/mr transient

(define-key magit-mode-map (kbd "@") 'magit-glab/mr)

(provide 'arvid-magit-mr)
