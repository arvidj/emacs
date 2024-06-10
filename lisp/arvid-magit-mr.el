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
;; - write a lot of tests
;; - write a lot of documentation
;; - make interactive arguments make more sense
;; - do a lot of refactoring
;; - consider the possibility of only using sync functions


;; Get current branch (magit-get-current-branch)
;; Get current MR

(require 'magit)
(require 'glab)
(require 'names)
(require 's)
(require 'el-mock)

;; Setting up a token
;; 1. set 'git config --global gitlab.user USERNAME'
;; 2. add 'machine gitlab.com/api/v4 login USERNAME^magit-mr password glpat-TOKEN' to .authinfo

(defun magit-glab/url-encode-project-id (project-id)
  "Encode a GitLab PROJECT-ID as a string.

A GitLab PROJECT-ID can either be a string on the form
NAMESPACE/PROJECT. In this case, the return value is the
URL-encoding of this string. The PROJECT-ID can be an integer, in
which case the return value is the decimal representation of the
integer as a string."
  (if (numberp project-id)
      (number-to-string project-id)
    (url-hexify-string project-id)))

(ert-deftest magit-glab/test-url-encode-project-id ()
  (should (equal "tezos%2Ftezos" (magit-glab/url-encode-project-id "tezos/tezos")))
  (should (equal "1234" (magit-glab/url-encode-project-id 1234))))

(defun magit-glab/url-mr (project-id mr-iid)
  "Path to GitLab API's `Get single MR' endpoint for PROJECT-ID and MR-IID.

Return path to the GitLab API's `Get single MR' endpoint for a
given PROJECT-ID (which can be a string on the form
NAMESPACE/PROJECT or integral) and an integer MR-IID.

For more information, see URL
`https://docs.gitlab.com/ee/api/merge_requests.html#get-single-mr'."
  (concat
   "/projects/"
   (magit-glab/url-encode-project-id project-id)
   "/merge_requests/"
   (number-to-string mr-iid)))

(ert-deftest magit-glab/test-url-mr ()
  (should (equal "/projects/tezos%2Ftezos/merge_requests/1234" (magit-glab/url-mr "tezos/tezos" 1234)))
  (should (equal "/projects/123/merge_requests/456" (magit-glab/url-mr 123 456))))

(defvar magit-glab/GET-cache-file "/tmp/.magit-glab-cache.el"
  "Path to magit-glab's cache")

(defvar magit-glab/GET-cache (make-hash-table :test 'equal)
  "Hash table for storing memoized results.")

(defun magit-glab/hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to a alist."
  (let ((alist nil))
    (maphash
     (lambda (k v) (setq alist (cons (cons k v) alist))) hash-table)
    alist))

;; TODO: save and load the cache on start / shut down
(defun magit-glab/save-cache ()
  "Save the memoized computations hash table to disk."
  (with-temp-file magit-glab/GET-cache-file
    (prin1 (magit-glab/hash-table-to-alist magit-glab/GET-cache)
           (current-buffer))))

(defun magit-glab/load-cache ()
  "Load the memoized computations hash table from disk."
  (let ((file-content
         (when (file-exists-p magit-glab/GET-cache-file)
           (with-temp-buffer
             (insert-file-contents magit-glab/GET-cache-file)
             (read (current-buffer))))))
    (when file-content
      (clrhash magit-glab/GET-cache) ; Clear current hash table
      (cl-loop
       for
       (key . value)
       in
       file-content
       do
       (puthash key value magit-glab/GET-cache)))))

(cl-defun magit-glab/get (resource params &key no-cache callback errorback)
  "Make a request for RESOURCE with caching and return the response body.

This function is as `ghub-request' with METHOD set to GET, and
the arguments RESOURCE, PARAMS, CALLBACK and ERRORBACK has the
same meaning.  However, if the request is successful, the
response is cached in `magit-glab/GET-cache'.  Subsequent calls
to `magit-glab/get', with exactly matching REQUEST and PARAMS,
return the same value unless NO-CACHE is non-nil.  When a cached
value is returned, all argument except the first passed to
CALLBACK are nil."
  (if no-cache
      (ghub-request
       "GET"
       resource
       params
       :auth 'magit-mr
       :forge 'gitlab
       :callback callback
       :errorback errorback)
    (if-let (cached-value
             (gethash (list resource params) magit-glab/GET-cache))
      ;; Value found in cache
      (if callback
          (funcall callback cached-value nil nil nil)
        cached-value)
      ;; No value in cache
      (if (or callback errorback)
          ;; If asynchronous
          (ghub-request
           "GET" resource params
           :auth 'magit-mr
           :forge 'gitlab
           :callback
           (lambda (resp header status req)
             (puthash (list resource params) resp magit-glab/GET-cache)
             (funcall callback resp header status req))
           :errorback errorback)
        ;; If synchronous
        (if-let (value
                 (ghub-request
                  "GET"
                  resource
                  params
                  :auth 'magit-mr
                  :forge 'gitlab))
          (puthash (list resource params) value magit-glab/GET-cache))))))


(ert-deftest magit-glab/test-get-sync ()
  "Test synchronous version of magit-glab/get"
  (let ((magit-glab/GET-cache (make-hash-table :test 'equal) ))
	(with-mock
      (stub ghub-request => "18.0")
      (should (equal "18.0" (magit-glab/get "/version" nil))))
    ;; Stub each function at most once per with-mock form.
    (with-mock
      (stub ghub-request => "17.0")
      (should (equal "18.0" (magit-glab/get "/version" nil)))
      (should (equal "17.0" (magit-glab/get "/version" nil :no-cache t))))))

(ert-deftest magit-glab/test-get-async ()
  "Test asynchronous version of magit-glab/get"
  (let* ((magit-glab/GET-cache (make-hash-table :test 'equal) )
         (ghub-request-original (symbol-function 'ghub-request))
         (ghub-request-mock-response nil)
         (ghub-request-mock (cl-function
                             (lambda (_method _resource _params &key auth forge callback errorback)
                               (ignore auth forge errorback)
                               (funcall callback ghub-request-mock-response nil nil nil)))))
    (unwind-protect
        (progn
          ;; Setup mock
          (fset 'ghub-request ghub-request-mock)
          ;; Start test
          (setq ghub-request-mock-response "18.0")
          (magit-glab/get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; The cached value is returned
          (setq ghub-request-mock-response "17.0")
          (magit-glab/get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; Unless no-cache is t
          (magit-glab/get
           "/version" nil
           :no-cache t
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "17.0")))))
      ;; Restore mock
      (fset 'ghub-request ghub-request-original))))

(cl-defun magit-glab/get1
    (url params &key no-cache callback errorback)
  (let ((resp
         (magit-glab/get
          url params
          :callback
          (when callback
            (lambda (resp) (funcall callback (car resp))))
          :errorback errorback
          :no-cache no-cache)))
    (when (not (or callback errorback))
      (car resp))))

(cl-defun magit-glab/get-user (username &key callback errorback)
  ""
  (magit-glab/get1
   "/users"
   `((username . ,username))
   :callback callback
   :errorback errorback))

(cl-defun magit-glab/get-mr
    (project-id mr-iid &key no-cache callback errorback)
  ""
  (magit-glab/get
   (magit-glab/url-mr project-id mr-iid)
   nil
   :callback callback
   :errorback errorback
   :no-cache no-cache))

(cl-defun magit-glab/get-mr-of-source-branch
    (project-id source-branch &key no-cache callback errorback)
  ""
  (magit-glab/get1
   (concat
    "/projects/"
    (magit-glab/url-encode-project-id project-id)
    "/merge_requests")
   `((source_branch . ,source-branch))
   :callback callback
   :errorback errorback
   :no-cache no-cache))

;; (magit-glab/get-mr-of-source-branch
;;  "tezos/tezos" "arvid@ci-add-cargo-cache")

(cl-defun magit-glab/mr-set-description
    (project-id mr-iid description &key callback errorback)
  ""
  (ghub-request
   "PUT"
   (magit-glab/url-mr project-id mr-iid)
   `((description . ,description))
   :auth 'magit-mr
   :forge 'gitlab
   :callback callback
   :errorback errorback))

(cl-defun magit-glab/mr-set-title
    (project-id mr-iid title &key callback errorback)
  ""
  (ghub-request
   "PUT"
   (magit-glab/url-mr project-id mr-iid)
   `((title . ,title))
   :auth 'magit-mr
   :forge 'gitlab
   :callback callback
   :errorback errorback))

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


;; (magit-glab/get-user "arvidnl")

(defun magit-glab/project-of-remote (remote-url)
  "Extract NAMESPACE/PROJECT from git URLs.

URL is a git repository URL in either of these forms:
- git@gitlab.com:NAMESPACE/PROJECT.git
- https://gitlab.com/NAMESPACE/PROJECT.git

Returns the 'NAMESPACE/PROJECT' part of the URL."
  ;; git@gitlab.com:tezos/tezos.git
  ;; https://gitlab.com/tezos/tezos.git
  (if (string-match
       "\\(git@gitlab\.com:\\|https://gitlab\.com/\\)\\([^/]+/[^/.]+\\)\\.git"
       remote-url)
      (match-string 2 remote-url)
    (error
     "Remote URL '%s' does not match expected format" remote-url)))

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
      (if-let ((remote-url (magit-get (format "remote.%s.url" remote))))
          (magit-glab/project-of-remote remote-url)
        (error
         "The remote '%s' has no url (remote.%s.url is not set)"
         remote remote))
    (error
     "Cannot infer GitLab project: no remote set for this branch, nor is remote.pushDefault set")))

(defvar-local magit-glab/mr nil
  "Merge request under edit in MR description buffers")

(defvar-local magit-glab/project-id nil
  "Project of the merge request under edit in MR description buffers")

;; TODO: should also take a callback
(defun magit-glab/mr-save-description-buffer ()
  ""
  (interactive)
  (let ((project-id magit-glab/project-id)
        (mr magit-glab/mr))
    (magit-glab/mr-set-description
     project-id (alist-get 'iid mr) (buffer-string)
     :callback
     (lambda (_resp _header _status _req)
       (set-buffer-modified-p nil)
       (message "Saving %s!%d: '%s'... Done!"
                project-id
                (alist-get 'iid mr)
                (alist-get 'title mr))))
    (message "Saving %s!%d: '%s'..."
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
(defun magit-glab/read-branch ()
  ""
  (let ((branch
         (or (magit-branch-at-point) (magit-get-current-branch))))
    (if (or current-prefix-arg (not branch))
        (read-string "Branch name: ")
      (magit-glab/strip-remote-prefix branch))))

(defun magit-glab/mr-edit-description (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t)))
    (message "Edit description of %s!%d: '%s'"
             project-id
             (alist-get 'iid mr)
             (alist-get 'title mr))
    (magit-glab/mr-create-description-buffer project-id mr)))

(defun magit-glab/mr-edit-title (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t)))
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

(defun magit-glab/mr-toggle-draft (branch)
  "Toggle the draft status of the MR associate to BRANCH"
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t)))
    (let* ((title (alist-get 'title mr))
           (is-draft (string-match "^\\(Draft: \\)+\\(.*\\)$" title))
           (new-title
            (if is-draft
                (let ((title-no-draft (match-string 2 title)))
	              title-no-draft)
              (concat "Draft: " title))))
      (magit-glab/mr-set-title
       project-id (alist-get 'iid mr) new-title
       :callback (lambda (_resp _header _status _req)
                   (if is-draft
                       (message "Unmarked %s!%d as draft."
                                project-id
                                (alist-get 'iid mr))
                     (message "Marked %s!%d as draft."
                              project-id
                              (alist-get 'iid mr))))
       :errorback (lambda (err _header _status _req)
                    (message
                     "An error occurred when updating the title of %s!%d: %s"
                     project-id (alist-get 'iid mr) err))
       )
      (if is-draft
          (message "Unmarking %s!%d as draft..."
                   project-id
                   (alist-get 'iid mr))
        (message "Marking %s!%d as draft..."
                 project-id
                 (alist-get 'iid mr))))))

(defun magit-glab/decode-assignees (assignee-objs)
  "From assignee objects to list of usernames (strings)"
  (mapcar
   (lambda (assignee-obj) (alist-get 'username assignee-obj))
   assignee-objs))

(defun magit-glab/to-user-id (id-or-username)
  "From ASSIGNEE-ID-OR-USERNAME to numerical user ids."
  (if (numberp id-or-username)
      id-or-username
    (if-let* ((assignee-username
               (string-remove-prefix "@" id-or-username)))
        (alist-get 'id (magit-glab/get-user assignee-username))
      (error
       "Could not find id associated to username '%s' -- do they exist?"
       assignee-username))))

(ert-deftest magit-glab/test-to-user-id ()
  (let ((magit-glab/GET-cache (make-hash-table :test 'equal)))
	(should (equal 4414596 (magit-glab/to-user-id "@arvidnl")))))

(defun magit-glab/format-user-as-candidate (user)
	""
    (let ((username (alist-get 'username user))
          (name (alist-get 'name user))
          (id (alist-get 'id user)))
      (cons (format "%s (@%s)" name username) id)))

(defun magit-glab/mr-edit-assignees (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t)))
    (if (not mr)
        (error
         "Couldn't find MR for branch '%s' in project '%s'"
         branch
         project-id)
      (let* ((current-assignees (mapcar #'magit-glab/format-user-as-candidate (alist-get 'assignees mr)))
             (candidate-assignees
              (append current-assignees
                      (mapcar #'magit-glab/format-user-as-candidate
                              (cons (alist-get 'author mr)
                                    (alist-get 'reviewers mr)))))
             (new-assignees
              (seq-uniq
               (completing-read-multiple
                ;; prompt
                (format
                 "Set assignees of %s!%d (space-separated GitLab usernames): "
                 project-id (alist-get 'iid mr))
                ;; table
                candidate-assignees
                nil ;; predicate
                nil ;; require-match
                ;; initial-input
                (if current-assignees
                    (concat (s-join ", " (mapcar #'car current-assignees)) ", ")
                  nil))))
             )
        (magit-glab/mr-set-assignees
         project-id
         (alist-get 'iid mr)
         (mapcar
          #'magit-glab/to-user-id
          (mapcar
           (lambda (selection) (or (cdr (assoc selection candidate-assignees)) selection))
           new-assignees))
         :callback
         (lambda (_resp _header _status _req)
           (if new-assignees
               (message (format "Updated assignees of %s!%d to: %s"
                                project-id (alist-get 'iid mr)
                                (s-join ", " new-assignees)))
             (message (format "Removed all assignees of %s!%d."
                              project-id (alist-get 'iid mr)))))
         :errorback
         (lambda (err _header _status _req)
           (message
            "An error occurred when updating the assignees of %s!%d: %s"
            project-id (alist-get 'iid mr) err)))
        (message "Setting assignees...")))))

(defun magit-glab/mr-assign-to-me (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t))
         (my-username (concat "@" (ghub--username nil 'gitlab))))
    (magit-glab/mr-set-assignees
     project-id
     (alist-get 'iid mr)
     (magit-glab/to-user-id my-username)
     :callback
     (lambda (_resp _header _status _req)
       (message (format "Updated assignees of %s!%d to: %s"
                        project-id (alist-get 'iid mr)
                        my-username)))
     :errorback
     (lambda (err _header _status _req)
       (message
        "An error occurred when updating the assignees of %s!%d: %s"
        project-id (alist-get 'iid mr) err)))
    (message "Setting assignees to %s..." my-username)))


(defun magit-glab/mr-assign-to-author (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t))
         (author-username (concat "@" (alist-get 'username (alist-get 'author mr))))
         (author-id (alist-get 'id (alist-get 'author mr))))
    (magit-glab/mr-set-assignees
     project-id
     (alist-get 'iid mr)
     (list author-id)
     :callback
     (lambda (_resp _header _status _req)
       (message (format "Updated assignees of %s!%d to: %s"
                        project-id (alist-get 'iid mr)
                        author-username)))
     :errorback
     (lambda (err _header _status _req)
       (message
        "An error occurred when updating the assignees of %s!%d: %s"
        project-id (alist-get 'iid mr) err)))
    (message "Setting assignees to %s..." author-username)))

(defun magit-glab/mr-assign-to-reviewers (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr
          (magit-glab/get-mr-of-source-branch
           project-id
           branch
           :no-cache t))
         (reviewers
          (mapcar #'magit-glab/format-user-as-candidate
                  (alist-get 'reviewers mr))))
    (if (not reviewers)
        (error "This MR has no reviewers!")
      (magit-glab/mr-set-assignees
       project-id
       (alist-get 'iid mr)
       (mapcar #'cdr reviewers)
       :callback
       (lambda (_resp _header _status _req)
         (message (format "Updated assignees of %s!%d to: %s"
                          project-id (alist-get 'iid mr)
                          (s-join ", " (mapcar #'car reviewers)))))
       :errorback
       (lambda (err _header _status _req)
         (message
          "An error occurred when updating the assignees of %s!%d: %s"
          project-id (alist-get 'iid mr) err))))
    (message "Setting assignees...")))

(defun magit-glab/mr-browse (branch)
  ""
  (interactive (list (magit-glab/read-branch)))
  (let* ((project-id (magit-glab/infer-project-id branch))
         (mr (magit-glab/get-mr-of-source-branch project-id branch)))
    (browse-url (alist-get 'web_url mr))))

(defun magit-glab/todo ()
	""
  (interactive)
  (error "TODO"))

(defun test-function (&optional args)
  (interactive
   (list (transient-args 'magit-glab/mr-assign-to-favorite)))
  (message "args: %s" args))

(defcustom
  magit-glab/favorite-users
  nil
  "A list of GitLab users favorite users.

Should be a list of values, where each value is on the list
PREFIX DESCRIPTION GITLAB_USERNAME.

For the prefix, use lower-case letters. Each prefix should be unique. Example:

'((\"i\" \"Myself\" \"@arvidnl\")
  (\"j d\" \"John Doe\" \"@...\"))
")

(defun magit-glab/mr-assign-to-favorite--set ()
  ""
  (interactive)
  (if-let (assignees (transient-args 'magit-glab/mr-assign-to-favorite))
      (let* ((branch (magit-glab/read-branch))
             (project-id (magit-glab/infer-project-id branch))
             (mr
              (magit-glab/get-mr-of-source-branch
               project-id
               branch
               :no-cache t)))
        (magit-glab/mr-set-assignees
         project-id
         (alist-get 'iid mr)
         (mapcar #'magit-glab/to-user-id assignees)
         :callback
         (lambda (_resp _header _status _req)
           (message (format "Updated assignees of %s!%d to: %s"
                            project-id (alist-get 'iid mr)
                            (s-join ", " assignees))))
         :errorback
         (lambda (err _header _status _req)
           (message
            "An error occurred when updating the assignees of %s!%d: %s"
            project-id (alist-get 'iid mr) err))))
    (error "Select a non-empty set of favorites first.")))

;; (defun magit-glab/mr-assign-to-favorite--add ()
;;   ""
;;   (interactive)
;;   (if-let (new-assignees (transient-args 'magit-glab/mr-assign-to-favorite))
;;       (let* ((branch (magit-glab/read-branch))
;;              (project-id (magit-glab/infer-project-id branch))
;;              (mr
;;               (magit-glab/get-mr-of-source-branch
;;                project-id
;;                branch
;;                :no-cache t))
;;              (current-assignees
;;               (mapcar #'magit-glab/to-user-id
;;                       (alist-get 'assignees mr)))
;;              (new-assignees
;;               (mapcar #'magit-glab/decode-assignee
;;                       new-assignees))
;;              (all-assignees (seq-uniq (append current-assignees new-assignees))))
;;         (magit-glab/mr-set-assignees
;;          project-id
;;          (alist-get 'iid mr)
;;          all-assignees
;;          :callback
;;          (lambda (_resp _header _status _req)
;;            (message (format "Updated assignees of %s!%d to: %s"
;;                             project-id (alist-get 'iid mr)
;;                             (s-join ", " all-assignees))))
;;          :errorback
;;          (lambda (err _header _status _req)
;;            (message
;;             "An error occurred when updating the assignees of %s!%d: %s"
;;             project-id (alist-get 'iid mr) err))))
;;     (error "Select a non-empy set of favorites first.")))

(defun magit-glab/mr-assign-to-favorite--setup-children (_)
	""
    (transient-parse-suffixes
     'magit-glab/mr-assign-to-favorite
     magit-glab/favorite-users))

(defun magit-glab/customize-favorites ()
	""
  (interactive)
  (customize-variable 'magit-glab/favorite-users))

(transient-define-prefix
  magit-glab/mr-assign-to-favorite () "Assign MR to a favorite user."
  [["Favorites"
    :if (lambda () magit-glab/favorite-users)
    :setup-children magit-glab/mr-assign-to-favorite--setup-children
    ]
   ["Handle favorites"
    ("C" "customize favorites" magit-glab/customize-favorites)]]
  ["Actions"
   :if (lambda () magit-glab/favorite-users)
   ("S" "set" magit-glab/mr-assign-to-favorite--set)
   ("A" "add" magit-glab/todo)])

(transient-define-prefix
 magit-glab/mr () "Act on a GitLab merge request."
 [:if
  (lambda () (or (magit-branch-at-point) (magit-get-current-branch)))
  :description
  (lambda ()
    (let ((branch
           (or (magit-branch-at-point) (magit-get-current-branch))))
      (concat
       (propertize "Act on GitLab merge request for "
                   'face
                   'transient-heading)
       (propertize branch 'face 'magit-branch-local)
       ":\n")))
  ["Edit"
   ("t" "title" magit-glab/mr-edit-title)
   ("d" "description" magit-glab/mr-edit-description)
   ("m" "milestone" magit-glab/todo)
   ("D" "toggle draft status" magit-glab/mr-toggle-draft)
   ("l" "labels" magit-glab/todo)
   ]
  ["Assignees"
   ("a a" "edit assignees" magit-glab/mr-edit-assignees)
   ("a m" "assign to me" magit-glab/mr-assign-to-me)
   ("a A" "assign to author" magit-glab/mr-assign-to-author)
   ("a r" "assign to reviewers" magit-glab/mr-assign-to-reviewers)
   ("a f" "assign to favorite" magit-glab/mr-assign-to-favorite)]
  ["Reviewers"
   ("r r" "edit reviewers" magit-glab/todo)]
  ["Actions"
   ("v" "view on forge" magit-glab/mr-browse)
   ]])


;; Update magit-mode-map such that pressing @ opens the magit-glab/mr transient

(define-key magit-mode-map (kbd "@") 'magit-glab/mr)

(provide 'arvid-magit-mr)
