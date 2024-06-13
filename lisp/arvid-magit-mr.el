;;; arvid-magit-mr.el --- Magit plugin for manipulating GitLab merge requests  -*- lexical-binding: t -*-

;; TODO
;; - updating merge requests
;;   - Set labels
;;   - Set milestone
;; - write a lot of tests
;; - write a lot of documentation
;; - make interactive arguments make more sense
;; - do a lot of refactoring
;; - consider the possibility of only using sync functions
;; - stuff with pipelines: go to head pipeline, trigger manual
;; - write readme.ml
;; - submit to melpa
;; - setup repository (and figure out how to use it with "straight"?)
;; - write a real header
;; - cache "known labels / users / milestones"
;; - make a real minor mode for writing descriptions

;; Setting up a token
;; 1. set 'git config --global gitlab.user USERNAME'
;; 2. add 'machine gitlab.com/api/v4 login USERNAME^magit-mr password glpat-TOKEN' to .authinfo


(require 'magit)
(require 'glab)
(require 'names)
(require 's)
(require 'el-mock)

;; Customization

(defgroup magit-glab nil
  "Manipulating GitLab merge requests from Emacs"
  :prefix "magit-glab-"
  :group 'convenience)

(defcustom mg-favorite-users nil
  "A list of GitLab favorite users for easy access.

Should be a list of values, where each value is on the list
PREFIX DESCRIPTION GITLAB_USERNAME.

For the prefix, use lower-case letters. Each prefix should be unique.

Example:
  \\='((\"i\" \"Myself\" \"@arvidnl\")
    (\"j d\" \"John Doe\" \"@...\"))"
  :group 'magit-glab
  :type '(repeat
          (list
           (string :tag "Prefix")
           (string :tag "Full name for display")
           (string :tag "GitLab username (include @)"))))

(defun mg-url-encode-project-id (project-id)
  "Encode a GitLab PROJECT-ID as a string.

A GitLab PROJECT-ID can either be a string on the form
NAMESPACE/PROJECT. In this case, the return value is the
URL-encoding of this string. The PROJECT-ID can be an integer, in
which case the return value is the decimal representation of the
integer as a string."
  (if (numberp project-id)
      (number-to-string project-id)
    (url-hexify-string project-id)))

(ert-deftest mg-test-url-encode-project-id ()
  (should (equal "tezos%2Ftezos" (mg-url-encode-project-id "tezos/tezos")))
  (should (equal "1234" (mg-url-encode-project-id 1234))))

(defun mg--url-mr (project-id mr-iid)
  "Path to GitLab API's `Get single MR' endpoint for PROJECT-ID and MR-IID.

Return path to the GitLab API's `Get single MR' endpoint for a
given PROJECT-ID (which can be a string on the form
NAMESPACE/PROJECT or integral) and an integer MR-IID.

For more information, see URL
`https://docs.gitlab.com/ee/api/merge_requests.html#get-single-mr'."
  (concat
   "/projects/"
   (mg-url-encode-project-id project-id)
   "/merge_requests/"
   (number-to-string mr-iid)))

(defun mg--url-project (project-id)
  "Path to GitLab API's `Get single project' endpoint for PROJECT-ID.

Return path to the GitLab API's `Get single project' endpoint for a
given PROJECT-ID (which can be a string on the form
NAMESPACE/PROJECT or integral).

For more information, see URL
`https://docs.gitlab.com/ee/api/projects.html#get-single-project'."
  (concat
   "/projects/"
   (mg-url-encode-project-id project-id)))

(ert-deftest mg--test-url-mr ()
  (should (equal "/projects/tezos%2Ftezos/merge_requests/1234" (mg--url-mr "tezos/tezos" 1234)))
  (should (equal "/projects/123/merge_requests/456" (mg--url-mr 123 456))))

(defvar mg--GET-cache-file "/tmp/.magit-glab-cache.el"
  "Path to magit-glab's cache")

(defvar mg--GET-cache (make-hash-table :test 'equal)
  "Hash table for storing memoized results.")

(cl-defun mg--get (resource params &key no-cache callback errorback)
  "Make a request for RESOURCE with caching and return the response body.

This function is as `ghub-request' with METHOD set to GET, and
the arguments RESOURCE, PARAMS, CALLBACK and ERRORBACK has the
same meaning.  However, if the request is successful, the
response is cached in `mg--GET-cache'.  Subsequent calls
to `mg--get', with exactly matching REQUEST and PARAMS,
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
             (gethash (list resource params) mg--GET-cache))
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
             (puthash (list resource params) resp mg--GET-cache)
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
          (puthash (list resource params) value mg--GET-cache))))))

(ert-deftest mg--test-get-sync ()
  "Test synchronous version of mg--get"
  (let ((mg--GET-cache (make-hash-table :test 'equal) ))
	(with-mock
      (stub ghub-request => "18.0")
      (should (equal "18.0" (mg--get "/version" nil))))
    ;; Stub each function at most once per with-mock form.
    (with-mock
      (stub ghub-request => "17.0")
      (should (equal "18.0" (mg--get "/version" nil)))
      (should (equal "17.0" (mg--get "/version" nil :no-cache t))))))

(ert-deftest mg--test-get-async ()
  "Test asynchronous version of mg--get"
  (let* ((mg--GET-cache (make-hash-table :test 'equal) )
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
          (mg--get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; The cached value is returned
          (setq ghub-request-mock-response "17.0")
          (mg--get
           "/version" nil
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "18.0"))))
          ;; Unless no-cache is t
          (mg--get
           "/version" nil
           :no-cache t
           :callback (lambda (resp _header _status _req)
                       (should (equal resp "17.0")))))
      ;; Restore mock
      (fset 'ghub-request ghub-request-original))))

(cl-defun mg--get1
    (url params &key no-cache callback errorback)
  ""
  (let ((resp
         (mg--get
          url params
          :callback
          (when callback
            (lambda (resp) (funcall callback (car resp))))
          :errorback errorback
          :no-cache no-cache)))
    (when (not (or callback errorback))
      (car resp))))

(cl-defun mg--get-user (username &key no-cache callback errorback)
  ""
  (mg--get1
   "/users"
   `((username . ,username))
   :no-cache no-cache
   :callback callback
   :errorback errorback))

(ert-deftest mg-test-url-encode-project-id ()
  (should (equal 4414596 (alist-get 'id (mg--get-user "arvidnl" :no-cache t))))
  (should (equal 4414596 (alist-get 'id (mg--get-user "arvidnl")))))

(cl-defun mg--get-mr
    (project-id mr-iid &key no-cache callback errorback)
  ""
  (mg--get
   (mg--url-mr project-id mr-iid)
   nil
   :callback callback
   :errorback errorback
   :no-cache no-cache))

(cl-defun mg--get-project
    (project-id &key no-cache callback errorback)
  ""
  (mg--get
   (mg--url-project project-id)
   nil
   :callback callback
   :errorback errorback
   :no-cache no-cache))

(cl-defun mg--get-mr-of-source-branch
    (project-id source-branch &key no-cache callback errorback)
  ""
  (mg--get1
   (concat
    "/projects/"
    (mg-url-encode-project-id project-id)
    "/merge_requests")
   `((source_branch . ,source-branch))
   :callback callback
   :errorback errorback
   :no-cache no-cache))

;; (mg--get-mr-of-source-branch
;;  "tezos/tezos" "arvid@ci-add-cargo-cache")


(defconst mg--mr-properties
  '(add_labels
    allow_collaboration
    allow_maintainer_to_push
    assignee_id
    assignee_ids
    description
    discussion_locked
    labels
    milestone_id
    remove_labels
    remove_source_branch
    reviewer_ids
    squash
    state_event
    target_branch
    title))

(defun mg--show-mr-property (property)
  ""
  (pcase property
    ('add_labels "add labels") ;; todo
    ('allow_collaboration "allow collaboration")
    ('allow_maintainer_to_push "allow maintainer to push")
    ('assignee_id "assignee")
    ('assignee_ids "assignees")
    ('description "description")
    ('discussion_locked "discussion locked")
    ('labels "labels")
    ('milestone_id "milestone id")
    ('remove_labels "remove labels")
    ('remove_source_branch "remove source branch")
    ('reviewer_ids "reviewers")
    ('squash "squash")
    ('state_event "state")
    ('target_branch "target branch")
    ('title "title")
    (_ (error "Property %s is not one of: %s" property
              (mapconcat #'symbol-name mg--mr-properties ", ")))))

(defun mg--show-mr (mr)
  ""
  (format
   "%s!%d"
   (alist-get 'path_with_namespace (mg--get-project (alist-get 'project_id mr)))
   (alist-get 'iid mr)))


(defun mg--format (mr string &rest objects)
  ""
  (concat
   "["
   (propertize (mg--show-mr mr) 'face 'magit-branch-local)
   "] " (apply 'format string objects)))

(defun mg--message (mr string &rest objects)
	""
  (message (apply 'mg--format mr string objects)))

(cl-defun mg--mr-set-prop-async
    (mr
     property
     value
     &key
     callback
     errorback
     message-progress
     message-success
     message-error
     show-value)
  ""
  (unless (memq property mg--mr-properties)
    (error "Unsupported property: %s. Accepted properties are: %s"
           property (mapconcat #'symbol-name mg--mr-properties ", ")))
  (let* ((value-pp (if show-value (funcall show-value value) value))
         (message-prog
          (or message-progress
              (mg--format mr "Setting %s%s"
                          (mg--show-mr-property property)
                          (if value-pp (format " to: '%s'" value-pp) ""))))
         (message-success
          (or message-success
              (format "%s... Done!" message-prog)))
         (message-error
          (or message-error
              (mg--format mr "An error occurred when setting %s:"
                          (mg--show-mr-property property)))))
    (message "%s..." message-prog)
    (ghub-request
     "PUT"
     (mg--url-mr (alist-get 'project_id mr) (alist-get 'iid mr))
     `((,property . ,value))
     :auth 'magit-mr
     :forge 'gitlab
     :callback
     (or callback
         (lambda (_resp _header _status _req)
           (message "%s" message-success)))
     :errorback
     (or errorback
         (lambda (err _header _status _req)
           (message "%s: %s" message-error err))))))

;; (mg--get-user "arvidnl")

(defun mg--project-of-remote (remote-url)
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
     "Remote URL '%s' does not match expected format for a GitLab remote" remote-url)))

;; Examples of usage:
;; (mg--project-of-remote "git@gitlab.com:NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (mg--project-of-remote "https://gitlab.com/NAMESPACE/PROJECT.git")
;; => "NAMESPACE/PROJECT"

;; (mg--project-of-remote "https://google.com")

(defun mg--infer-project-id (branch)
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
          (mg--project-of-remote remote-url)
        (error
         "The remote '%s' has no url (remote.%s.url is not set)"
         remote remote))
    (error
     "Cannot infer GitLab project: no remote set for this branch, nor is remote.pushDefault set")))

(defvar-local mg--mr nil
  "Merge request under edit in MR description buffers")

;; TODO: if saving fails, user should not lose their description.
(cl-defun mg-mr-save-description-buffer (&key callback errorback)
  ""
  (interactive)
  (mg--mr-set-prop-async
   mg--mr 'description (buffer-string)
   :show-value (lambda (_) nil)
   :callback
   (lambda (resp header status req)
     (set-buffer-modified-p nil)
     (mg--message mg--mr "Setting description... Done!")
     (funcall callback resp header status req))
   :errorback errorback))

(defun mg-mr-save-and-close-description-buffer ()
  ""
  (interactive)
  (mg-mr-save-description-buffer
   :callback (lambda (_ _ _ _)
               (magit-kill-this-buffer))))

(defun mg-mr-cancel-description-buffer ()
  ""
  (interactive)
  (magit-kill-this-buffer)
  (mg--message mg--mr "Description edit cancelled"))

(defun mg--mr-create-description-buffer (mr)
  ""
  ;; Generate a unique buffer name
  (let* ((base-name
          (format "Edit description of %s"
                  (mg--show-mr mr)))
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
    (setq mg--mr mr)
    (keymap-local-set
     "C-c C-c" #'mg-mr-save-and-close-description-buffer)
    (keymap-local-set
     "C-c C-s" #'mg-mr-save-description-buffer)
    (keymap-local-set
     "C-x C-s" #'mg-mr-save-description-buffer)
    (keymap-local-set
     "C-c C-k" #'mg-mr-cancel-description-buffer)
    ;; Return the newly created buffer
    buffer-name))


(defun mg--strip-remote-prefix (branch-name)
  "Strip the remote prefix from BRANCH-NAME if present."
  (let ((components (split-string branch-name "/")))
    (if (> (length components) 1)
        (mapconcat 'identity (cdr components) "/") ; Rejoin the rest if more than one slash exists
      branch-name))) ; Return the original if no slash found

;; By default, get current branch or branch-at-point. If prefix is
;; given or if both those values are nil, then read a value instead.
(defun mg--read-branch ()
  ""
  (let ((branch
         (or (magit-branch-at-point) (magit-get-current-branch))))
    (if (or current-prefix-arg (not branch))
        (read-string "Branch name: ")
      (mg--strip-remote-prefix branch))))

(cl-defun mg--read-mr (&key cache)
  ""
  ;; TODO: if can't deduce branch -> ask for full mr ref
  ;; TODO: if can't deduce project -> ask for full mr ref
  ;; TODO: if can't deduce mr -> ask for full mr ref
  (let* ((branch (mg--read-branch))
         (project-id (mg--infer-project-id branch))
         (mr
          (mg--get-mr-of-source-branch
           project-id
           branch
           :no-cache (not cache))))
    (or mr
        (error
         "Couldn't find MR for branch '%s' in project '%s'"
         branch
         project-id))))

(transient-define-suffix mg--mr-edit-description (mr)
  "Set the title of MR to NEW-TITLE."
  (interactive (list (oref transient-current-prefix scope)))
  (mg--mr-create-description-buffer mr))

(transient-define-suffix mg--mr-edit-title (mr new-title)
  "Set the title of MR to NEW-TITLE."
  (interactive (let* ((mr (oref transient-current-prefix scope))
                      (new-title
                       (read-string
                        (mg--format mr "New title: ")
                        (alist-get 'title mr))))
                 (list mr new-title)))
  (mg--mr-set-prop-async mr 'title new-title))

(transient-define-suffix mg--mr-edit-labels (mr new-labels)
  "Set the labels of MR to NEW-LABELS."
  (interactive (let* ((mr (oref transient-current-prefix scope))
                      (new-labels
                       (read-string
                        (mg--format mr "New labels: ")
                        (s-join ", " (alist-get 'labels mr)))))
                 (list mr new-labels)))
  (mg--mr-set-prop-async mr 'labels new-labels))

(transient-define-suffix mg--mr-edit-target-branch (mr target-branch)
  "Set the target branch of the MR associated to BRANCH to TARGET-BRANCH"
  (interactive (list
                (oref transient-current-prefix scope)
                (magit-read-other-branch "New target branch")))
  (mg--mr-set-prop-async mr 'target_branch target-branch))

(transient-define-suffix mg--mr-toggle-draft (mr)
  "Toggle the draft status of the MR associate to BRANCH"
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((title (alist-get 'title mr))
         (is-draft (string-match "^\\(Draft: \\)+\\(.*\\)$" title))
         (new-title
          (if is-draft
              (let ((title-no-draft (match-string 2 title)))
	            title-no-draft)
            (concat "Draft: " title))))
    (mg--mr-set-prop-async mr 'title new-title
                           :message-progress
                           (mg--format mr "%s as draft"
                                       (if is-draft "Unmarking" "Marking")))))

(defun mg--decode-assignees (assignee-objs)
  "From assignee objects to list of usernames (strings)"
  (mapcar
   (lambda (assignee-obj) (alist-get 'username assignee-obj))
   assignee-objs))

(defun mg--to-user-id (id-or-username)
  "From ASSIGNEE-ID-OR-USERNAME to numerical user ids."
  (if (numberp id-or-username)
      id-or-username
    (if-let* ((assignee-username
               (string-remove-prefix "@" id-or-username)))
        (alist-get 'id (mg--get-user assignee-username))
      (error
       "Could not find id associated to username '%s' -- do they exist?"
       assignee-username))))

(ert-deftest mg--test-to-user-id ()
  (let ((mg--GET-cache (make-hash-table :test 'equal)))
	(should (equal 4414596 (mg--to-user-id "@arvidnl")))))

(defun mg--format-user-as-candidate (user)
	"Return user as a cons '(NAME (@USERNAME) . ID)'"
    (let ((username (alist-get 'username user))
          (name (alist-get 'name user))
          (id (alist-get 'id user)))
      (cons (format "%s (@%s)" name username) id)))

(defun mg--format-favorites-as-candidates ()
  ""
  (mapcar
   (lambda (user)
     (let* ((name (car (cdr user)))
            (username (car (cdr (cdr user))))
            (id (mg--to-user-id username)))
       (cons (format "%s (%s)" name username) id)))
   mg-favorite-users))

(transient-define-suffix mg--mr-edit-assignees (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((current-reviewers (mapcar #'mg--format-user-as-candidate (alist-get 'reviewers mr)))
         (current-assignees (mapcar #'mg--format-user-as-candidate (alist-get 'assignees mr)))
         (candidate-assignees
          (append current-reviewers
                  current-assignees
                  (list (mg--format-user-as-candidate (alist-get 'author mr)) )
                  (mg--format-favorites-as-candidates)))
         (new-assignees
          (seq-uniq
           (completing-read-multiple
            ;; prompt
            (mg--format
             mr
             "Set assignees (space-separated GitLab usernames) [reviewers: %s]: "
             (if current-reviewers
                 (concat (s-join ", " (mapcar #'car current-reviewers)))
               "none"))
            ;; table
            candidate-assignees
            nil ;; predicate
            nil ;; require-match
            ;; initial-input
            (if current-assignees
                (concat (s-join ", " (mapcar #'car current-assignees)) ", ")
              nil))))
         (new-assignees-aux
          (mapcar
           (lambda (selection) (or (cdr (assoc selection candidate-assignees)) selection))
           new-assignees)))
    (mg--mr-set-prop-async
     mr
     'assignee_ids
     (mapcar #'mg--to-user-id new-assignees-aux)
     :show-value (lambda (_)
                   (if new-assignees
                       (s-join ", " new-assignees)
                     "None")))))

(transient-define-suffix mg--mr-assign-to-favorite--set (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (if-let (assignees (transient-args 'mg-mr-assign-to-favorite))
      ;; (print assignees)
      (mg--mr-set-prop-async mr
                             'assignee_ids
                             (mapcar #'mg--to-user-id assignees)
                             :show-value
                             (lambda (_) (s-join ", " assignees)))
    (error "Select a non-empty set of favorites first.")))

(transient-define-suffix mg--mr-assign-to-reviewers (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((reviewers
          (if-let (reviewers (alist-get 'reviewers mr))
              (mapcar #'mg--format-user-as-candidate
                      (alist-get 'reviewers mr))
            (error "This MR has no reviewers!"))))
    (mg--mr-set-prop-async mr
                           'assignee_ids
                           (mapcar #'cdr reviewers)
                           :show-value
                           (lambda (_)
                             (s-join ", " (mapcar #'car reviewers))))))

(transient-define-suffix mg--mr-assign-to-me (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((my-username (concat "@" (ghub--username nil 'gitlab)))
         (my-id (mg--to-user-id my-username)))
    (mg--mr-set-prop-async
     mr
     'assignee_ids
     (list my-id)
     :show-value (lambda (_) my-username))))

(transient-define-suffix mg-mr-assign-to-author (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((author-username (concat "@" (alist-get 'username (alist-get 'author mr))))
         (author-id (alist-get 'id (alist-get 'author mr))))
    (mg--mr-set-prop-async
     mr
     'assignee_ids
     (list author-id)
     :show-value (lambda (_) author-username))))


(transient-define-suffix mg--mr-edit-reviewers (mr)
  ""
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((current-reviewers (mapcar #'mg--format-user-as-candidate (alist-get 'reviewers mr)))
         (current-assignees (mapcar #'mg--format-user-as-candidate (alist-get 'assignees mr)))
         (candidate-reviewers
          (append current-reviewers
                  current-assignees
                  (list (mg--format-user-as-candidate (alist-get 'author mr)) )
                  (mg--format-favorites-as-candidates)))
         (new-reviewers
          (seq-uniq
           (completing-read-multiple
            ;; prompt
            (mg--format
             mr
             "Set reviewers (space-separated GitLab usernames) [assignees: %s]: "
             (if current-assignees
                 (concat (s-join ", " (mapcar #'car current-assignees)))
               "none"))
            ;; table
            candidate-reviewers
            nil ;; predicate
            nil ;; require-match
            ;; initial-input
            (if current-reviewers
                (concat (s-join ", " (mapcar #'car current-reviewers)) ", ")
              nil))))
         (new-reviewers-aux
          (mapcar
           (lambda (selection) (or (cdr (assoc selection candidate-reviewers)) selection))
           new-reviewers)))
    (mg--mr-set-prop-async
     mr
     'reviewer_ids
     (mapcar #'mg--to-user-id new-reviewers-aux)
     :show-value (lambda (_)
                   (if new-reviewers
                       (s-join ", " new-reviewers)
                     "None")))))


(transient-define-suffix mg-mr-browse (mr)
  "Browse the MR of the current BRANCH on GitLab with ‘browse-url’."
  (interactive (list (oref transient-current-prefix scope)))
  (browse-url (alist-get 'web_url mr)))

(transient-define-suffix mg-mr-browse-kill (mr)
  "Add the URL of the current MR to the kill ring.

Works like ‘mg-mr-browse’, but puts the address in the
kill ring instead of opening it with ‘browse-url’."
  (interactive (list (oref transient-current-prefix scope)))
  (let* ((web-url (alist-get 'web_url mr)))
    (kill-new web-url)
    (message "Added URL `%s' to kill ring" web-url)))

(defun mg--todo ()
	"Placeholder for functionality that is not yet implemented."
  (interactive)
  (error "TODO"))

(defun mg--mr-assign-to-favorite--setup-children (_)
	""
    (transient-parse-suffixes
     'mg-mr-assign-to-favorite
     mg-favorite-users))

(defun mg-customize-favorites ()
	""
  (interactive)
  (customize-variable 'mg-favorite-users))

(transient-define-prefix
  mg-mr-assign-to-favorite (mr) "Assign MR to a favorite user."
  [["Favorites"
    :if (lambda () mg-favorite-users)
    :setup-children mg--mr-assign-to-favorite--setup-children
    :class transient-column
    ]
   ["Handle favorites"
    ("C" "customize favorites" mg-customize-favorites)]]
  ["Actions"
   :if (lambda () mg-favorite-users)
   ("S" "set" mg--mr-assign-to-favorite--set)
   ("A" "add" mg--todo)]
  (interactive (list (mg--read-mr :cache (eq transient-current-command 'mg-mr))))
  (transient-setup 'mg-mr-assign-to-favorite nil nil :scope mr))

(transient-define-prefix
 mg-mr (mr) "Act on a GitLab merge request."
 [:description
  (lambda ()
    (let ((mr (oref (transient-prefix-object) scope)))
	  (let ((title (concat
                    (propertize "Act on GitLab merge request "
                                'face
                                'transient-heading)
                    (propertize (mg--show-mr mr)
                                'face 'magit-branch-local)
                    ": "
                    (format "%s " (alist-get 'title mr))
                    (format "[target: %s]"
                            (propertize (alist-get 'target_branch mr)
                                        'face 'magit-branch-remote))))
            (labels (format "Labels: [%s]" (s-join ", " (alist-get 'labels mr)))))
        (concat (s-join "\n" (list title labels)) "\n"))))
  ["Edit"
   ("t" "title" mg--mr-edit-title)
   ("d" "description" mg--mr-edit-description)
   ("m" "milestone" mg--todo)
   ;; TODO: since we know the MR we make this either "Draft" or "Undraft"
   ("D" "toggle draft status" mg--mr-toggle-draft)
   ("l" "labels" mg--mr-edit-labels)
   ("T" "target branch" mg--mr-edit-target-branch)
   ]
  [:description
   (lambda ()
     (let ((mr (oref (transient-prefix-object) scope)))
       (format "Assignees [%s]"
               (s-join ", "
                       (mapcar
                        (lambda (user) (concat "@" (alist-get 'username user)))
                        (alist-get 'assignees mr))))))
   ("a a" "edit assignees" mg--mr-edit-assignees)
   ("a m" "assign to me" mg--mr-assign-to-me)
   ("a A" "assign to author" mg-mr-assign-to-author)
   ("a r" "assign to reviewers" mg-mr-assign-to-reviewers)
   ("a f" "assign to favorite" mg-mr-assign-to-favorite)]
  [:description
   (lambda ()
     (let ((mr (oref (transient-prefix-object) scope)))
       (format "Reviewers [%s]"
               (s-join ", "
                       (mapcar
                        (lambda (user) (concat "@" (alist-get 'username user)))
                        (alist-get 'reviewers mr))))))
   ("r r" "edit reviewers" mg--mr-edit-reviewers)]
  ["Actions"
   ("v" "open MR on GitLab" mg-mr-browse)
   ("k" "add MR url to kill ring" mg-mr-browse-kill)
   ]]
 (interactive (list (mg--read-mr)))
 (transient-setup 'mg-mr nil nil :scope mr))

;; Update magit-mode-map such that pressing @ opens the magit-glab-mr transient
(define-key magit-mode-map (kbd "@") 'mg-mr)

(provide 'arvid-magit-mr)

;; Local Variables:
;; read-symbol-shorthands: (("mg-" . "magit-glab-"))
;; End:
