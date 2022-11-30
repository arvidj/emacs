(defun arvid-mr/query-firefox-history (query)
    ""
  (interactive)
  (let* ((db (string-trim (shell-command-to-string "find $HOME/.mozilla/firefox/ -name places.sqlite -printf \"%T@ %Tc %p\\n\" | sort -n | head -n 1 | cut -d ' ' -f7")))
         (dbtmp (make-temp-name "/tmp/firefox-places."))
         (cmd (concat
               "sqlite3 "
               (shell-quote-argument dbtmp) " "
               (shell-quote-argument query))))

    (copy-file db dbtmp t)
    (assert (and (file-exists-p dbtmp) (> (file-attribute-size (file-attributes dbtmp)) 0)))

    (let* ((results (shell-command-to-string cmd)))
      (mapcar (lambda (s) (let ((s (split-string s "|")))
                            (cons (car s) s)))
              (split-string results "\n")))))

(defun arvid-mr/issues-get-candidates-firefox ()
  ""
  (interactive)
  (let* ((limit 1000)
         (query (format "select p.title, p.url from moz_historyvisits as h, moz_places as p where p.id == h.place_id and p.url like '%%issues%%' order by h.visit_date desc limit %d;" limit)))
    (arvid-mr/query-firefox-history query)))

(defun arvid-mr/mrs-get-candidates-firefox ()
  ""
  (interactive)
  (let* ((limit 1000)
         (query (format "select p.title, p.url from moz_historyvisits as h, moz_places as p where p.id == h.place_id and p.url like '%%merge_requests%%' order by h.visit_date desc limit %d;" limit)))
    (arvid-mr/query-firefox-history query)))

(cl-defstruct mergerequest title url org repo number)

(cl-defstruct issue title url org repo number)

(defun obj-browse-url ()
    ""
  (interactive)
  
  )

;; (setq mr-url "https://gitlab.com/tezos/tezos/merge_requests/1366")


(defun arvid-mr/mr-make-url-aux (org repo number)
  ""
  (format "https://gitlab.com/%s/%s/merge_requests/%d" org repo number))


(defun arvid-mr/issue-make-url-aux (org repo number)
  ""
  (format "https://gitlab.com/%s/%s/issues/%d" org repo number))


(defun arvid-mr/mr-parse-gitlab-url (url)
    ""
  (when (string-match
         ".*gitlab.com/\\(.*?\\)/\\([^/]*?\\)/\\(-/\\)?merge_requests/\\([[:digit:]]+\\).*"
         url)
    (let ((org (match-string 1 url))
          (repo (match-string 2 url))
          (number (string-to-number (match-string 4 url))))
      (make-mergerequest :title nil
                         :url (arvid-mr/mr-make-url-aux org repo number)
                         :org org :repo repo :number number))))


(assert (equal (arvid-mr/mr-parse-gitlab-url "https://gitlab.com/tezos/tezos/-/merge_requests/1887")
               (make-mergerequest :title nil :org "tezos" :repo "tezos" :number 1887 :url "https://gitlab.com/tezos/tezos/merge_requests/1887")))

(defun arvid-mr/issue-parse-gitlab-url (url)
  ""
  (when (string-match
         ".*gitlab.com/\\(.*?\\)/\\([^/]*?\\)/\\(-/\\)?issues/\\([[:digit:]]+\\).*"
         url)
    (make-issue :title nil
                :url url
                :org (match-string 1 url)
                :repo (match-string 2 url)
                :number (string-to-number (match-string 4 url)))))

(assert (equal (arvid-mr/issue-parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/issues/29")
               (make-issue :title nil :org "nomadic-labs" :repo "tezos" :number 29 :url "https://gitlab.com/nomadic-labs/tezos/issues/29")))

(defun arvid-mr/parse-gitlab-url (url)
  (or (arvid-mr/mr-parse-gitlab-url url)
      (arvid-mr/issue-parse-gitlab-url url)))


(assert (equal (arvid-mr/parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/issues/29")
               (make-issue :title nil :org "nomadic-labs" :repo "tezos" :number 29 :url "https://gitlab.com/nomadic-labs/tezos/issues/29")))

(assert (equal (arvid-mr/parse-gitlab-url "https://gitlab.com/tezos/tezos/-/merge_requests/1887")
               (make-mergerequest :title nil :org "tezos" :repo "tezos" :number 1887 :url "https://gitlab.com/tezos/tezos/merge_requests/1887")))


(defun arvid-mr/obj-title (obj)
  (cond
   ((mergerequest-p obj)
    (mergerequest-title obj))
   ((issue-p obj)
    (issue-title obj))))

(defun arvid-mr/obj-set-title (obj title)
  (cond
   ((mergerequest-p obj)
    (setf (mergerequest-title obj) title))
   ((issue-p obj)
    (setf (issue-title obj) title))))

(let ((m (make-mergerequest :title "test" :org "tezos" :repo "tezos" :number 1887 :url "https://gitlab.com/tezos/tezos/merge_requests/1887")))
  (assert (equal (arvid-mr/obj-title m) "test"))
  (arvid-mr/obj-set-title m "foo")
  (assert (equal (arvid-mr/obj-title m) "foo")))

(defun arvid-mr/parse-candidate (obj-candidate)
  ""
  (interactive)
  (let* ((title (car obj-candidate))
         (url (cadr obj-candidate))
         (obj (arvid-mr/parse-gitlab-url url)))
    (arvid-mr/obj-set-title obj title)
    obj))

(assert (equal
         (arvid-mr/parse-candidate '("foo" "https://gitlab.com/tezos/tezos/merge_requests/1887"))
         (make-mergerequest :title "foo" :org "tezos" :repo "tezos" :number 1887
                            :url "https://gitlab.com/tezos/tezos/merge_requests/1887")))
(assert (equal
         (arvid-mr/parse-candidate '("foo" "https://gitlab.com/tezos/tezos/issues/1887"))
         (make-issue :title "foo" :org "tezos" :repo "tezos" :number 1887
                     :url "https://gitlab.com/tezos/tezos/issues/1887")))



(defvar
  arvid-mr/shortlink-regexp
  "\\(\\([[:alnum:]_-]+\\)/\\)?\\([[:alnum:]_-]+\\)?\\([!\\#]\\)\\([[:digit:]]*\\)")


(defvar
  arvid-mr/shortlink-regexp-3
  "\\([[:alnum:]_-]+\\)/\\([[:alnum:]_-]+\\)\\([!\\#]\\)\\([[:digit:]]*\\)")

(defvar
  arvid-mr/shortlink-regexp-2
  "\\([[:alnum:]_-]+\\)\\([!\\#]\\)\\([[:digit:]]*\\)")

(defvar
  arvid-mr/shortlink-regexp-1
  "\\([!\\#]\\)\\([[:digit:]]*\\)")

(assert (string-match arvid-mr/shortlink-regexp "!1123"))
(assert (string-match arvid-mr/shortlink-regexp-1 "!1123"))
(assert (string-match arvid-mr/shortlink-regexp "foo!1123"))
(assert (string-match arvid-mr/shortlink-regexp-2 "foo!1123"))
(assert (string-match arvid-mr/shortlink-regexp "foo/bar!1123"))
(assert (string-match arvid-mr/shortlink-regexp-3 "foo/bar!1123"))

(assert (string-match arvid-mr/shortlink-regexp "#1123"))
(assert (string-match arvid-mr/shortlink-regexp-1 "#1123"))
(assert (string-match arvid-mr/shortlink-regexp "foo#1123"))
(assert (string-match arvid-mr/shortlink-regexp-2 "foo#1123"))
(assert (string-match arvid-mr/shortlink-regexp "foo/bar#1123"))
(assert (string-match arvid-mr/shortlink-regexp-3 "foo/bar#1123"))

(defvar arvid-mr/default-org "tezos")

(defvar arvid-mr/default-repo "tezos")

(defun arvid-mr/type-string-to-type (type)
    ""
  (cond
   ((string= type "!") 'arvid-mr/mergerequest)
   ((string= type "#") 'arvid-mr/issue)))

(assert (eql (arvid-mr/type-string-to-type "#") 'arvid-mr/issue))
(assert (eql (arvid-mr/type-string-to-type "!") 'arvid-mr/mergerequest))

(defun arvid-mr/parse-shortlink (shortlink)
  (when (and shortlink (string-match arvid-mr/shortlink-regexp shortlink))
    (let ((type (arvid-mr/type-string-to-type (match-string 4 shortlink)))
          (org (or (match-string 2 shortlink) arvid-mr/default-org))
          (repo (or (match-string 3 shortlink) arvid-mr/default-repo))
          (number (string-to-number (match-string 5 shortlink))))
      (case type
        ('arvid-mr/mergerequest
         (make-mergerequest :title nil
                            :org org
                            :repo repo
                            :number number
                            :url (arvid-mr/mr-make-url-aux org repo number)))
        ('arvid-mr/issue
         (make-issue :title nil
                     :org org
                     :repo repo
                     :number number
                     :url (format
                           (arvid-mr/issue-make-url-aux org repo number)
                           org repo number)))
        )
      )))

(assert (equal (arvid-mr/parse-shortlink "foo/bar!1123")
               (make-mergerequest :org "foo" :repo "bar" :number 1123 :url "https://gitlab.com/foo/bar/merge_requests/1123")))
(assert (equal (arvid-mr/parse-shortlink "foo/tezos!123123")
               (make-mergerequest :org "foo" :repo "tezos" :number 123123 :url "https://gitlab.com/foo/tezos/merge_requests/123123")))
(assert (equal (arvid-mr/parse-shortlink "https://gitlab.com/nomadic-labs/albert-lang.io/-/jobs/605404211") nil))

(assert (equal (arvid-mr/parse-shortlink "foo/bar#1123")
               (make-issue :org "foo" :repo "bar" :number 1123 :url "https://gitlab.com/foo/bar/issues/1123")))
(assert (equal (arvid-mr/parse-shortlink "foo/tezos#123123")
               (make-issue :org "foo" :repo "tezos" :number 123123 :url "https://gitlab.com/foo/tezos/issues/123123")))
(assert (equal (arvid-mr/parse-shortlink "https://gitlab.com/nomadic-labs/albert-lang.io/-/jobs/605404211") nil))


(defun arvid-mr/mergerequest-make-url (mr-candidate)
    ""
  (interactive)
  (when-let (mr (arvid-mr/parse-candidate mr-candidate))
    (arvid-mr/mr-make-url-aux
     (mergerequest-org mr)
     (mergerequest-repo mr)
     (mergerequest-number mr))))

;; (assert (equal
;;          (arvid-mr/mergerequest-make-url
;;           ()
;;           "https://gitlab.com/tezos/tezos/-/merge_requests/1887"
;;           )
;;          )

;;  )


(defun arvid-mr/make-shortlink-aux (symbol org repo number full)
    ""
  (interactive)
  (cond
   ((and
     (not full)
     (string= org arvid-mr/default-org)
     (string= repo arvid-mr/default-repo))
    (format "%s%d" symbol nr))
   ((and (not full)
         (string= org arvid-mr/default-org))
    (format "%s%s%d" repo symbol nr))
   (t (format "%s/%s%s%d" org repo symbol nr)))
  )

(defun arvid-mr/issue-make-shortlink (issue &optional full)
  ""
  (interactive)
  (let ((org (issue-org issue))
        (repo (issue-repo issue))
        (nr (issue-number issue)))
    (arvid-mr/make-shortlink-aux "#" org repo nr full)))

(assert (equal "nomadic-labs/tezos#23"
               (arvid-mr/issue-make-shortlink
                (arvid-mr/issue-parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/-/issues/23"))))

(defun arvid-mr/mergerequest-make-shortlink (mr &optional full)
  ""
  (interactive)
  (let ((org (mergerequest-org mr))
        (repo (mergerequest-repo mr))
        (nr (mergerequest-number mr)))
    (arvid-mr/make-shortlink-aux "!" org repo nr full)))

(assert (equal "!1887"
               (arvid-mr/mergerequest-make-shortlink
                (arvid-mr/mr-parse-gitlab-url "https://gitlab.com/tezos/tezos/-/merge_requests/1887"))))

(assert (equal "tezos/tezos!1887"
               (arvid-mr/mergerequest-make-shortlink
                (arvid-mr/mr-parse-gitlab-url "https://gitlab.com/tezos/tezos/-/merge_requests/1887")
                t
                )))

(assert (equal "nomadic-labs/tezos!1887"
               (arvid-mr/mergerequest-make-shortlink
                (arvid-mr/mr-parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/-/merge_requests/1887"))))



(defun arvid-mr/obj-make-shortlink (obj &optional full)
    ""
  (cond
   ((mergerequest-p obj)
    (arvid-mr/mergerequest-make-shortlink obj full))
   ((issue-p obj)
    (arvid-mr/issue-make-shortlink obj full))))

(defun arvid-mr/obj-url (obj)
  ""
  (cond
   ((mergerequest-p obj) (mergerequest-url obj))
   ((issue-p obj) (issue-url obj))))


(assert (equal "nomadic-labs/tezos!1887"
               (arvid-mr/obj-make-shortlink
                (arvid-mr/parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/-/merge_requests/1887"))))

(assert (equal "nomadic-labs/tezos#32"
               (arvid-mr/obj-make-shortlink
                (arvid-mr/parse-gitlab-url "https://gitlab.com/nomadic-labs/tezos/issues/32"))))

(defun arvid-mr/insert-org-link (mr-candidate)
    ""
    (when-let (mr (arvid-mr/parse-candidate mr-candidate))
      (insert (format "[[%s][%s]]"
                      (mergerequest-url mr)
                      (arvid-mr/mergerequest-make-shortlink mr)
                      ))))

(defun arvid-mr/obj-insert-shortlink (obj-candidate) ""
       (when-let (obj (arvid-mr/parse-candidate obj-candidate))
         (insert (arvid-mr/obj-make-shortlink obj))))

(defun arvid-mr/open (mr-candidate)
  ""
  (browse-url (mergerequest-url (arvid-mr/parse-candidate mr-candidate))))

(defun arvid-mr/insert-url (mr-candidate)
  ""
  (insert (mergerequest-url (arvid-mr/parse-candidate mr-candidate))))


(defun arvid-mr/insert-url (mr-candidate)
  ""
  (insert (arvid-mr/mergerequest-make-org-link (arvid-mr/parse-candidate mr-candidate))))

(defun arvid-mr/insert-title (mr-candidate)
  ""
  (insert (mergerequest-title (arvid-mr/parse-candidate mr-candidate))))

(defun arvid-mr/helm-mrs ()
  ""
  (interactive)
  (helm :sources (helm-build-sync-source "MRs visited in Firefox"
                   :candidates (arvid-mr/mrs-get-candidates-firefox)
                   :action '(("Insert shortlink format (org/repo!no)" . arvid-mr/obj-insert-shortlink)
                             ("Insert URL" . arvid-mr/insert-url)
                             ("Insert Org Link" . arvid-mr/insert-org-link)
                             ("Insert name" . arvid-mr/insert-title)
                             ("Open in browser" . arvid-mr/open)
                             ))))


(defun arvid-mr/helm-issues ()
  ""
  (interactive)
  (helm :sources (helm-build-sync-source "Issues visited in Firefox"
                   :candidates (arvid-mr/issues-get-candidates-firefox)
                   :action '(("Insert shortlink format (org/repo!no)" . arvid-mr/obj-insert-shortlink)
                             ("Insert URL" . arvid-mr/insert-url)
                             ("Insert Org Link" . arvid-mr/insert-org-link)
                             ("Insert name" . arvid-mr/insert-title)
                             ("Open in browser" . arvid-mr/open)
                             ))))



(defun arvid-mr/shortlink-at-point ()
  "Return the shortlink at point, or nil if none is found."
  (when (or (thing-at-point-looking-at arvid-mr/shortlink-regexp-3 500)
            (thing-at-point-looking-at arvid-mr/shortlink-regexp-2 500)
            (thing-at-point-looking-at arvid-mr/shortlink-regexp-1 500))
    (buffer-substring (match-beginning 0) (match-end 0))))

;; foo/bar!12312
;; bar!12312
;; !12312
;; foo/bar#12312
;; bar#12312
;; #12312

(defun arvid-mr/org-open-gitlab-shortlink ()
  ""
  (let ((atp (arvid-mr/shortlink-at-point)))
    (when-let (obj (arvid-mr/parse-shortlink atp))
      (browse-url (arvid-mr/obj-url obj))
      t
      )))

(add-hook 'org-open-at-point-functions 'arvid-mr/org-open-gitlab-shortlink)

(provide 'arvid-mr)

(defun arvid-mr/gitlab-shortlink-description (link default-desc)
    ""
  (interactive)
  (if-let ((mr (arvid-mr/mr-parse-gitlab-url link)))
    (arvid-mr/mergerequest-make-shortlink mr)
    default-desc))

(arvid-mr/gitlab-shortlink-description "https://gitlab.com/tezos/tezos/-/merge_requests/1887" "Foo")

(setq org-make-link-description-function 'arvid-mr/gitlab-shortlink-description)


;; misc:

(defun arvid-gl/gitlab-goto-line ()
  ""
  (interactive)
  (let ((repo "tezos/tezos")
        (branch "master")
        (file (magit-file-relative-name))
        (line (line-number-at-pos)))
    (browse-url
     (format
      "https://gitlab.com/%s/-/blob/%s/%s#L%d"
      repo
      branch
      file
      line))))


;; TODO: eldoc mode
