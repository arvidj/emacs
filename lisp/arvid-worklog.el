
(defcustom
  arvid-wl/worklog-file
  "/home/arvid/dev/nomadic-labs/worklog/worklog.org"
  "Location of the worklog file")

;; TODO
;;; 1. change current / next week
;;; 2. remove org meta data on top of the fiel
;;; 3. remove clock drawers
;;; 4. add time to headings recursively


;; (defun clean-title (title)
;;     ""
;;   (interactive)



;;   )

;; ;; This does not work, cannot figure out.
;; (let ((s " - [Finished] Review [[https://gitlab.com/tezos/tezos/-/merge_requests/1887][#123]] <1h37>")
;;       (re org-bracket-link-analytic-regexp))
;;   (replace-regexp-in-string
;;    re
;;    (lambda (match)
;;      (save-match-data
;;      (let ((url (concat (match-string 2 s) ":" (match-string 3 s))) (title (match-string 5 s)))
;;        url
;;        ;; (message url)
;;        ;; (save-match-data
;;          ;; (if-let* ((obj (arvid-mr/parse-gitlab-url url))
;;          ;;             (shortlink (arvid-mr/make-shortlink obj t)))
;;          ;;   shortlink
;;          ;; match
;;          ;;   )
;;        ;; )
;;        ;; "foo"
;;        )
;;      )
;;      )
;;    s)
;;   )

  ;; (string-match re s)
  ;; (let ((url (concat (match-string 2 s) ":" (match-string 3 s)))
  ;;       (title (match-string 5 s)))
  ;;   (when-let* ((obj (arvid-mr/parse-gitlab-url url))
  ;;               (shortlink (arvid-mr/make-shortlink obj t)))

  ;;     (replace-regexp-in-string)

  ;;     )
  ;; (message "Title: %s, Url: %s" title url)
  ;; ))



(defun arvid-wl/org-remove-drawer-at (pos)
  "Remove a drawer at position POS.
POS may also be a marker."
  (with-current-buffer (if (markerp pos) (marker-buffer pos) (current-buffer))
    (org-with-wide-buffer
     (goto-char pos)
     (let ((drawer (org-element-at-point)))
       (when (and (memq (org-element-type drawer) '(drawer property-drawer)))
         (delete-region (org-element-property :begin drawer)
                        (progn (goto-char (org-element-property :end drawer))
                               (skip-chars-backward " \r\t\n")
                               (forward-line)
                               (point))))))))

(defun arvid-wl/org-remove-drawer-from-heading ()
  "Remove the first drawer from under an heading"
  (save-excursion
    (cl-assert (outline-on-heading-p) nil "arvid-wl/org-remove-drawer-from-heading: must be called on an outline heading")
    (forward-line)
    (if (org-at-drawer-p)
        (arvid-wl/org-remove-drawer-at (point)))))

(defun arvid-wl/heading-to-list-item (time-range time-range-prop)
  ""
  (interactive)
  ;; (org-clock-display time-range)
  (let ((time (or (get-text-property (point) time-range-prop) 0))
        (title (arvid-wl/org-heading-title))
        (todo (arvid-wl/org-heading-todo)))
    (cl-assert todo nil (format "arvid-wl/heading-to-list-item: missing TODO state in %s" title))
    (org-cut-subtree)
    (insert (format " - %s %s <%sh%02d>\n" todo title (/ time 60) (% time 60))))
  ;; (org-clock-remove-overlays)
  )

(defun arvid-wl/heading-has-leaves-p ()
  "Return t if heading has children"
  (cl-assert (outline-on-heading-p) nil "arvid-wl/heading-has-leaves-p: must be called on an outline heading")
  (save-excursion
    (< (org-current-level)
           (progn (outline-next-heading)
                  (org-current-level)))))



(setq org-clock-display-default-range 'lastweek)

(defun arvid-wl/at-workitem-p ()
  "Returns t if point is currently at a heading that is a workitem.

   A work item is a heading that either has one leaf (a simple work item)
   or a heading with the WORKITEM property set to t (a composed work item)."
  (or (not (arvid-wl/heading-has-leaves-p))
      (string= (org-entry-get (point) "WORKITEM") "t")))

(require 'org)

(defun arvid-wl/org-heading-title ()
  (pcase (org-heading-components)
    (`(,_ . (,_ . (,todo . (,_ . (,title . ,_))))) title)
    (t nil)))

(defun arvid-wl/workitem-info (title)
  (if (string-match "\\(.+\\) (\\(.*#.*\\))" title)
      `(,(match-string 1 title) . ,(match-string 2 title))
    `(,title . nil)))

(cl-assert (equal (arvid-wl/workitem-info "Foo (sd#bar)") '("Foo" . "sd#bar")))
(cl-assert (equal (arvid-wl/workitem-info "Foo") '("Foo" . nil)))

(defun arvid-wl/org-heading-todo ()
  (pcase (org-heading-components)
    (`(,_ . (,_ . (,todo . (,_ . (,title . ,_))))) todo)
    (t nil)))

(defun arvid-wl/format-time (time)
  (format "<%sh%02d>" (/ time 60) (% time 60)))

(defun arvid-wl/heading-add-time-suffix (total-time)
  (cl-assert (outline-on-heading-p) nil "arvid-wl/heading-add-time-suffix: must be called on an outline heading")
  (save-excursion
    (end-of-line)
    (insert " " (arvid-wl/format-time total-time))))

(defun arvid-wl/org-remove-contents ()
  (cl-assert (outline-on-heading-p) nil "arvid-wl/heading-add-time-suffix: must be called on an outline heading")
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (delete-region (point)
                   (progn (outline-next-heading) (point)))))

(defun arvid-wl/prune2-composed-workitem (time-range time-range-prop)
  (cl-assert (outline-on-heading-p) nil "arvid-wl/prune2-composed-workitem: must be called on an outline heading")

  ;; (org-clock-display time-range)
  (let* ((init-level (org-current-level))
         (workitem-info (arvid-wl/workitem-info (arvid-wl/org-heading-title)))
         (workitem-title (car workitem-info))
         (workitem-issue (cdr workitem-info))
         (total-time (or (get-text-property (point) time-range-prop) 0))
         (subheading-time 0))

    ;; remove contents and the header
    (arvid-wl/org-remove-contents)
    (kill-whole-line)

    (while (< init-level (org-current-level))

      (org-clock-display)
      (let ((time (or (get-text-property (point) time-range-prop) 0))
            (title (arvid-wl/org-heading-title))
            (todo (arvid-wl/org-heading-todo)))

        (cl-assert
         (not (arvid-wl/heading-has-leaves-p))
         "Children of composed items cannot themselves have leaves")

        (org-cut-subtree)
        (when (> time 0)
          (setq subheading-time (+ time subheading-time))
          (insert (format " - %s %s: %s %s%s\n"
                          todo
                          workitem-title
                          title
                          (if workitem-issue
                              (format "(%s) " workitem-issue)
                            "")
                          (arvid-wl/format-time time))))))

    (cl-assert (= total-time subheading-time)
            t
            "arvid-wl/prune2-composed-workitem: sum of sub-item times at %d in workitem "
            (line-number-at-pos)
            (workitem-title)
            )
    ;; (org-clock-remove-overlays)
    ))


;; TODO: assert that the times of the children equals the sum
;; of times of sub-items

;; simple approximation: just check there is no drawer. this doesn't work
;; because composed work items will have a property drawer
;; (cl-assert (not
;;          (save-excursion
;;            (forward-line)
;;            (not (org-at-drawer-p))))
;;         "Composed work items should not be clocked into: clock into sub-items"
;;         )

;; (defun arvid-wl/prune2 (&optional time-range)
(defun arvid-wl/prune2 (week)
  ""
  (interactive (list (intern (ido-completing-read "Prune work log for which week? "
                                                  '("thisweek" "lastweek") nil t))))

  ;; (interactive (list
  ;;               (let ((arg current-prefix-arg))
  ;;                 (if (equal arg '(4)) 'thisweek
  ;;                   (if (equal arg '(16)) 'lastweek
  ;;                     (intern (ido-completing-read
  ;;   		                   "Choose recent file: "
  ;;                              '("thisweek" "lastweek")
  ;;   		                   nil
  ;;                              t)))))))


  (setq org-clock-display-default-range week)

  (let ((time-range-prop :org-clock-minutes-default)
        (time-range week))

  (save-excursion
    (outline-show-all)
    (beginning-of-buffer)
    (outline-next-heading)

    (while (outline-on-heading-p)
      (pcase (org-heading-components)
        (`(,_ . (,_ . (,todo . (,_ . (,title . ,_)))))
         (org-clock-display)
         (let ((time (get-text-property (point) time-range-prop)))
           (if time
               (progn
                 (cond
                   ;; A composed work item
                  ((string= (org-entry-get (point) "WORKITEM") "t")

                   ;; recursively transform children to list items
                   (arvid-wl/prune2-composed-workitem time-range time-range-prop)
                   )
                  ;; A simple work item
                  ((not (arvid-wl/heading-has-leaves-p))
                   (arvid-wl/heading-to-list-item time-range time-range-prop))
                  ;; A category
                  (t
                   ;; this is not a work-item, with a non-empty time. Suffix with time
                   ;; and proceed to the next heading.
                   (arvid-wl/heading-add-time-suffix time)
                   (outline-next-heading))))

             ;; If no time, remove the heading and subitems
             (message "Archive: %s" title)
             (org-cut-subtree))))))
    (org-clock-remove-overlays))))

(defun arvid-wl/clock-display-time-thisweek ()
    ""
  (interactive)
  (org-clock-display 'thisweek))

(defun arvid-wl/clock-display-time-lastweek ()
  ""
  (interactive)
  (org-clock-display 'lastweek))



;; (case 'foo
;;   ('thisweek -1)
;;   ('lastweek 7))

(defun arvid-wl/get-week-start-from-relative (week &optional time)
  ""
  (let* ((weekday (mod (- (decoded-time-weekday (decode-time time)) 1) 7))
         (delta (make-decoded-time :day (- (+ weekday
                                              (case week
                                                ('thisweek 0)
                                                ('lastweek 7)))))))
    (print weekday)
    (encode-time (decoded-time-add (decode-time time) delta))))

(cl-assert (string= "2021w01" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek (date-to-time "2021-01-10 00:00")))))
(cl-assert (string= "2021w02" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek (date-to-time "2021-01-11 00:00")))))
(cl-assert (string= "2021w02" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek (date-to-time "2021-01-14 00:00")))))
(cl-assert (string= "2021w02" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek (date-to-time "2021-01-17 00:00")))))
(cl-assert (string= "2021w03" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek (date-to-time "2021-01-18 00:00")))))
(cl-assert (string= "2020w53" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'lastweek (date-to-time "2021-01-10 00:00")))))
(cl-assert (string= "2021w01" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'lastweek (date-to-time "2021-01-11 00:00")))))
(cl-assert (string= "2021w01" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'lastweek (date-to-time "2021-01-14 00:00")))))
(cl-assert (string= "2021w01" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'lastweek (date-to-time "2021-01-17 00:00")))))
(cl-assert (string= "2021w02" (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'lastweek (date-to-time "2021-01-18 00:00")))))

(defun arvid-wl/spinoff-filename (week &optional time)
  ""
  (let* ((delta (make-decoded-time :day
                                   (case week
                                     ('thisweek 0)
                                     ('lastweek -7))))
         (time (encode-time (decoded-time-add (decode-time time) delta))))
    (format-time-string "%Yw%V.org" time)))

(cl-assert (string= (arvid-wl/spinoff-filename 'thisweek (date-to-time "2020-04-15 00:00")) "2020w16.org"))
(cl-assert (string= (arvid-wl/spinoff-filename 'lastweek (date-to-time "2020-04-15 00:00")) "2020w15.org"))

(defun arvid-wl/decode-buffer-name (file-name)
	""
    (if (string-match "\\([[:digit:]]+\\)w\\([[:digit:]]+\\).org" file-name)
        (let ((year (string-to-number (match-string 1 file-name)))
              (week (string-to-number (match-string 2 file-name))))
          (cond
           ((string= (format "%sw%s.org" year week) (format-time-string "%Yw%V.org"))
            'thisweek)
           ((string= (format "%sw%s.org" year (+ week 1)) (format-time-string "%Yw%V.org"))
            'lastweek))
          )))

(defun arvid-wl/worklog-check-sanity (week)
  ""
  (interactive (list (intern (ido-completing-read "Spin-off work log for which week? "
                                                  '("thisweek" "lastweek") nil t))))


  (setq org-clock-display-default-range 'lastweek)

  (let ((time-range-prop :org-clock-minutes-default)
        (time-range 'lastweek)
        (messages nil))

    (save-excursion
      ;; (find-file arvid-wl/worklog-file)
      (outline-show-all)
      (beginning-of-buffer)
      (outline-next-heading)

      (org-clock-display)
      (while (outline-on-heading-p)
        (pcase (org-heading-components)
          (`(,_ . (,_ . (,todo . (,_ . (,title . ,_)))))
           (let ((time (get-text-property (point) time-range-prop)))
             (if time
                 ;; 1. check that there is a todo state
                 (if (and (not todo) (not (arvid-wl/heading-has-leaves-p)))
                     (add-to-list 'messages
                                  `((,(line-number-at-pos) . ,(current-column))
                                    ,(format "missing TODO state in %s" title)))
                   )

               ;; 2. TODO: check that workitem times match
               ;;    - if this item is a work item, get time, position and start counting
               ;;      for each leaf, subtract time from workitem total
               ;;      if total is not 0 at the end, add errro
               ))
           ))
        (outline-next-heading)
        )
      (org-clock-remove-overlays))

    (if (not messages)
        t
      (when (get-buffer "*arvid-worklog/sanity*")
        (kill-buffer "*arvid-worklog/sanity*"))
      (with-current-buffer (get-buffer-create "*arvid-worklog/sanity*")
        (display-buffer (current-buffer))

        (insert (format "Sanity check in worklog file %s\n\n" arvid-wl/worklog-file))
        (dolist (msg messages)
          (let ((line (caar msg))
                (col (cdar msg))
                (msg (cdr msg)))
            (insert (format "%s:%d:%d: %s\n" arvid-wl/worklog-file line col msg))))

        (compilation-mode))
      nil)))

(defun arvid-wl/spinoff (week)
  ""
  (interactive (list (intern (ido-completing-read "Spin-off work log for which week? "
                                                  '("thisweek" "lastweek") nil t))))

  (cl-assert (file-exists-p arvid-wl/worklog-file))

  (if (arvid-wl/worklog-check-sanity week)

      (let ((weekly-worklog (f-join
                             (f-parent arvid-wl/worklog-file)
                             (arvid-wl/spinoff-filename week))))

        (message weekly-worklog)
        (find-file weekly-worklog)
        (insert-file arvid-wl/worklog-file)
        (org-mode-restart)
        )))

(defun arvid-wl/org-clock-in-hook-no-workitem ()
  "Hook that verifies that the user is not logging into a composed workitem"
  (cl-assert (not (string= (org-entry-get (point) "WORKITEM") "t"))
          nil
          "Cannot clock into to workitems: clock into one of its sub-headings"))

;; (add-to-list 'org-clock-in-prepare-hook 'arvid-wl/org-clock-in-hook-no-workitem)

(defun arvid-wl/overview ()
  ""
  (interactive)

  (let* ((today (format-time-string "CLOCK: [%Y-%m-%d"))
         (re (regexp-quote today)))
    (occur re)))

(defun arvid-wl/overview2 ()
  ""
  (interactive)

  (interactive (list (intern (ido-completing-read "Overview work log for which week? "
                                                  '("thisweek" "lastweek") nil t))))
  

  (save-excursion
    (when (get-buffer "*arvid-worklog/overview*")
      (kill-buffer "*arvid-worklog/overview*"))
    (let ((overview-buffer (get-buffer-create "*arvid-worklog/overview*"))
          (worklog-buffer (current-buffer)))



      (let ((today (format-time-string "CLOCK: [%Y-%m-%d"))
            (re (regexp-quote today))
            ;; todo: results should be a map: heading to list of clock ins
            ;; each clock in is a string
            (results nil))

        (while (re-seach-forward re)
          (arvid-wl/org-heading-title)
          ;; add to results
          )

        ;; for each headign in results
        ;;;  print heading
        ;;;  print checkins sorted
        ;;;  print total

        )
      )))


(defun arvid-wl/magit-commit-msg-add-week-no ()
  "Add year-week number as commit message"
  (interactive)
  (if nil
      (insert (format-time-string "%Yw%V" (arvid-wl/get-week-start-from-relative 'thisweek)))))

(provide 'arvid-worklog)
