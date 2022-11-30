;; (setq helm-bibtex-bibliography '("~/Data/Onebox/Research/bibliography.bib" "~/Data/Onebox/Research/teamlibrary.bib"))

;; (setq helm-bibtex-bibliography '("~/Data/Onebox/Research/bibliography.bib"))
(setq helm-bibtex-bibliography
      '("~/Dropbox/Jobb/Nomadic_Labs/nl_bibliography.bib")
      )

(setq helm-bibtex-library-path '("~/Data/Onebox/Research/Library/"))
(setq helm-bibtex-pdf-field "File")
(setq helm-bibtex-notes-path "~/Dropbox/Jobb/Nomadic_Labs/nl_bibliography.org")

(setq helm-bibtex-format-citation-functions
      '((org-mode . helm-bibtex-format-citation-ebib)
        (latex-mode . helm-bibtex-format-citation-cite)
        (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
        (default . helm-bibtex-format-citation-default)))
(global-set-key (kbd "C-c h") 'helm-bibtex)

(defun helm-bibtex-edit-note-here (key)
  (let ((entry (helm-bibtex-get-entry key)))
    (insert (s-format helm-bibtex-notes-template-one-file
                      'helm-bibtex-apa-get-value
                      entry))))

(defun helm-bibtex-using-current-input ()
    ""
  (interactive)
  (helm :sources '(helm-source-bibtex helm-source-fallback-options)
        :full-frame helm-bibtex-full-frame
        :buffer "*helm bibtex*"
        :candidate-number-limit 500
        :input (car (org--property-local-values "Custom_ID" nil))
        ))


;; Tries to open the item as a PDF, URL or last fallback, in Google
;; Scholar
(defun helm-bibtex-pdf-url-or-scholar (key)
  (--if-let
      (-flatten
       (-map 'helm-bibtex-find-pdf (helm-marked-candidates :with-wildcard t)))
      (-each it helm-bibtex-pdf-open-function)

    ;; Could not open PDF, try URL
    (let ((keys (helm-marked-candidates :with-wildcard t)))
      (dolist (key keys)
        (let* ((entry (helm-bibtex-get-entry key))
               (url (helm-bibtex-get-value "url" entry))
               (doi (helm-bibtex-get-value "doi" entry))
               (browse-url-browser-function
                (or helm-bibtex-browser-function
                    browse-url-browser-function)))
          (if url (helm-browse-url url)
            (if doi (helm-browse-url (s-concat "http://dx.doi.org/" doi))
              ;; Could not open URL, try Google Scholar Search
              (dolist (key keys)
                (let* ((entry (helm-bibtex-get-entry key))
                       (title (helm-bibtex-get-value "title" entry))
                       (title-clean (helm-bibtex-clean-string title)))
                  (browse-url (concat "https://scholar.google.co.uk/scholar?q=" title-clean)))
                ))))))))


(use-package helm-bibtex
  :config
  (helm-add-action-to-source "PDF / URL / Scholar" 'helm-bibtex-pdf-url-or-scholar helm-source-bibtex)
  (helm-add-action-to-source "Edit note here" 'helm-bibtex-edit-note-here helm-source-bibtex))


(provide 'arvid-helm-bibtex)
