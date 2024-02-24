;;; Org-mode

(defun aj/find-currently-hidden-column ()
  ""
  (save-excursion
    (catch 'break
      (cl-loop
       for col-num upfrom 0 do (org-table-goto-column col-num)
       (when (org-find-overlays 'invisible)
         (throw 'break col-num))
       (when (= (point) (point-at-eol))
         (throw 'break nil))))))

(defun aj/find-number-columns ()
  (save-excursion
    (catch 'break
      (cl-loop
       for col-num upfrom 0 do (org-table-goto-column col-num)
       (if (= (point) (point-at-eol))
           (throw 'break (1- col-num)))))))

;; Doesn't work.
(defun aj/org-mode-hook ()
  (flyspell-mode)
  ;; for speed
  (when (< (buffer-size) 10000)
    (flyspell-buffer))
  (auto-fill-mode))

(defun aj/org-latex-ignore-heading-filter-headline
    (headline backend info)
  "Strip headline from HEADLINE. Ignore BACKEND and INFO."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))

(defun aj/org-export-latest ()
  ""
  (interactive)
  ""
  (org-export-dispatch 4))

(defun aj/org-table-cell-goto-end ()
  ""
  (interactive)
  (if (search-forward-regexp "[[:space:]]*|")
      (goto-char (match-beginning 0))))

(defun aj/org-table-cell-goto-beginning ()
  ""
  (interactive)
  (if (search-backward-regexp "|[[:space:]]*")
      (goto-char (match-end 0))))

;; Stuff for org-mode at Nomadic Labs

(defun aj/org-to-latex-error ()
  ""
  (interactive)
  (find-file (f-swap-ext (buffer-file-name) "tex"))
  (read-only-mode t)

  (TeX-next-error))

(defun aj/org-screenshot-at-point ()
  "Interactively pick a region to screen shot,
    save it to the current folder and insert an org-link to it"
  (interactive)
  (let ((file-name
         (trim-string
          (shell-command-to-string
           "tempfile -d . -p 'ss-' -s '.png'"))))
    (shell-command (concat "import " file-name))
    (insert (concat "[[" file-name "]]"))))

(defun aj/org-google-scholar-heading ()
  ""
  (interactive)
  (let ((heading
         (nth
          4
          (ignore-errors
            (org-heading-components)))))
    (browse-url
     (concat "https://scholar.google.com/scholar?hl=en&q=" heading))))

(defun aj/org-toggle-noexport-tag ()
  ""
  (interactive)
  (let ((tags (org-get-tags)))
    (if (member "noexport" tags)
        (org-set-tags-to (delete "noexport" tags))
      (org-set-tags-to
       (cons
        "noexport"
        (if (string= (car tags) "")
            nil
          tags))))))

(use-package
 org
 :commands org-mode
 :ensure t
 :config


 (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


 (setq org-directory "~/Dropbox/Org")
 (setq org-default-notes-file (concat org-directory "/ideas.org"))
 (define-key global-map "\C-cc" 'org-capture)

 (setq org-capture-templates
       '(("t"
          "Todo"
          entry
          (file+headline "~/Data/Onebox/Research/todo.org" "Unsorted")
          "* TODO %?\n  %i\n  %a")
         ("j"
          "Journal"
          entry
          (file+datetree "journal.org")
          "* %?\nEntered on %U\n  %i\n  %a")
         ("c"
          "The Perfect Machine"
          entry
          (file+datetree "perfect-machine.org")
          "* %?\nEntered on %U\n  %i\n  %a")
         ("N"
          "Nomadic labs journal"
          entry
          (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org")
          "* Summary\n* Todo\n"
          :unnarrowed
          :immediate-finish)
         ("m"
          "Nomadic labs journal [monday]"
          entry
          (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org")
          "* Todo

 - [ ] [[*meditation][meditation]]
 - [ ] laboxy: [[https://nomadiclabs-lgv6x45n.laboxy.net/?destination=timesheets/input][online]] & [[file:Org/laboxy.org][laboxy.org]]: last week
 - [ ] plan week ([[https://calendar.google.com/calendar/u/0/r/week][google calendar]])
 - [ ] plan daily ([[https://calendar.google.com/calendar/u/0/r/day][google calendar]])
 - [ ] micromachine
 - [ ] merge team coordinator:
   - [ ] check [[https://gitlab.com/tezos/tezos/-/pipelines?page=1&scope=all&source=schedule][scheduled pipelines]] and [[https://gitlab.com/tezos/tezos/-/pipelines?page=1&scope=all&ref=master&source=push][merges on master]]
   - [ ] check out the [[file:~/dev/nomadic-labs/merge-coordination/tezos-merge-statuses/active.org][active.org]]
 - [ ] prepare [[https://hackmd.io/r9WOQntNR7iyVwn97q-QYw][merge team meeting]]
 - [ ] admin
"
          :unnarrowed
          :immediate-finish)
         ("r"
          "Random notes"
          entry
          (file+datetree "~/Data/Onebox/Notes/random-notes.org")
          "* %?\nEntered on %U\n  %i\n  %a")
         ("d"
          "Planning daily"
          entry
          (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org")
          "* Planning daily

 |  Time | Activity    | Comment | Status |
 |-------+-------------+---------+--------|
 |  8:30 |             |         |        |
 |  9:00 |             |         |        |
 |  9:30 |             |         |        |
 | 10:00 |             |         |        |
 | 10:30 |             |         |        |
 | 11:00 |             |         |        |
 | 11:30 |             |         |        |
 | 12:00 | -- Lunch -- |         |        |
 | 12:30 |             |         |        |
 | 13:00 |             |         |        |
 | 13:30 |             |         |        |
 | 14:00 |             |         |        |
 | 14:30 |             |         |        |
 | 15:00 |             |         |        |
 | 15:30 |             |         |        |
 | 16:00 |             |         |        |
 | 16:30 |             |         |        |
 | 17:00 |             |         |        |
 | 17:30 |             |         |        |
 | 18:00 |             |         |        |
 | 18:30 |             |         |        |
 | 19:00 |             |         |        |
 | 19:30 |             |         |        |

"
          :unnarrowed
          :immediate-finish)
         ("w"
          "Planning weekly"
          entry
          (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org")
          "* Planning weekly

 - Monday ::
 - Tuesday ::
 - Wednesday ::
 - Thursday ::
 - Friday ::"
          :unnarrowed
          :immediate-finish)))

 (global-set-key "\C-cl" 'org-store-link)
 (global-set-key "\C-ca" 'org-agenda)
 (global-set-key "\C-cb" 'org-iswitchb)
 (global-set-key "\C-cb" 'org-iswitchb)

 (aj/define-keys
  org-mode-map
  `(("C-," backward-kill-word)
    ("C-<return>" nil)

    ("C-'" nil)
    ("C-y" aj/yank-or-pop)

    ("M-a" org-beginning-of-line)
    ("C-e" nil)
    ("M-e" org-end-of-line)
    ("C-x C-e" LaTeX-environment)
    ("C-c [" LaTeX-environment)
    ("C-c ]" LaTeX-close-environment)

    ("C-c C-j" helm-org-in-buffer-headings)))

 (setq org-structure-template-alist
       '(("a" . "export ascii")
         ("c" . "center")
         ("C" . "comment")
         ("e" . "example")
         ("E" . "export")
         ("h" . "export html")
         ("l" . "export latex")
         ("q" . "quote")
         ("s" . "src")
         ("v" . "verse")))

 ;; https://orgmode.org/worg/exporters/beamer/ox-beamer.html
 ;; add only-environment

 (setq org-outline-path-complete-in-steps nil)
 (setq org-refile-targets '((nil :maxlevel . 3)))
 (setq org-html-head "
<style type=\"text/css\">
       /*<![CDATA[*/
         @import url(https://fonts.googleapis.com/css?family=Inconsolata|Inter&display=swap);body{margin:40px auto;max-width:900px;line-height:1.6;font-size:16px;color:#444;padding:0 10px;font-family:Inter,sans-serif}h1,h2,h3{line-height:1.2;font-family:Inter,sans-serif}img{width:700px;border-radius:10px}pre{font-family:Inconsolata,monospace}::selection{color:#fff;background:#ff4081}
       /*]]>*/
      </style>
")

 (org-clock-persistence-insinuate)
 (add-to-list 'org-agenda-files "~/Dropbox/Jobb/Nomadic_Labs/Org/")

 (org-babel-do-load-languages
  'org-babel-load-languages '((sqlite . t))))

(use-package remember :ensure t :after org)
(use-package org-clock :after org)
(use-package
 ox-beamer
 :after org
 :config
 (add-to-list
  'org-beamer-environments-extra
  '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}")))

(use-package
 ox-latex
 :after org
 :config
 (add-hook 'org-mode-hook 'aj/org-mode-hook)
 (add-hook 'org-ctrl-c-ctrl-c-final-hook 'aj/org-export-latest)

 (setq
  org-latex-pdf-process
  (quote
   ("latexmk -pdf -interaction=nonstopmode -output-directory=%o %f")))

 (add-to-list
  'org-latex-classes
  '("conference{llncs}"
    "\\documentclass[conference]{llncs}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (add-to-list
  'org-latex-classes
  '("arvidletter"
    "\\documentclass{letter}
                [DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
    ("\\comment{%s}" . "")
    ("\\comment{%s}" . "")
    ("\\comment{%s}" . "")
    ("\\comment{%s}" . "")
    ("\\comment{%s}" . ""))))
(use-package ox-md :after org)

;;; For remember in Org.
;;; http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
;; (org-remember-insinuate)
;; (define-key global-map "\C-cr" 'org-remember)

(provide 'arvid-org-mode)
