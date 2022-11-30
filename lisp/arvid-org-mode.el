;;; Org-mode
;; (add-to-list 'load-path "~/.emacs.d/plugins/org-mode/lisp")

(require 'remember)

(require 'org)
(require 'org-clock)

(require 'ox-beamer)
(require 'ox-latex)
(require 'ox-taskjuggler)
(require 'ox-reveal)
(require 'ox-md)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


(setq org-directory "~/Dropbox/Org")
(setq org-default-notes-file (concat org-directory "/ideas.org"))
(define-key global-map "\C-cc" 'org-capture)

;; (setq org-capture-templates nil)
(defun feeling-template ()
    (let ((f (read-string "Feeling: ")))
      (concat "* Feelings\n     :PROPERTIES:\n     :Feeling: "
              f
              "\n     :END:\n")))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Data/Onebox/Research/todo.org" "Unsorted")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("i" "Idea" entry (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/ideas.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("Q" "Question" entry (file+datetree "~/Data/Onebox/Research/questions.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Study day" entry (file+datetree "notes.org")
         "* Administration%?\n* Coq\n* BSPlib\n* Abstract reading / bibliography\n* Reading\n* Writing\n" :unnarrowed :immediate-finish)
        ("I" "Implementation day" entry (file+datetree "notes.org")
         "* Administration%?\n* Abstract reading / bibliography\n* Writing\n* Implementation\n" :unnarrowed :immediate-finish)
        ("N" "Nomadic labs journal" entry (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org") "* Summary\n* Todo\n" :unnarrowed :immediate-finish)
        ("m" "Nomadic labs journal [monday]" entry (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org") "* Todo

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
" :unnarrowed :immediate-finish)
        ("f" "Feelings" entry (file+datetree "~/Data/Onebox/Notes/feelings.org")
         (function feeling-template)
          :unnarrowed :immediate-finish)
        ("r" "Random notes" entry (file+datetree "~/Data/Onebox/Notes/random-notes.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("d" "Planning daily" entry (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org") "* Planning daily

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

" :unnarrowed :immediate-finish)
        ("w" "Planning weekly" entry (file+datetree "~/Dropbox/Jobb/Nomadic_Labs/notes.org") "* Planning weekly

 - Monday :: 
 - Tuesday :: 
 - Wednesday :: 
 - Thursday :: 
 - Friday ::" :unnarrowed :immediate-finish)
        ("a" "Abstract reading" entry (file+datetree "~/Data/Onebox/Research/abstract-reading.org") "* Abstracts\n" :unnarrowed :immediate-finish)
        ("W" "NL Weeklog" entry (function nl-weeklog-location) "" :unnarrowed :immediate-finish)
        ("q" "NL Last Weeklog" entry (function nl-last-weeklog-location) "" :unnarrowed :immediate-finish)
        ))

(defvar nl-worklog-dir "~/dev/nomadic-labs/worklog/")


(defun nl-weeklog-location ()
    ""
    (find-file (concat (file-name-as-directory nl-worklog-dir) (format-time-string "%Gw%V.org")))
    (when (= (buffer-size) 0)
      (insert-file "template.org")))


(defun nl-last-weeklog-location ()
  ""
  (find-file (concat (file-name-as-directory nl-worklog-dir) (format-time-string "%Gw%V.org" (time-subtract (current-time) (* 24 7 3600)))))
  (when (= (buffer-size) 0)
    (insert-file "template.org")))


(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cb" 'org-iswitchb)

;;; For remember in Org.
;;; http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
;; (org-remember-insinuate)
;; (define-key global-map "\C-cr" 'org-remember)
(define-keys org-mode-map
  `(("C-," backward-kill-word)
    ("C-<return>" nil)

    ("C-'" nil)
    ("C-y" arvid-yank-or-pop)

	("M-a" org-beginning-of-line)
    ("C-e" nil)
    ("M-e" org-end-of-line)
    ("C-x C-e" LaTeX-environment)
    ("C-c [" LaTeX-environment)
    ("C-c ]" LaTeX-close-environment)

    ("C-c C-j" helm-org-in-buffer-headings)

    ))


;; (setq org-latex-pdf-process
;;       (quote
;;        ("pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "bibtex %b"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -interaction nonstopmode -output-directory %o %f")))


;; (setq org-latex-pdf-process
;;       (quote
;;        ("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "%latex -interaction nonstopmode -output-directory %o %f"
;;         "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(setq org-latex-pdf-process
      (quote
       ;; ("pdflatex -interaction nonstopmode -output-directory %o %f")
       ("latexmk -pdf -interaction=nonstopmode -output-directory=%o %f")
       ))


;; (setq org-latex-pdf-process
;;       (quote ("latexmk.exe -pdf %f")))


(defun find-currently-hidden-column ()
  ""
  (save-excursion
	(catch 'break
	  (loop for col-num upfrom 0 do
			(org-table-goto-column col-num)
			(when (org-find-overlays 'invisible)
			  (throw 'break col-num))
			(when (= (point) (point-at-eol))
			  (throw 'break nil))))))

(defun find-number-columns ()
    (save-excursion
	(catch 'break
	  (loop for col-num upfrom 0 do
			(org-table-goto-column col-num)
			(if (= (point) (point-at-eol))
			  (throw 'break (1- col-num)))))))

(defun my-org-clock-formatter ()
  ""
  (interactive)
  
  )

;; Doesn't work.
(defun arvid-org-mode-hook ()
  ;; (push '(?* . ?*)
  ;; 		(getf autopair-extra-pairs :everywhere))
  ;; (set-visual-wrap-column 70)

  ;; (add-to-list 'org-latex-classes
  ;;   		   '("scrreprt"
  ;;   			 "\\documentclass\{scrreprt\}
  ;;    \\usepackage[english]{babel}
  ;;    \[DEFAULT-PACKAGES]
  ;;    \[PACKAGES]
  ;;    \[EXTRA]"
  ;;   			 ("\\chapter{%s}" . "\\chapter*{%s}")
  ;;   			 ("\\section{%s}" . "\\section*{%s}")
  ;;   			 ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;   			 ;; ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;   			 ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;   			 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")

  ;;                ))

  ;; (setq org-latex-pdf-process
  ;;       (quote ("latexmk.exe -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f  %f")))


  ;; (setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))



  ;; (add-to-list org-export-filter-headline-functions
  ;;   	'org-latex-ignore-heading-filter-headline)

  (flyspell-mode)
  ;; for speed
  (when (< (buffer-size) 10000)
    (flyspell-buffer))
  (auto-fill-mode))

(add-hook 'org-mode-hook 'arvid-org-mode-hook)



(defun org-latex-ignore-heading-filter-headline (headline backend info)
  "Strip headline from HEADLINE. Ignore BACKEND and INFO."
  (when (and (org-export-derived-backend-p backend 'latex)
             (string-match "\\`.*ignoreheading.*\n" headline))
    (replace-match "" nil nil headline)))

;; (defun org-remove-headlines (backend)
;;   "Remove headlines with :no_title: tag."
;;   (org-map-entries (lambda () (let ((beg (point)))
;;                                 (outline-next-visible-heading 1)
;;                                 (backward-char)
;;                                 (delete-region beg (point))))
;;                    "no_export" tree)
;;   (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
;;                    "no_title"))

;; (add-hook 'org-export-before-processing-hook #'org-remove-headlines)



;; (add-to-list 'org-latex-classes)

;; (add-to-list 'org-latex-packages-alist '("" "listings"))
;; (add-to-list 'org-latex-packages-alist '("" "color"))

(defun arvid-org-export-latest ()
    ""
  (interactive)
  ""
  (org-export-dispatch 4))

;; (remove-hook 'org-ctrl-c-ctrl-c-hook 'arvid-org-export-latest)
(add-hook 'org-ctrl-c-ctrl-c-final-hook 'arvid-org-export-latest)

(add-to-list 'org-latex-classes
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

;; (setq org-latex-classes nil)
(add-to-list 'org-latex-classes
             '("arvidletter"
               "\\documentclass{letter}
                [DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"
               ("\\comment{%s}" . "")
               ("\\comment{%s}" . "")
               ("\\comment{%s}" . "")
               ("\\comment{%s}" . "")
               ("\\comment{%s}" . "")
               ))


;; (add-to-list 'org-structure-template-alist
;;              '("b" "#+BEGIN_EXPORT beamer\n?\n#+END_EXPORT"))

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
        ("v" . "verse"))
      )

;; https://orgmode.org/worg/exporters/beamer/ox-beamer.html
;; add only-environment

(add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets '((nil :maxlevel . 3)))
(setq org-html-head "
<style type=\"text/css\">
       /*<![CDATA[*/
         @import url(https://fonts.googleapis.com/css?family=Inconsolata|Inter&display=swap);body{margin:40px auto;max-width:900px;line-height:1.6;font-size:16px;color:#444;padding:0 10px;font-family:Inter,sans-serif}h1,h2,h3{line-height:1.2;font-family:Inter,sans-serif}img{width:700px;border-radius:10px}pre{font-family:Inconsolata,monospace}::selection{color:#fff;background:#ff4081}
       /*]]>*/
      </style>
")


(autoload 'orch-toggle "orch" nil t)


(defun org-table-cell-goto-end ()
  ""
  (interactive)
  (if (search-forward-regexp "[[:space:]]*|")
      (goto-char (match-beginning 0))))

(defun org-table-cell-goto-beginning ()
  ""
  (interactive)
  (if (search-backward-regexp "|[[:space:]]*")
      (goto-char (match-end 0))))


(defun arvid-fixer-get (euro)
    ""
  (interactive)
  
  )

(org-clock-persistence-insinuate)

(require 'org-drill)

;; Stuff for org-mode at Nomadic Labs

(add-to-list 'org-agenda-files "~/Dropbox/Jobb/Nomadic_Labs/Org/")

(defun arvid/org-drill-insert-item (question answer)
  ""
  (interactive "MQuestion: \nMAnwer: ")
  (pcase (org-heading-components)
    (`(,_ . (,_ . (,_ . (,_ . (,title . ,_)))))

     (org-insert-heading)
     (if (string= title "Answer")
         (org-do-promote)
       (org-do-demote))
     (insert "Drill")
     (org-set-tags ":drill:")
     (org-return)
     (org-return)
     (insert question)

     (org-insert-heading)
     (org-do-demote)
     (insert "Answer")
     (newline)
     (newline)
     (insert answer))
    (_ nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))


(provide 'arvid-org-mode)
