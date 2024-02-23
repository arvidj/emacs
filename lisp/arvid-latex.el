(add-to-list 'auto-mode-alist '("\\.tex" . latex-mode))
(add-hook 'plain-TeX-mode-hook 'LaTeX-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)
(add-hook 'LaTeX-mode-hook 'er/add-latex-mode-expansions)

;; Setup for sync

(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)
;; (setq TeX-view-program-list
;;       '(("Sumatra PDF" ("\"C:\\\\Program Files (x86)\\\\SumatraPDF\\\\SumatraPDF.exe\" -reuse-instance"
;;                         (mode-io-correlate " -forward-search %b %n ") " %o"))))

;; (eval-after-load 'tex
;;   '(progn
;;      (assq-delete-all 'output-pdf TeX-view-program-selection)
;;      (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))


;; Setup for externalize

(add-to-list
 'safe-local-variable-values
 '(TeX-command-extra-options . "-shell-escape"))

;; Inserting file names
(defun aj/tex-input ()
  (interactive)
  (let ((path (f-no-ext (read-file-name "File: ")))
        ;; TODO Should only complete on dir but do not know how!
        (rel-path
         (if TeX-master
             (f-dirname TeX-master)
           ".")))
    (insert
     (concat "\\input{" (file-relative-name path rel-path) "}"))))

(defun arvid-LaTeX-mode-hook ()
  ""
  (interactive)
  (define-key
   latex-mode-map (kbd "M-{")
   (lambda (arg)
     (interactive "P")
     (insert-pair arg ?\\ ?\\)
     (insert-pair arg ?{ ?})))
  (define-key
   latex-mode-map (kbd "C-{")
   (lambda (arg)
     (interactive "P")
     (insert-pair arg ?\\ ?\\)
     (insert-pair arg ?\[ ?\])))

  ;; (define-key latex-mode-map (kbd "C-c `") 'next-error)
  ;; (define-key tex-mode-map (kbd "C-c `") 'next-error)

  (define-key TeX-mode-map [remap next-error] nil)
  (define-key TeX-mode-map (kbd "C-c `") 'TeX-next-error)
  (define-key TeX-mode-map (kbd "C-c \\") 'TeX-next-error)
  (reftex-mode)
  (define-key reftex-mode-map (kbd "C-c \\") 'TeX-next-error)
  ;; (define-key reftex-mode-map [remap next-error] nil)
  ;; (define-key reftex-mode-map (kbd "C-x -") 'next-error)
  (define-key TeX-mode-map (kbd "C-c C-i") 'aj/tex-input)
  (reftex-mode)

  (define-key TeX-mode-map (kbd "C-c C-o") 'arvid-latex-open)
  (define-key TeX-mode-map (kbd "C-c m") 'arvid-goto-TeX-master)
  (define-key LaTeX-mode-map (kbd "C-c C-d") nil)
  ;; (define-key LaTeX-mode-map (kbd "C-c C-d d") 'dict-for-word-at-point)
  ;; (define-key LaTeX-mode-map (kbd "C-c C-d w") 'fr-wordreference-word-at-point)
  ;; (define-key LaTeX-mode-map (kbd "C-c C-d c") 'fr-wr-conjugate-word-at-point)
  ;; (define-key TeX-mode-map [remap helm-dabbrev] 'dabbrev-expand)

  (add-to-list 'TeX-view-program-selection '(output-pdf "Atril"))

  (if (file-exists-p ".spell")
      (progn
        (message "do spell")
        (flyspell-mode)
        (flyspell-buffer))
    (message "no spell"))

  (setq ispell-tex-skip-alists
        (list
         (append
          (car ispell-tex-skip-alists)
          '(("\\\\nospellcheck" ispell-tex-arg-end)))
         (cadr ispell-tex-skip-alists))))

(setq ispell-tex-skip-alists
      (list
       (append
        (car ispell-tex-skip-alists)
        '(("\\\\universite" ispell-tex-arg-end)
          ("\\\\laboratoire" ispell-tex-arg-end)
          ("\\\\specialite" ispell-tex-arg-end)
          ("\\\\colophon" ispell-tex-arg-end)
          ("\\\\rsevalprog" ispell-tex-arg-end)
          ("\\\\prref" ispell-tex-arg-end)
          ("\\\\rl" ispell-tex-arg-end)))
       (cadr ispell-tex-skip-alists)))

(add-hook 'LaTeX-mode-hook 'arvid-LaTeX-mode-hook)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (remove-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (remove-hook 'LaTeX-mode-hook 'preview-mode-setup)

(defun latex-insert-inline-math-brackets (open close)
  (interactive "k")
  (insert (concat "\\" open "\\" close))
  (backward-char (length (concat "\\" close))))

(defun latex-insert-inline-math-brackets ()
  (interactive)
  (insert "\\(\\)")
  (backward-char 2))


(defun latex-insert-inline-math-brackets-before ()
  (interactive)
  (backward-char 1)
  (insert "\\(")
  (forward-char 1)
  (insert "\\)"))

; egentligen vill jag ha en funktion som gör följande:
;  (1) om transient mark är aktiverat, omslut hela markeringen med \( \), men ignorera whitespace i början och slutet av markeringen
;  (2) om transient mark inte är aktiverat, omslut nästa hela ord med \( \)
; hur fungerar C-c C-o in latex mode? den gör ungefär vad jag vill

(fset 'latex-compile "\C-x\C-s\C-c\C-f\C-x1")

;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;; 	  '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

; mark-active - om transient mark är på?
; region-beginning region-end
; point är pekaren just nu
;     word-at-point hade hoppats på att använda den för (1) men den verkar inte göra vad jag vill, den har en striktare definition av word än vad jag har.
; save-excursion:
; re-search-forward

(defun latex-insert-inline-math-brackets-magic ()
  (interactive)
  (if transient-mark-mode
      (wrap-from-to-with (region-beginning) (region-end) "\\(" "\\)") ;; funkar!
    (let ((nn (next-non-whitespace (point))))
      (wrap-from-to-with nn (next-whitespace nn) "\\(" "\\)"))))

(defun next-whitespace (beg)
  (save-excursion
    (goto-char beg)
    (re-search-forward "\\([^[:blank:]]\\)\\([[:blank:]]\\|$\\)")
    (+ (match-beginning 1) 1)))

(defun next-non-whitespace (beg)
  (save-excursion
    (goto-char beg)
    (re-search-forward "[^[:blank:]]")
    (- (point) 1)))

(defun test-mark-active ()
  (interactive)
  (if transient-mark-mode
      (message "mark is active")
    (message "mark is inactive")))

(defun wrap-from-to-with (beg end left right)
  (save-excursion
    (goto-char beg)
    (insert left)
    (goto-char (+ end (length left)))
    (insert right)))

;; (add-to-list 'LaTeX-font-list '(?\C-q "{\\tiny" "}"))
;; (add-to-list LaTeX-font-list '(?\C-2 "{\\scriptsize" "}"))
;; (add-to-list LaTeX-font-list '(?\C-3 "{\\footnotesize" "}"))
;; (add-to-list LaTeX-font-list '(?\C-4 "{\\small" "}"))
;; (add-to-list LaTeX-font-list '(?\C-5 "{\\normalsize" "}"))
;; (add-to-list LaTeX-font-list '(?\C-6 "{\\large" "}"))
;; (add-to-list LaTeX-font-list '(?\C-7 "{\\Large" "}"))
;; (add-to-list LaTeX-font-list '(?\C-8 "{\\LARGE" "}"))
;; (add-to-list LaTeX-font-list '(?\C-9 "{\\huge" "}"))
;; (add-to-list LaTeX-font-list '(?\C-0 "{\\Huge" "}"))

(defun latex-factor (start end filename)
  (interactive "r\nF")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (write-region start end filename)
        (delete-region start end)
        (insert (concat "\\input{" filename "}"))
        (message (concat "Wrote " filename)))))

(defun arvid-ffap-latex-mode (name)
  ""
  (interactive)
  (let ((guess (concat (f-join (f-dirname TeX-master) name) ".tex")))
    (if (file-exists-p guess)
        guess
      (ffap-latex-mode name))))

;; todo: bind C-c C-o to
;;   - reftex-goto-label (at point): if we're at a cref/prref
;;   - ffap: if we're at a include

(defun arvid-reftex-toc-rename-label-at-pt ()
  "Rename the label at point.
This launches a global search and replace in order to rename a label.
Renaming a label is hardly ever necessary - the only exception is after
promotion/demotion in connection with a package like fancyref, where the
label prefix determines the wording of a reference."
  (interactive)
  (let* ((label (thing-at-point 'symbol))
         newlabel)
    (if (not (stringp label))
        (error "This is not a label entry"))
    (setq newlabel
          (read-string (format "Rename label \"%s\" to:" label)
                       label))
    (if (assoc newlabel (symbol-value reftex-docstruct-symbol))
        (if (not
             (y-or-n-p
              (format-message "Label `%s' exists.  Use anyway? "
                              label)))
            (error "Abort")))
    (save-excursion
      (save-window-excursion
        ;; (reftex-toc-visit-location t)
        (condition-case nil
            (reftex-query-replace-document
             (concat
              "{"
              (regexp-quote label) "}")
             (format "{%s}" newlabel))
          (error t))))
    (reftex-toc-rescan)))


(defun arvid-reftex-goto-label-at-point (&optional other-window)
  "Prompt for a label (with completion) and jump to the location of this label.
Optional prefix argument OTHER-WINDOW goes to the label in another window."
  (interactive "P")
  (reftex-access-scan-info)
  (let* ((wcfg (current-window-configuration))
         (docstruct (symbol-value reftex-docstruct-symbol))
         ;; If point is inside a \ref{} or \pageref{}, use that as
         ;; default value.
         (default (thing-at-point 'symbol))
         (label
          (completing-read (if default
                               (format "Label (default %s): " default)
                             "Label: ")
                           docstruct
                           (lambda (x) (stringp (car x)))
                           t
                           nil
                           nil
                           default))
         (selection (assoc label docstruct))
         (where
          (progn
            (reftex-show-label-location selection t nil 'stay)
            (point-marker))))
    (unless other-window
      (set-window-configuration wcfg)
      (switch-to-buffer (marker-buffer where))
      (goto-char where))
    (reftex-unhighlight 0)))

(defun arvid-latex-open ()
  "Find file at point (in input/include macros) or goes to definition of
label (in reference-macros)."
  (interactive)
  (let ((macro (TeX-current-macro)))
    (cond
     ((not (stringp macro))
      (message "No macro found"))
     ((member macro '("input" "include" "inpclude" "includegraphics"))
      (ffap))
     ((member macro '("cref" "ref" "prref"))
      (arvid-reftex-goto-label-at-point))
     ((stringp macro)
      (message (format "unknown macro type %s" macro))))))

(defun arvid-goto-TeX-master ()
  ""
  (interactive)
  (find-file TeX-master))

;; does not work: not sure why?

;; (defun arvid-update-compilation-lighter-modeline-hook (file)
;;   ""
;;   (if (TeX-error-report-has-errors-p)
;;       ;; Add sad lighter
;;       (set-face-background 'mode-line "#444488")
;;     ;; Otherwise happy
;;     (set-face-background 'mode-line "#436ab7")))

;; (add-to-list 'TeX-after-compilation-finished-functions
;;              'arvid-update-compilation-lighter-modeline-hook)

;; automatic synchronization: on point change, post-command-hook, if poitn is different from last, call TeX-viewx

(provide 'arvid-latex)
