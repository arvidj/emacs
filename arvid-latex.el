(add-hook 'LaTeX-mode-hook
	  '(lambda () 
	     ;; (define-key latex-mode-map (kbd "C-(") 
	     ;;   'latex-insert-inline-math-brackets-magic)
	     ;; (define-key latex-mode-map (kbd "C-)") 
 	     ;;   'latex-insert-inline-math-brackets-before)
	     ))

(add-hook 'latex-mode-hook 'flyspell-mode)
(global-hl-line-mode 1)

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

(fset 'latex-compile
   "\C-x\C-s\C-c\C-f\C-x1")

; mark-active - om transient mark är på?
; region-beginning region-end
; point är pekaren just nu
;     word-at-point hade hoppats på att använda den för (1) men den verkar inte göra vad jag vill, den har en striktare definition av word än vad jag har.
; save-excursion: 
; re-search-forward

(defun latex-insert-inline-math-brackets-magic () 
  (interactive)
  (if transient-mark-mode
      (wrap-from-to-with (region-beginning) (region-end) "\\(" "\\)" ) ;; funkar!
    (let ((nn (next-non-whitespace (point))))
       (wrap-from-to-with nn (next-whitespace nn) "\\(" "\\)" ))))

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