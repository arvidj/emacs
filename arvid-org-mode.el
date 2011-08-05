;;; Org-mode
(require 'remember)
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cb" 'org-iswitchb)

;;; For remember in Org.
;;; http://orgmode.org/manual/Setting-up-Remember.html#Setting-up-Remember
(org-remember-insinuate)
(define-key global-map "\C-cr" 'org-remember)
(define-keys org-mode-map
  `(("C-," backward-kill-word)
	("C-c fe" ,(make-inserter "ɛ"))
	("C-c fg" ,(make-inserter "ʒ"))
	("C-c fc" ,(make-inserter "ç"))
	("C-c fö" ,(make-inserter "ə"))))

(defvar arvid-org-francaise-overlays nil)
(defun arvid-org-francaise-toggle-overlays ()
  ""
  (interactive)
  (mapc 'delete-overlay arvid-org-francaise-overlays)

  ;; For each column

  (let* ((cur (find-currently-hidden-column))
		 (max (find-number-columns))
		 (hide-col (% (1+ (or cur 0)) max)))
	(goto-char (org-table-begin))
	(while (<= (point-at-eol) (1- (org-table-end)))
	  (beginning-of-line)
	  (unless (looking-at org-table-hline-regexp)
		(org-table-goto-column hide-col)
		(setq start (point))
		(re-search-forward "|") (backward-char 2)
		(let ((ov (org-make-overlay start (point))))
		  ;; (org-overlay-put ov 'face 'secondary-selection)
		  (org-overlay-put ov 'invisible t)
		  (push ov arvid-org-francaise-overlays)
		  ))
	  (next-line))))

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



(provide 'arvid-org-mode)
