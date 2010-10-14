;; TODO: A way of collapsing / expanding function calls. Ie, transform:
;;	  fun(a, b, c)
;;	 into
;;	  fun(
;;		 a,
;;		 b,
;;		 c
;;	  )
;; For fun and profit
;;
;; TODO: Fix / when stopping block comments.
;;   inserting / when point is at x:
;;    /**
;;     *
;;     * x
;;   then remove space between x and preceding *
;;
;;  Also alt-j at /** does not prefix next line with *

;; Create my personal style.
(defconst my-c-style
  '((c-hanging-braces-alist     . ((defun-open after)
								   (substatement-open after)))
    (c-offsets-alist            . ((case-label        . +)
				   (arglist-close     . 0)
				   (comment-intro     . 0)))
    (c-hanging-semi&comma-criteria nil))  ;; Does not work
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; other customizations
  (setq c-basic-offset 4)
  (setq tab-width 4)

  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1)
  
  ;; TODO: make this work with transposeWords, so that case is
  ;; preserved in an intelligent fashion. That is, if point is at:
  ;;   this.fooBar
  ;;           ^
  ;; and transpose words is used, the result shall become:
  ;;   that.barFoo
  ;; instead of
  ;;   that.Barfoo
  ;; which is useless.
  (c-subword-mode 1)
  ;; (auto-complete-mode 1)

  ;; need to unset this so that I can use it for window-mgmt
  (local-unset-key (kbd "C-M-j")) 
  (local-unset-key (kbd "C-M-k")) 

  ;; (flymake-mode 1)
  (c-toggle-electric-state 1)

  ;; Set fill width to 80 columns
  (setq fill-column 80)

  ;; Try to make auto-pair + electric mode work together.
  ;; (setq autopair-handle-action-fns '(my-test-handler))

  (define-keys c-mode-map
	`(("M-a" nil)
	  ("ä" ,(make-inserter "$"))
	  ("ö" ,(make-inserter ";"))
	  (";" report-intelligence-level)
	  ("$" report-intelligence-level)
	  ("M-ä" ,(make-inserter "ä")))))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; (defun my-test-handler (a b c)
;;   (autopair-default-handle-action a b c)
;;   (open-line 1)
;;   (next-line)
;;   (indent-according-to-mode)
;;   (previous-line)
;;   (indent-according-to-mode))

(provide 'arvid-c)
