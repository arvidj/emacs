;; TODO: auto-load

;;; Haskell
;; (load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(defun my-haskell-mode-hook ()
  ""
  (interactive)
  (interactive-haskell-mode)
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
  (define-key interactive-haskell-mode-map (kbd "M-n") nil)
  (define-key interactive-haskell-mode-map (kbd "M-p") nil)
  (define-key
   interactive-haskell-mode-map
   (kbd "C-x `")
   'haskell-goto-next-error)
  (define-key
   interactive-haskell-mode-map (kbd "s-j") 'haskell-goto-next-error)
  (define-key
   interactive-haskell-mode-map (kbd "s-k") 'haskell-goto-prev-error))


;;;; hs-lint
;; (require 'hs-lint)

(provide 'arvid-haskell)


;; http://www.emacswiki.org/emacs/PrettyLambda#toc3
;; (defun unicode-symbol (name)
;;   "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
;;   or GREATER-THAN into an actual Unicode character code. "
;;   (decode-char 'ucs (case name
;; 		      ;; arrows
;; 		      ('left-arrow 8592)
;; 		      ('up-arrow 8593)
;; 		      ('right-arrow 8594)
;; 		      ('down-arrow 8595)
;;
;; 		      ;; boxes
;; 		      ('double-vertical-bar #X2551)
;;
;; 		      ;; relational operators
;; 		      ('equal #X003d)
;; 		      ('not-equal #X2260)
;; 		      ('identical #X2261)
;; 		      ('not-identical #X2262)
;; 		      ('less-than #X003c)
;; 		      ('greater-than #X003e)
;; 		      ('less-than-or-equal-to #X2264)
;; 		      ('greater-than-or-equal-to #X2265)
;;
;; 		      ;; logical operators
;; 		      ('logical-and #X2227)
;; 		      ('logical-or #X2228)
;; 		      ('logical-neg #X00AC)
;;
;; 		      ;; misc
;; 		      ('nil #X2205)
;; 		      ('horizontal-ellipsis #X2026)
;; 		      ('double-exclamation #X203C)
;; 		      ('prime #X2032)
;; 		      ('double-prime #X2033)
;; 		      ('for-all #X2200)
;; 		      ('there-exists #X2203)
;; 		      ('element-of #X2208)
;;
;; 		      ;; mathematical operators
;; 		      ('square-root #X221A)
;; 		      ('squared #X00B2)
;; 		      ('cubed #X00B3)
;;
;; 		      ;; letters
;; 		      ('lambda #X03BB)
;; 		      ('alpha #X03B1)
;; 		      ('beta #X03B2)
;; 		      ('gamma #X03B3)
;; 		      ('delta #X03B4))))
;;
;; (defun substitute-pattern-with-unicode (pattern symbol)
;;   "Add a font lock hook to replace the matched part of PATTERN with the
;;   Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
;; 					     ,(unicode-symbol symbol))
;; 			     nil))))))
;;
;; (defun substitute-patterns-with-unicode (patterns)
;;   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
;;   (mapcar #'(lambda (x)
;; 	      (substitute-pattern-with-unicode (car x)
;; 					       (cdr x)))
;; 	  patterns))
;;
;; (defun haskell-unicode ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list (cons "\\(<-\\)" 'left-arrow)
;; 	 (cons "\\(->\\)" 'right-arrow)
;; 	 (cons "\\(==\\)" 'identical)
;; 	 (cons "\\(/=\\)" 'not-identical)
;; 	 (cons "\\(()\\)" 'nil)
;; 	 (cons "\\<\\(sqrt\\)\\>" 'square-root)
;; 	 (cons "\\(&&\\)" 'logical-and)
;; 	 (cons "\\(||\\)" 'logical-or)
;; 	 (cons "\\<\\(not\\)\\>" 'logical-neg)
;; 	 (cons "\\(>\\)\\[^=\\]" 'greater-than)
;; 	 (cons "\\(<\\)\\[^=\\]" 'less-than)
;; 	 (cons "\\(>=\\)" 'greater-than-or-equal-to)
;; 	 (cons "\\(<=\\)" 'less-than-or-equal-to)
;; 	 (cons "\\<\\(alpha\\)\\>" 'alpha)
;; 	 (cons "\\<\\(beta\\)\\>" 'beta)
;; 	 (cons "\\<\\(gamma\\)\\>" 'gamma)
;; 	 (cons "\\<\\(delta\\)\\>" 'delta)
;; 	 (cons "\\(''\\)" 'double-prime)
;; 	 (cons "\\('\\)" 'prime)
;; 	 (cons "\\(!!\\)" 'double-exclamation)
;; 	 (cons "\\(\\.\\.\\)" 'horizontal-ellipsis))))
;;
;; (add-hook 'haskell-mode 'haskell-unicode)
;;
;; (defun flymake-Haskell-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-Haskell-cmdline))
;;
;; (defun flymake-get-Haskell-cmdline (source base-dir)
;;   (list "flycheck_haskell.pl"
;; 	(list source base-dir)))
;;
;; (push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)
;;
;; The following code is introduced at
;; http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/
;;
;; (defun credmp/flymake-display-err-minibuf ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (let* ((line-no             (flymake-current-line-no))
;; 	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;; 	 (count               (length line-err-info-list))
;; 	 )
;;     (while (> count 0)
;;       (when line-err-info-list
;; 	(let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;; 	       (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;; 	       (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;; 	       (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;; 	  (message "[%s] %s" line text)
;; 	  )
;; 	)
;;       (setq count (1- count)))))
;;
;; ;; Bind the above function to the key ’\C-c d’.

;; (add-hook
;;  'haskell-mode-hook
;;  #'(lambda ()
;;     (define-key haskell-mode-map "\C-cd"
;;       'credmp/flymake-display-err-minibuf)))

;; Add the following setting to see the error/warning message at the minibuffer.

;; (when (fboundp 'resize-minibuffer-mode) ; for old emacs
;;   (resize-minibuffer-mode)
;;   (setq resize-minibuffer-window-exactly nil))

;; optional setting
;; if you want to use flymake always, then add the following hook.
;; (add-hook
;;  'haskell-mode-hook
;;  #'(lambda ()
;;     (if (not (null buffer-file-name)) (flymake-mode))))
