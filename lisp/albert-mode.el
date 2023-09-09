;; Indentation

;; definition begining are not indented
(defcustom albert-default-indent-size 2
  "default indentation increment")

(defcustom albert-definition-indent 0
  "Indentation of definition begining")
(defcustom albert-after-definition-indent
  (+ albert-definition-indent albert-default-indent-size)
  "Indentation of definition begining")
(defcustom albert-definition-start-keywords '("def" "type")
  "Keywords starting a new definition")

(defcustom albert-indent-comments nil
  "Shall we indent comments")

(defun is-definition-start-line ()
  "return true if the previously matched line start with a definition keyword"
  (beginning-of-line)
  (message "is definition %s ?" (match-string 1))
  (seq-contains albert-definition-start-keywords (match-string 1)))


(defun is-comment ()
  "return true if the current line is a comment"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*//")))

(defun is-first-pattern ()
  "return true if the current line follows a match"
  (save-excursion
    (forward-line -1)
    (while (not (non-empty-line))
      (forward-line -a))
    (beginning-of-line)
    (looking-at "[ \t]*match")))

(defun non-empty-line ()
  "return true if the line is not entirely made of space"
  (beginning-of-line)
  (looking-at "[ \t]*\\([^[:space:]\n\r]+\\)"))

(defun is-bar ()
  (beginning-of-line)
  (looking-at "[ \t]*|"))
(defun is-header ()
  (beginning-of-line)
  (looking-at "[ \t]*match"))
;; (require 'smie)
;; (defvar albert-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((id)
;;      (lhs (id)
;;           ("{" affect_list "}" )
;;           )
;;      (rhs (var "*" var )
;;           (var "+" var )
;;           (var "-" var )
;;           (var "/mod" var )
;;           ("dup" var )
;;           (var)
;;           (id var)
;;           )
;;      (inst
;;       (inst ";" inst)
;;       (var "=" rhs)
;;       ("match" var "with" inst "end"))
;;      (def ("type" var "=" tdef)
;;           ("def" fname type ":" type "=" inst )
;;           )))))

(defun albert-indent-line ()
  "Indent current line of albert code"
  (if (and (is-comment) (not albert-indent-comments)) ; comments are indented only if albert-indent-comments is set
      ()
    (progn
      (beginning-of-line)
      (if (bobp) ;first line of the file
          (indent-line-to 0)
        (let ((not-indented t)
              current-indent)
          (if (is-bar) ; | bar
              (save-excursion
                (while not-indented
                  (forward-line -1)
                  (cond
                   ((is-bar) ; align on previous bar
                    (message
                     "albert-indent-line : bar indented  relatively to bar at %s"
                     (what-line))
                    (setq current-indent (current-indentation))
                    (setq not-indented nil))
                   ((is-first-pattern) ; align on previous bar
                    (message
                     "albert-indent-line : bar indented  relatively to bar at %s"
                     (what-line))
                    (setq current-indent
                          (- (current-indentation)
                             albert-default-indent-size))
                    (setq not-indented nil))
                   ((non-empty-line) ; shift from previous text
                    (message
                     "albert-indent-line : bar indented  relatively to text at %s"
                     (what-line))
                    (setq current-indent
                          (+ (current-indentation)
                             albert-default-indent-size))
                    (setq not-indented nil))
                   ((bobp) ;align at zero if no previous text
                    (message
                     "albert-indent-line : bar indented  on first line")
                    (setq current-indent albert-definition-indent)
                    (setq not-indented nil))))))
          (if (non-empty-line)
              (cond
               ((is-definition-start-line) ;definition start at 0
                (message
                 "albert-indent-line : non-empty line indented as definition")
                (setq current-indent albert-definition-indent))
               ;; ((is-subdefinition-start-line)    ;subdefinition start at one tab
               ;;                          (message "albert-indent-line : non-empty line indented as sub-definition")
               ;;  (setq current-indent albert-subdefinition-indent)
               ;;  )
               (t ;default : not header, not bar
                (save-excursion
                  (while not-indented
                    (forward-line -1)
                    (if (non-empty-line)
                        (cond
                         ((is-header) ;text is shifted from header
                          (message
                           "albert-indent-line : non-empty indented relatively to header at %s"
                           (what-line))
                          (setq current-indent
                                (+ (current-indentation)
                                   albert-default-indent-size))
                          (setq not-indented nil))
                         ((is-definition-start-line) ;text is shifted from header
                          (message
                           "albert-indent-line : non-empty indented relatively to definition at %s"
                           (what-line))
                          (setq current-indent
                                (+ (current-indentation)
                                   albert-default-indent-size))
                          (setq not-indented nil))
                         ((is-bar) ;text is unshifted from previous bar
                          (message
                           "albert-indent-line : non-empty indented relatively to bar at %s"
                           (what-line))
                          (setq current-indent
                                (- (current-indentation)
                                   albert-default-indent-size))
                          (setq not-indented nil))
                         ((and (non-empty-line) (not (is-comment))) ;text is aligned with previous non-empty, non-header/comment line
                          (message
                           "albert-indent-line : non-empty indented relatively to text at %s"
                           (what-line))
                          (setq current-indent (current-indentation))
                          (setq not-indented nil))
                         ((bobp) ;align at zero if no previous text
                          (message
                           "albert-indent-line : non-empty indented on first line")
                          (setq current-indent 0)
                          (setq not-indented nil)))
                      ())))))
            (setq current-indent 0) ;empty line are indented at 0
            )
          (indent-line-to current-indent))))))
(defun indent-at-point ()
  "interactively use albert-indent-line (debug purpose)"
  (interactive)
  (albert-indent-line))


(define-generic-mode
 'albert-mode
 ;; comments:
 '("//")
 ;; keywords:
 '("def" "type" "match" "with" "end" "=>" ";")
 ;; more font-lock
 '((":[[:space]]* \\([:word:]\\)" . (1 font-lock-face)))
 ;; auto-mode-alist
 (list "\\.alb0?\\'")
 ;; Pre mode-hook (setting indentation)
 (list
  (lambda ()
    (set
     (make-local-variable 'indent-line-function) 'albert-indent-line))
  (lambda () (set (make-local-variable 'tab-always-indent) t)))
 "Major mode for editing albert format files.")


;    ("\\%.*" . 'font-lock-doc-face)

(provide 'albert-mode)
