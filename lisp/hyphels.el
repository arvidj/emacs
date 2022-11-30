
;; Weaselwords
(defface writegood-hyphen-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (((class color) (background light))
     (:inherit font-lock-warning-face :background "moccasin"))
    (((class color) (background dark))
     (:inherit font-lock-warning-face :background "DarkOrange")))
  "Writegood face for hyphen words"
  :group 'writegood)

(defcustom writegood-hyphen-words
  '("data-flow")
  "The hyphen words to use"
  :group 'writegood
  :type '(repeat string))

(defun writegood-hyphen-font-lock-keywords-regexp ()
  "Generate regex that matches hyphen-words"
  ;; (concat "\\b" (regexp-opt writegood-hyphen-words) "\\b"))
  (concat "\\b" "\\sw+" "-" "\\sw+" "\\b"))

(defun writegood-hyphen-font-lock-keywords ()
  (list (list (writegood-hyphen-font-lock-keywords-regexp)
        0 (quote 'writegood-hyphen-face) 'prepend)))

(defun writegood-hyphen-turn-on ()
  (interactive)
  "Turn on syntax highlighting for hyphen"
  (font-lock-add-keywords nil (writegood-hyphen-font-lock-keywords) t))
