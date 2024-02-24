;;;;;;;;;;;;;;;;;;;;;;
;; Interface and appeareances

;;; color-themes

;; (load-theme 'modus-vivendi)
(load-theme 'modus-operandi)

;; Highlight surrounding parentheses.
(use-package
 highlight-parentheses
 :ensure t
 :config
 (highlight-parentheses-mode 1)
 (add-hook 'find-file-hook 'highlight-parentheses-mode t))

(show-paren-mode 1)
(global-hl-line-mode)

;; Kolumn
(fset 'yes-or-no-p 'y-or-n-p)

;; GUI
(dolist (mode
         '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (if (fboundp mode)
      (funcall mode -1)))

;; Disable splash screen
(setq inhibit-splash-screen t)


;; Font size
;;;; Default size
;; (set-face-attribute 'default nil :height 115)
;;;; Smaller
(set-face-attribute 'default nil :height 130)

;; fixa copy / paste till andra program
;; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)

;; För line-numbers vid sidan
;; http://stud4.tuwien.ac.at/~e0225855/linum/linum.html


;; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-height-threshold 80)
(setq split-width-threshold 160)


;; Makes last error visited in the compilation log more visible
(defface right-triangle-face
  '((t (:background "red" :foreground "yellow")))
  "Face for `right-triangle-face`.")
(set-fringe-bitmap-face 'right-triangle 'right-triangle-face)

(setq visible-bell t)

(provide 'arvid-gui)


;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-blue
;;    ((t (:background "deep sky blue" :foreground "deep sky blue"))))
;;  '(caml-types-expr-face ((t (:background "gray28"))))
;;  '(compilation-error ((t (:inherit font-lock-warning-face))))
;;  '(font-lock-comment-face ((t (:foreground "light gray"))))
;;  '(highlight-thing ((t (:inherit 'underline))))
;;  '(merlin-compilation-error-face
;;    ((t (:inherit default :underline (:color "#E44" :style wave)))))
;;  '(merlin-type-face
;;    ((t (:inherit caml-types-expr-face :background "SkyBlue4!100"))))
;;  '(proof-eager-annotation-face ((t (:background "peach puff"))))
;;  '(smerge-refined-added
;;    ((t (:inherit smerge-refined-change :background "olive drab"))))
;;  '(tuareg-font-lock-extension-node-face
;;    ((t
;;      (:inherit
;;       tuareg-font-lock-infix-extension-node-face
;;       :background "DodgerBlue4")))))
