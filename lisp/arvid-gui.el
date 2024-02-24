;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Interface and appearance

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

;; Highlights parenthesis matching the one at point.
(show-paren-mode 1)

;; Globally highlight the current line.
(global-hl-line-mode)

(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;
;; GUI

;; Hide menu-bar, tool-bar, scroll-bar and tooltips.
(dolist (mode
         '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (if (fboundp mode)
      (funcall mode -1)))

;; Disable splash screen
(setq inhibit-splash-screen t)


;; Font size
(set-face-attribute 'default nil :height 130)

;; Fix copy / paste between X programs.
;; http://www.emacswiki.org/emacs/CopyAndPaste
(setq x-select-enable-clipboard t)

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
