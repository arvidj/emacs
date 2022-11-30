
;;;;;;;;;;;;;;;;;;;;;;
;; Interface and appeareances

;;; color-themes

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/zenburn")
;; (load-theme 'purple-haze t)
(load-theme 'zenburn t)

(add-to-list 'custom-theme-load-path "/home/arvid/.emacs.d/elpa/birds-of-paradise-plus-theme-0.1.1")
(require 'birds-of-paradise-plus-theme)
(load-theme 'birds-of-paradise-plus t)

(add-to-list 'custom-theme-load-path "/home/arvid/.emacs.d/elpa/nord-theme-0.5.0")
(load-theme 'nord t)

(with-eval-after-load "ample-theme"
  (custom-theme-set-faces
   'nord
   '(markdown-header-face-1 nil (( t (:foreground nil :inherit 'org-level-1))))
   '(markdown-header-face-2 nil (( t (:foreground nil :inherit 'org-level-2))))
   '(markdown-header-face-3 nil (( t (:foreground nil :inherit 'org-level-3))))
   '(markdown-header-face-4 nil (( t (:foreground nil :inherit 'org-level-4))))
   '(markdown-header-face-5 nil (( t (:foreground nil :inherit 'org-level-5))))
   '(markdown-header-face-6 nil (( t (:foreground nil :inherit 'org-level-6))))))

;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized t)

;; (add-to-list 'custom-theme-load-path "/home/arvid/.emacs.d/lisp/")
;; (load "pink-bliss.el")
;; (require 'pink-bliss-theme)
;; (nyan-mode 1)
;; (load-theme 'pinkbliss t)

;; Highlight surrounding parentheses.
(require 'highlight-parentheses)
(highlight-parentheses-mode 1)
(add-hook 'find-file-hook 'highlight-parentheses-mode t)
(show-paren-mode 1)
(global-hl-line-mode)

;; Kolumn
;; (require 'column-marker) 
(fset 'yes-or-no-p 'y-or-n-p)

;; GUI
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
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

;; With org-mode, it make crash all emacs with a “Fatal error (10)Bus
;; error” when you try to collapse a tree (S-tab) A similar problem
;; was recently fixed in the Emacs core. Please try the most recent
;; CVS version.

;; linum mode is terribly slow at editing large slow. turning it off 
;; for now.
;; (global-linum-mode 1)
;; (column-number-mode 1)

;; (setq cua-enable-cua-keys nil)
;; (setq cua-toggle-set-mark nil)
;; (cua-mode)


