;;;;;;;;;;;;;;;;;;;;;;
;; Interface and appeareances

;;; color-themes
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/plugins/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)

;; Highlight surrounding parentheses.
(require 'highlight-parentheses)
(highlight-parentheses-mode 1)
(add-hook 'find-file-hook 'highlight-parentheses-mode t)
(show-paren-mode 1)

;; Kolumn
;; (require 'column-marker) 
(fset 'yes-or-no-p 'y-or-n-p)

;; GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Stäng av splash-screen
(setq inhibit-splash-screen t)

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
(column-number-mode 1)

(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil)
(cua-mode)
