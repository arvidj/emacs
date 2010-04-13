;;;;;;;;;;;;;;;;;;;;;;
;; Interface and appeareances

;;; color-themes
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/plugins/twilight-emacs/color-theme-twilight.el")
(color-theme-twilight)

;; Kolumn
;; (require 'column-marker) 
(fset 'yes-or-no-p 'y-or-n-p)


;; gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Stäng av splash-screen
(setq inhibit-splash-screen t)


;; För parantes matchning
;; http://www.cs.cmu.edu/cgi-bin/info2www?(emacs)Matching
;; http://www.emacswiki.org/cgi-bin/wiki/HighlightParentheses
(show-paren-mode 1)
(setq show-paren-style 'expression) ;; matcha hela uttryck
(set-face-background 'show-paren-match-face "#333")
(set-face-foreground 'show-paren-match-face "#eee")

;; (transient-mark-mode -1)

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
