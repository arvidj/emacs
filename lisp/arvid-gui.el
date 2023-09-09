;;;;;;;;;;;;;;;;;;;;;;
;; Interface and appeareances

;;; color-themes

(use-package
 nord-theme
 :ensure t
 :config
 (if (daemonp)
     (add-hook
      'after-make-frame-functions
      (lambda (frame)
        (with-selected-frame frame
          (load-theme 'nord t))))
   (load-theme 'nord t)))

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

(provide 'arvid-gui)
