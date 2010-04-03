;; Processing
(add-to-list 'load-path "~/.emacs.d/plugins/processing-emacs/")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "~/processing/")


