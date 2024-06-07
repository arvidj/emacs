(require 'package)
(require 'use-package)

(add-to-list
 'package-archives '("melpa" . "https://melpa.org/packages/")
 t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
(add-to-list
 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")
 t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(provide 'arvid-package)
