(require 'package)
(require 'use-package)

(let* ((no-ssl
        (and (memq system-type '(windows-nt ms-dos))
             (not (gnutls-available-p))))
       (proto
        (if no-ssl
            "http"
          "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/"))
               t)
  (add-to-list 'package-archives
               (cons
                "melpa-stable"
                (concat proto "://stable.melpa.org/packages/"))
               t))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(provide 'arvid-package)
