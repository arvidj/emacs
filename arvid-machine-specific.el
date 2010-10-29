;; Load features that are specific to different computers I use.

(cond
 ((equal system-name "arvid-desktop-gluteus-lvlup")
  (require 'arvid-sql-gluteus)))

(provide 'arvid-machine-specific)
