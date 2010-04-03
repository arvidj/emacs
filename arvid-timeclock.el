;; timeclock
(require 'timeclock-x)

(global-set-key (kbd "C-c ti") 'timeclock-in)
(global-set-key (kbd "C-c to") 'timeclock-out)
(global-set-key (kbd "C-c tc") 'timeclock-change)
(global-set-key (kbd "C-c tr") 'timeclock-reread-log)
(global-set-key (kbd "C-c tu") 'timeclock-update-modeline)
(global-set-key (kbd "C-c tw") 'timeclock-when-to-leave-string)
(global-set-key (kbd "C-c tf") 'timeclock-visit-timelog)

(add-hook 'timeclock-in-hook '(lambda () (set-face-background 'mode-line "#444488")))
(add-hook 'timeclock-out-hook '(lambda () (set-face-background 'mode-line "#555753")))

(timeclock-initialize)

(provide 'arvid-timeclock)
