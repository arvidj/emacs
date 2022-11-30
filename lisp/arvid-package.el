(require 'package)

(defun get-ip-address (&optional dev)
  "get the IP-address for device DEV (default: eth0)"
  (let ((dev (if dev dev "eth0"))) 
    (format-network-address (car (network-interface-info dev)) t)))

(let ((ip (or (get-ip-address "enp0s25")
              (get-ip-address "wlo1"))))
  (if (string= ip "10.203.78.214")
      (setq url-proxy-services
            '(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . "127.0.0.1:3128")
              ("https" . "127.0.0.1:3128")))
    (setq url-proxy-services nil)))

;; Currently removed since it is blocked at Alstom
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/"))) t)


(package-initialize)

;; as per https://emacs.stackexchange.com/questions/34201/emacs-cant-find-node-when-node-was-installed-using-nvm
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'arvid-package)
