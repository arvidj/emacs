;;; autoreload-mode.el --- Reload chrome after each save

;; Author: Arvid Jakobsson <arvid@bl00m.com>
;; URL: https://github.com/arvidj/phpunit.el
;; Version: 0.1.0
;; Keywords: chrome, reload, web

;;; License:

;; Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; See the code.

;;; Code:

(define-minor-mode autoreload-mode
  "Toggle Autoreload mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Autoreload mode is enabled, saving the current buffer
reloads the active tab in Google Chrome"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Autoreload"
  ;; The minor mode bindings.
  nil
  :group 'autoreload


  (if autoreload-mode
      (add-hook 'write-contents-functions 'arvid-save-reload-hook)
    (remove-hook 'write-contents-functions 'arvid-save-reload-hook)))

(defun arvid-save-reload-hook ()
  "Reloads Chrome."
  (shell-command "/Users/arvidjakobsson/bin/chrome-cli reload")
  nil)

(provide 'autoreload-mode)

;;; autoreload-mode.el ends here
