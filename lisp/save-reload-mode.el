;;; wrap-region.el --- Wrap text with punctation or tag

;; Copyright (C) 2008-2010 Johan Andersson

;; Author: Arvid Jakobsson <arvid.jakobsson@gmail.com>
;; Maintainer: Arvid Jakobsson <arvid.jakobsson@gmail.com>
;; Version: 0.1.2
;; Keywords: speed, convenience

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Code:

;; TODO: Open up a web-socket server, on save -> send message. 

(defun reload-browsers ()
  (interactive)
  (shell-command "~/.reload-browsers"))

;;;###autoload
(define-minor-mode save-reload-mode
  "Reloads browsers on save"
  :init-value nil
  :lighter " sr"
  (if save-reload-mode
    (add-hook 'after-save-hook 'reload-browsers nil 'local)
	(remove-hook 'after-save-hook 'reload-browsers 'local)))

;;;###autoload
(defun turn-on-save-reload-mode ()
  "Turn on `save-reload-mode'"
  (interactive)
  (save-reload-mode +1))

;;;###autoload
(defun turn-off-save-reload-mode ()
  "Turn off `save-reload-mode'"
  (interactive)
  (save-reload-mode -1))

;;;###autoload
;; (define-globalized-minor-mode wrap-region-global-mode
;;   save-reload-mode
;;   turn-on-save-reload-mode)
