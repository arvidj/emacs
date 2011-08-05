;;; php-comint.el --- Run javascript in an inferior process window.

;;; Copyright (C) 2008 Paul Huff
     
;;; Author: Paul Huff <paul.huff@gmail.com>
;;; Maintainer: Paul Huff <paul.huff@gmail.com>
;;; Created: 26 May 2008
;;; Version: 0.0.1
;;; Package-Requires: ()
;;; Keywords: javascript, inferior-mode, convenience


;; php-comint.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; {at your option} any later version.

;; php-comint.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING, or type `C-h C-c'. If
;; not, write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Commentary:
;;; php-comint.el let's you run an inferior javascript process in emacs, 
;;; and defines a few functions for sending javascript input to it quickly.

;;  Usage: 
;;  Put php-comint.el in your load path
;;  Add (require 'php-comint) to your .emacs
;;  Set inferior-php-program-command to the execution command for running your javascript REPL
;;  (setq inferior-php-program-command "/path/to/executable <args>")
;;  Do: M-x run-php
;;  Away you go.

;;  I've added the following couple of lines to my .emacs to take advantage of 
;;  cool keybindings for sending things to the javascript interpreter inside
;;  of Steve Yegge's most excellent php2-mode.

;; (add-hook 'php2-mode-hook '(lambda () 
;;			    (local-set-key "\C-x\C-e" 'php-send-last-sexp)
;;			    (local-set-key "\C-\M-x" 'php-send-last-sexp-and-go)
;;			    (local-set-key "\C-cb" 'php-send-buffer)
;;			    (local-set-key "\C-c\C-b" 'php-send-buffer-and-go)
;;			    (local-set-key "\C-cl" 'php-load-file-and-go)
;;			    ))

;;  This is version 0.0.1, so I've only tested it on my own version of emacs which is currently:
;;  GNU Emacs 22.0.90.1 (i386-apple-darwin8.8.1, Carbon Version 1.6.0) of 2006-10-28
;;  Not sure if it'll work anywhere else, but it doesn't require anything apple-ish, just emacs-ish.

;; Additionally, I've only tested this with rhino.  I'm sure it'll probably work with spidermonkey, 
;; though if it barfs let me know, and I'll update it.

;; I'm a newbie elisper, so please let me know if I'm a. doing things the wrong way, b.
;; making things work they way they shouldn't in the elisp world.

;;; History:
;;

;;; Code:

(require 'comint)

(provide 'php-comint)

(defcustom inferior-php-program-command "/usr/local/bin/phpsh" "Path to the javascript interpreter")

(defgroup inferior-php nil
  "Run a javascript process in a buffer."
  :group 'inferior-php)

(defcustom inferior-php-mode-hook nil
  "*Hook for customizing inferior-php mode."
  :type 'hook
  :group 'inferior-php)

;;;###autoload
(defun run-php (cmd &optional dont-switch-p)
  "Run an inferior PHP process, input and output via buffer `*php*'.
If there is a process already running in `*php*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-php-program-command').
Runs the hook `inferior-php-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (if current-prefix-arg
			 (read-string "Run php: " inferior-php-program-command)
			 inferior-php-program-command)))
  (if (not (comint-check-proc "*php*"))
      (save-excursion (let ((cmdlist (split-string cmd)))
						(set-buffer (apply 'make-comint "php" (car cmdlist)
										   nil (cdr cmdlist)))
						(ansi-color-for-comint-mode-on)
						(setq comint-process-echoes t)
						(inferior-php-mode))))
  (setq inferior-php-program-command cmd)
  (setq inferior-php-buffer "*php*")
  (if (not dont-switch-p)
      (pop-to-buffer "*php*")))

;;;###autoload
(defun php-send-region (start end)
  "Send the current region to the inferior PHP process."
  (interactive "r")
  (run-php inferior-php-program-command t)
  (comint-send-region inferior-php-buffer start end)
  (comint-send-string inferior-php-buffer "\n"))

;;;###autoload
(defun php-send-region-and-go (start end)
  "Send the current region to the inferior PHP process."
  (interactive "r")
  (run-php inferior-php-program-command t)
  (comint-send-region inferior-php-buffer start end)
  (comint-send-string inferior-php-buffer "\n")
  (switch-to-php inferior-php-buffer))

;;;###autoload
(defun php-send-last-sexp-and-go ()
  "Send the previous sexp to the inferior Php process."
  (interactive)
  (php-send-region-and-go (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun php-send-last-sexp ()
  "Send the previous sexp to the inferior PHP process."
  (interactive)
  (php-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun php-send-buffer ()
  "Send the buffer to the inferior PHP process."
  (interactive)
  (php-send-region (point-min) (point-max)))


;;;###autoload
(defun php-send-buffer-and-go ()
  "Send the buffer to the inferior PHP process."
  (interactive)
  (php-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun php-load-file (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-php inferior-php-program-command t)
    (comint-send-string inferior-php-buffer (concat "load(\"" filename "\")\n"))))

;;;###autoload
(defun php-load-file-and-go (filename)
  "Load a file in the javascript interpreter."
  (interactive "f")
  (let ((filename (expand-file-name filename)))
    (run-php inferior-php-program-command t)
    (comint-send-string inferior-php-buffer (concat "load(\"" filename "\")\n"))
    (switch-to-php inferior-php-buffer)))

;;;###autoload
(defun switch-to-php (eob-p)
  "Switch to the javascript process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and inferior-php-buffer (get-buffer inferior-php-buffer))
          (php-interactively-start-process))
      (pop-to-buffer inferior-php-buffer)
    (error "No current process buffer.  See variable `inferior-php-buffer'"))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar inferior-php-buffer)

(defvar inferior-php-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-x\C-e" 'php-send-last-sexp)
    (define-key m "\C-cl" 'php-load-file)
    m))

;;;###autoload
(define-derived-mode inferior-php-mode comint-mode "Inferior PHP"
  "Major mode for interacting with an inferior javascript process.

The following commands are available:
\\{inferior-php-mode-map}

A javascript process can be fired up with M-x run-php.

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-php-mode-hook (in that order).

You can send text to the inferior PHP process from othber buffers containing
PHP source.
    switch-to-php switches the current buffer to the PHP process buffer.
    php-send-region sends the current region to the PHP process.


"
(use-local-map inferior-php-mode-map)
)
