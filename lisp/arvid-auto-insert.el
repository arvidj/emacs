(defun mit-spdx-hdr ()
  "docstring"
  (interactive)
  (let ((year (calendar-extract-year (calendar-current-date))))
    (format
     "(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) %d Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)
"
     year)))

(use-package
 autoinsert
 :init (auto-insert-mode t)
 (setq auto-insert-alist
       '((tuareg-mode . (lambda () (insert (mit-spdx-hdr)))))))

(provide 'arvid-auto-insert)
