(defun aj/mit-spdx-header ()
  ""
(format
"(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) %s Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

" (format-time-string "%Y")))



(use-package autoinsert
  :init
  (auto-insert-mode t)
  (setq auto-insert-alist '((tuareg-mode . (lambda () (insert (aj/mit-spdx-header)))))))

(provide 'arvid-auto-insert)
