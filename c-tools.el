;;; c-tools.el --- A collection of handy emacs tools that may or may
;; not make you a more productive programmer.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

(defun make-makefile()
  "Creates a very simple c/c++ makefile for when you're feeling
  extremely, extremely lazy."
  (interactive)
  ;; Use (split-string (buffer-name) "\\.") to get a list ("c-tools"
  ;; "el") etc. Traverse to last elem (file type), read it, and decide
  ;; if c/c++.
  (let* ((buf-name (buffer-name))))
  )

(require 'resize-window-mode)
(require 'font-interface)
(require 'nav-tools)

(provide 'c-tools)
;;; c-tools ends here



