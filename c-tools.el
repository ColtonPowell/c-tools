;;; c-tools.el --- A collection of handy emacs tools that may or may
;; not make you a more productive programmer.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

(defun delete-word(arg)
  "Deletes arg words"
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word(arg)
  "Deletes arg words backwards."
  (interactive "p")
  (delete-word (- arg)))

(require 'resize-window-mode)
(require 'font-interface)
(require 'nav-tools)

(provide 'c-tools)
;;; c-tools ends here



