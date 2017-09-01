;;; font-interface.el --- A more user-friendly approach to changing
;;; the font and its attributes.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:
;; The amount to increment/decrement font size by.
(defvar font-margin 10)

(defun font-size-increment ()
  "An interactive function that allows you to increment your font size."
  (interactive)
  (let* ((current-font-size (face-attribute 'default :height))
	 (new-font-size (+ current-font-size font-margin)))
    (set-face-attribute 'default nil
			:height new-font-size)
    )
  )

(defun font-size-decrement ()
  "An interactive function that allows you to decrement your font size."
  (interactive)
  (let* ((current-font-size (face-attribute 'default :height))
	 (new-font-size (- current-font-size font-margin)))
    (set-face-attribute 'default nil
			:height new-font-size)
    )
  )


(provide 'font-interface)
;;; font-interface ends here
