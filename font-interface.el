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

;; TODO:
;; - Throw errors if font-family is not a valid font
;;   > Can use font-family-list to get all fonts, then parse
(defun set-font-family (font-family)
  "An interactive function that lets you get to setting your font
family quicker."
  (interactive "MEnter a font family:")
  ;; store font family and match-found as nil
  (let ((val (font-family-list))
	(match-found nil))
    ;; should break if match-found
    (while val
      (if (string= font-family (car val))
	  (setq match-found t))
      (setq val (cdr val))
      )
    )
  ;; if match-found
  (set-face-attribute 'default nil
		      :family font-family)
  )
  ;; else message err


(provide 'font-interface)
;;; font-interface ends here
