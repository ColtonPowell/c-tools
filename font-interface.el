;;; font-interface.el --- A more user-friendly approach to changing
;;; the font and its attributes.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; The amount to increment/decrement font size by.
(defvar font-margin 10)
;; The face to edit
(defvar face-to-edit 'default)

;; Set face-to-edit
(defun set-face-to-edit(new-face)
  ;; Get the new face string. Remove the ' from the default face
  (interactive "MEnter the new face to edit: ")
  ;;(list (read-string "Enter the new face to edit: ")))

  ;; Construct the symbol for the new face based on input
  (setq new-face (intern new-face))

  ;; Set
  (setq face-to-edit new-face)
  )

;; Increment the font size by font-margin
(defun increment-font-size ()
  "An interactive function that allows you to increment your font size."
  (interactive)
  (let* ((current-font-size (face-attribute face-to-edit :height))
	 (new-font-size (+ current-font-size font-margin)))
    (set-face-attribute face-to-edit nil
			:height new-font-size)
    )
  )

;; Decrement the font size by font-margin
(defun decrement-font-size ()
  "An interactive function that allows you to decrement your font size."
  (interactive)
  (let* ((current-font-size (face-attribute face-to-edit :height))
	 (new-font-size (- current-font-size font-margin)))
    (set-face-attribute face-to-edit nil
			:height new-font-size)
    )
  )

;; perhaps expand to set-attribute
(defun set-font-family (font-family)
  "An interactive function that lets you get to setting your font
family quicker."
  (interactive "MEnter a font family:")
  ;; store font family and match-found as nil
  (let ((val (font-family-list))
	(match-found nil))
    ;; while val != nil or match isn't found, check every font family
    ;; to see if there is a match with the input.
    (while (or val (not match-found))
      (if (string= font-family (car val))
	  (setq match-found t))
      (setq val (cdr val))
      )
    ;; if match-found
    (if match-found
	(set-face-attribute face-to-edit nil
			    :family font-family)
      ;; else message err
      (message "Font family \"%s\" not found." font-family))
    )
  )


(provide 'font-interface)
;;; font-interface ends here
































