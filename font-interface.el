;;; font-interface.el --- Change your font without having to learn elisp!
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; The amount to increment/decrement font size by.
(defvar font-margin 10)
;; The face to edit
(defvar face-to-edit 'default)

(defun set-font-margin (new-font-margin)
  "Set a new font margin. Your font height will be
incremented/decremented by this value whenever you use 
increment-font-size or decrement-font-size."
  (interactive
   (list (read-number "Enter a new font margin: " font-margin)))
  (setq font-margin new-font-margin))

(defun set-face-to-edit(new-face)
  "Select a new face to edit."
  (interactive "MEnter the new face to edit: ")
  ;; Construct the symbol for the new face based on input
  (setq new-face (intern new-face))
  ;; Set
  (setq face-to-edit new-face))

(defun increment-font-size ()
  "Increment the font size by font-margin."
  (interactive)
  (let* ((current-font-size (face-attribute face-to-edit :height))
	 (new-font-size (+ current-font-size font-margin)))
    (set-face-attribute face-to-edit nil
			:height new-font-size)
    )
  )

(defun decrement-font-size ()
  "Decrement the font size by font-margin."
  (interactive)
  (let* ((current-font-size (face-attribute face-to-edit :height))
	 (new-font-size (- current-font-size font-margin)))
    (set-face-attribute face-to-edit nil
			:height new-font-size)
    )
  )

(defun set-font-family (font-family)
  "Set the font family for face-to-edit"
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
































