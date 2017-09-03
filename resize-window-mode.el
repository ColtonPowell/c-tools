;;; resize-window-mode.el --- The window resizing component of
;;; c-tools.el
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; Lists containing the strings of the keys to be defined in the
;; resize-window-mode keymap
(defvar rw-enlarge-vertically-keys (list nil))
(defvar rw-shrink-vertically-keys (list nil))
(defvar rw-enlarge-horizontally-keys (list nil))
(defvar rw-shrink-horizontally-keys (list nil))
(defvar rw-exit-keys (list nil))

;; Use to enable/disable the default keys. Set to nil to disable them.
(defvar rw-enable-default-keys t)

;; The value that determines the number of resize actions per
;; keypress. Default is 3.
(defvar rw-margin 3)

;; Set the default margin
(defun rw-set-margin (new-margin)
  "Set a new margin for resize-window-mode right from the minibuffer."
  (interactive (list (read-number "Enter a new margin: " rw-margin)))
  (setq rw-margin new-margin)
  (message "%d" rw-margin))

;; Functions for resizing the windows interactively
(defun rw-enlarge-window-vertically()
  (interactive)
  (message "Enlarging window vertically %d times." rw-margin)
  (enlarge-window rw-margin))

(defun rw-shrink-window-vertically()
  (interactive)
  (message "Shrinking window vertically %d times." rw-margin)
  (shrink-window rw-margin))

(defun rw-shrink-window-horizontally()
  (interactive)
  (message "Shrinking window horizontally %d times." rw-margin)
  (shrink-window-horizontally rw-margin))

(defun rw-enlarge-window-horizontally()
  (interactive)
  (message "Enlarging window horizontally %d times." rw-margin)
  (enlarge-window-horizontally rw-margin))

;; The custom keymap used
(defvar resize-window-mode-map
  (let ((map (make-sparse-keymap))
	(current nil))
    ;; Enable default keys if the user allows
    (when rw-enable-default-keys
      ;; Add default vertical enlarge keys
      (add-to-list 'rw-enlarge-vertically-keys "<up>")
      (add-to-list 'rw-enlarge-vertically-keys "C-p")
      ;; Add default vertical shrink keys
      (add-to-list 'rw-shrink-vertically-keys "<down>")
      (add-to-list 'rw-shrink-vertically-keys "C-n")
      ;; Add default horizontal shrink keys
      (add-to-list 'rw-shrink-horizontally-keys "<left>")
      (add-to-list 'rw-shrink-horizontally-keys "C-b")
      ;; Add default horizontal enlarge keys
      (add-to-list 'rw-enlarge-horizontally-keys "<right>")
      (add-to-list 'rw-enlarge-horizontally-keys "C-f")
      ;; Add default exit keys
      (add-to-list 'rw-exit-keys "C-g")
      (add-to-list 'rw-exit-keys "<escape>"))

    ;; Now bind the keys
    ;; Vertical enlarge
    (dolist (current rw-enlarge-vertically-keys)
      (if current
	  (define-key map (kbd current) 'rw-enlarge-window-vertically)))
    ;; Vertical shrink
    (dolist (current rw-shrink-vertically-keys)
      (if current
	  (define-key map (kbd current) 'rw-shrink-window-vertically)))
    ;; Horizontal shrink
    (dolist (current rw-shrink-horizontally-keys)
      (if current
	  (define-key map (kbd current) 'rw-shrink-window-horizontally)))
    ;; Horizontal enlarge
    (dolist (current rw-enlarge-horizontally-keys)
      (if current
	  (define-key map (kbd current) 'rw-enlarge-window-horizontally)))
    ;; Exit
    (dolist (current rw-exit-keys)
      (if current
	  (define-key map (kbd current) 'resize-window-mode)))
    
    map)
  "The resize-window-mode keymap.")

;; The actual mode
(define-minor-mode resize-window-mode
  nil
  nil
  resize-window-mode-map
  "Toggle window resize mode.")

(provide 'resize-window-mode)
;;; resize-window-mode ends here
