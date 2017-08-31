;;; c-tools.el --- A collection of handy emacs tools that may or may
;; not make you a more productive programmer.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Commentary:
;; Enjoy :)

;;; Code:

;; ======================================================================
;; resize-window-mode
;; ======================================================================
;; Lists containing the strings of the keys to be defined in the
;; resize-window-mode keymap
(defvar rw-enlarge-vertically-keys (list nil))
(defvar rw-shrink-vertically-keys (list nil))
(defvar rw-enlarge-horizontally-keys (list nil))
(defvar rw-shrink-horizontally-keys (list nil))
(defvar rw-exit-keys (list nil))

;; Use to enable/disable the default keys. Set to nil to disable them.
(defvar rw-enable-default-keys t)

;; Multiplies the number of actions per keypress in
;; window-resize-mode. Default is 3.
(defvar rw-multiplier 3)

;; Functions for resizing the windows interactively
(defun rw-enlarge-window-vertically()
  (interactive)
  (message "Enlarging window vertically %d times." rw-multiplier)
  (enlarge-window rw-multiplier))

(defun rw-shrink-window-vertically()
  (interactive)
  (message "Shrinking window vertically %d times." rw-multiplier)
  (shrink-window rw-multiplier))

(defun rw-shrink-window-horizontally()
  (interactive)
  (message "Shrinking window horizontally %d times." rw-multiplier)
  (shrink-window-horizontally rw-multiplier))

(defun rw-enlarge-window-horizontally()
  (interactive)
  (message "Enlarging window horizontally %d times." rw-multiplier)
  (enlarge-window-horizontally rw-multiplier))

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

;; END resize-window-mode
;; ======================================================================

(provide 'c-tools)
;;; c-tools ends here



