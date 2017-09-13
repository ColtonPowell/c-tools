;;; resize-window-mode.el --- The window resizing component of
;; c-tools.el
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; Lists containing the strings of the keys to be defined in the
;; resize-window-mode keymap
(defvar rwm-enlarge-window-vertically-keys (list nil))
(defvar rwm-shrink-window-vertically-keys (list nil))
(defvar rwm-enlarge-window-horizontally-keys (list nil))
(defvar rwm-shrink-window-horizontally-keys (list nil))
(defvar rwm-exit-keys (list nil))

;; Use to enable/disable the default keys. Set to nil to disable them.
(defvar rwm-enable-default-keys t)

;; The value that determines the number of resize actions per
;; keypress. Default is 3.
(defvar rwm-margin 3)

;; Set the default margin from the minibuffer
(defun rwm-set-margin (new-margin)
  "Set a new margin for resize-window-mode."
  (interactive (list (read-number "Enter a new margin: " rwm-margin)))
  (setq rwm-margin new-margin)
  (message "%d" rwm-margin))

;; Functions for resizing the windows interactively
(defun rwm-enlarge-window-vertically()
  "Enlarge the window vertically rwm-margin times."
  (interactive)
  (message "Enlarging window vertically %d times." rwm-margin)
  (enlarge-window rwm-margin))

(defun rwm-shrink-window-vertically()
  "Shrink the window vertically rwm-margin times."
  (interactive)
  (message "Shrinking window vertically %d times." rwm-margin)
  (shrink-window rwm-margin))

(defun rwm-shrink-window-horizontally()
  "Shrink the window horizontally rwm-margin times."
  (interactive)
  (message "Shrinking window horizontally %d times." rwm-margin)
  (shrink-window-horizontally rwm-margin))

(defun rwm-enlarge-window-horizontally()
  "Enlarge the window vertically rwm-margin times."
  (interactive)
  (message "Enlarging window horizontally %d times." rwm-margin)
  (enlarge-window-horizontally rwm-margin))

(defvar resize-window-mode-map
  (let ((map (make-sparse-keymap))
	(current nil))
    ;; Enable default keys if the user allows
    (when rwm-enable-default-keys
      ;; Add default vertical enlarge keys
      (add-to-list 'rwm-enlarge-window-vertically-keys "<up>")
      (add-to-list 'rwm-enlarge-window-vertically-keys "C-p")
      ;; Add default vertical shrink keys
      (add-to-list 'rwm-shrink-window-vertically-keys "<down>")
      (add-to-list 'rwm-shrink-window-vertically-keys "C-n")
      ;; Add default horizontal shrink keys
      (add-to-list 'rwm-shrink-window-horizontally-keys "<left>")
      (add-to-list 'rwm-shrink-window-horizontally-keys "C-b")
      ;; Add default horizontal enlarge keys
      (add-to-list 'rwm-enlarge-window-horizontally-keys "<right>")
      (add-to-list 'rwm-enlarge-window-horizontally-keys "C-f")
      ;; Add default exit keys
      (add-to-list 'rwm-exit-keys "C-g")
      (add-to-list 'rwm-exit-keys "<escape>"))

    ;; Now bind the keys
    ;; Vertical enlarge
    (dolist (current rwm-enlarge-window-vertically-keys)
      (if current
	  (define-key map (kbd current) 'rwm-enlarge-window-vertically)))
    ;; Vertical shrink
    (dolist (current rwm-shrink-window-vertically-keys)
      (if current
	  (define-key map (kbd current) 'rwm-shrink-window-vertically)))
    ;; Horizontal shrink
    (dolist (current rwm-shrink-window-horizontally-keys)
      (if current
	  (define-key map (kbd current) 'rwm-shrink-window-horizontally)))
    ;; Horizontal enlarge
    (dolist (current rwm-enlarge-window-horizontally-keys)
      (if current
	  (define-key map (kbd current) 'rwm-enlarge-window-horizontally)))
    ;; Exit
    (dolist (current rwm-exit-keys)
      (if current
	  (define-key map (kbd current) 'resize-window-mode)))
    
    map)
  "The resize-window-mode keymap.
Default keys are:
    - \"C-p\" and \"<up>\" (up arrow) to enlarge your current window vertically.
    - \"C-n\" and \"<down>\" to shrink your current window vertically.
    - \"C-b\" and \"<left>\" to shrink your current window horizontally.
    - \"C-f\" and \"<right>\" to enlarge your current window horizontally.
    - \"C-g\" and \"<escape>\" exit resize-window-mode

To disable these keys, (setq rwm-enable-default-keys nil)

To add your own custom keys, add each key *as a string* to their respective list in your .emacs file. There are 5:
    - rwm-enlarge-window-vertically-keys
    - rwm-shrink-window-vertically-keys
    - rwm-shrink-window-horizontally-keys
    - rwm-enlarge-window-horizontally-keys
    - rwm-exit-keys

To change the number of resize actions per keystroke, change rwm-margin accordingly. Default value is 3. Recommended values are 1-5.
")

;; The actual mode
(define-minor-mode resize-window-mode
  nil
  nil
  resize-window-mode-map
  "Toggle resize-window mode.")

(provide 'resize-window-mode)
;;; resize-window-mode ends here
