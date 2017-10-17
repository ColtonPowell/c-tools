;;; resize-window-mode.el --- The window resizing component of
;; c-tools.el
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; Determines the number of resize actions per keypress.
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

;; resize-window-mode's keymap. A blank map by default, customizable
;; by the user or by (rwm-toggle-default-keys)
(defvar resize-window-mode-map (make-sparse-keymap))

;; Determines whether default keys are toggled on/off
(defvar rwm-default-keys-on nil)

;; Toggles the default keys on and off depending on their current state
(defun rwm-toggle-default-keys()
  (interactive)
  ;; If default keys are on, turn them off and vice versa
  (if rwm-default-keys-on
      (progn
	;; Disable default vertical enlarge keys
	(define-key resize-window-mode-map (kbd "<up>") nil)
	(define-key resize-window-mode-map (kbd "C-p") nil)
	;; Disable default vertical shrink keys
	(define-key resize-window-mode-map (kbd "<down>") nil)
	(define-key resize-window-mode-map (kbd "C-n") nil)
	;; Disable default horizontal enlarge keys
	(define-key resize-window-mode-map (kbd "<right>") nil)
	(define-key resize-window-mode-map (kbd "C-f") nil)
	;; Disable default horizontal shrink keys
	(define-key resize-window-mode-map (kbd "<left>") nil)
	(define-key resize-window-mode-map (kbd "C-b") nil)
	;; Disable default exit keys
	(define-key resize-window-mode-map (kbd "<escape>") nil)
	(define-key resize-window-mode-map (kbd "C-g") nil)
	;; Record these changes
	(setq rwm-default-keys-on nil))

    ;; Enable default vertical enlarge keys
    (define-key resize-window-mode-map (kbd "<up>") 'rwm-enlarge-window-vertically)
    (define-key resize-window-mode-map (kbd "C-p") 'rwm-enlarge-window-vertically)
    ;; Enable default vertical shrink keys
    (define-key resize-window-mode-map (kbd "<down>") 'rwm-shrink-window-vertically)
    (define-key resize-window-mode-map (kbd "C-n") 'rwm-shrink-window-vertically)
    ;; Enable default horizontal enlarge keys
    (define-key resize-window-mode-map (kbd "<right>") 'rwm-enlarge-window-horizontally)
    (define-key resize-window-mode-map (kbd "C-f") 'rwm-enlarge-window-horizontally)
    ;; Enable default horizontal shrink keys
    (define-key resize-window-mode-map (kbd "<left>") 'rwm-shrink-window-horizontally)
    (define-key resize-window-mode-map (kbd "C-b") 'rwm-shrink-window-horizontally)
    ;; Enable default exit keys
    (define-key resize-window-mode-map (kbd "<escape>") 'resize-window-mode)
    (define-key resize-window-mode-map (kbd "C-g") 'resize-window-mode)
    ;; Record these changes
    (setq rwm-default-keys-on t))

"Toggles the default resize-window-mode keys. They are off by
default. Toggling the keys off will WIPE resize-window-mode-maps
keybinds for C-p/n/f/b and <up/down/right/left>. Next patch will
instead restore the previous bindings instead of wiping them

Use (rwm-toggle-default-keys) ONCE to enable default keys, shown below:
    - \"C-p\" and \"<up>\" (up arrow) to enlarge your current window vertically.
    - \"C-n\" and \"<down>\" to shrink your current window vertically.
    - \"C-f\" and \"<right>\" to enlarge your current window horizontally.
    - \"C-b\" and \"<left>\" to shrink your current window horizontally.
    - \"C-g\" and \"<escape>\" exit resize-window-mode

Use the 'define-key' function to customize keybinds

Change 'rwm-margin' to change the number of resizes per
keystroke. Default value is 3, recommended values are 1-5."
)

;; The actual mode
(define-minor-mode resize-window-mode
  nil
  nil
  resize-window-mode-map
  "Toggle resize-window mode.")

(provide 'resize-window-mode)
;;; resize-window-mode ends here
