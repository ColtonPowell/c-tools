;;; resize-window-mode.el --- The window resizing component of
;; c-tools.el
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

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
      (define-key map (kbd "<up>") 'rwm-enlarge-window-vertically)
      (define-key map (kbd "C-p") 'rwm-enlarge-window-vertically)
      ;; Add default vertical shrink keys
      (define-key map (kbd "<down>") 'rwm-shrink-window-vertically)
      (define-key map (kbd "C-n") 'rwm-shrink-window-vertically)
      ;; Add default horizontal enlarge keys
      (define-key map (kbd "<right>") 'rwm-enlarge-window-horizontally)
      (define-key map (kbd "C-f") 'rwm-enlarge-window-horizontally)
      ;; Add default horizontal shrink keys
      (define-key map (kbd "<left>") 'rwm-shrink-window-horizontally)
      (define-key map (kbd "C-b") 'rwm-shrink-window-horizontally)
      ;; Add default exit keys
      (define-key map (kbd "<escape>") 'resize-window-mode)
      (define-key map (kbd "C-g") 'resize-window-mode))

    map)
  
  "The resize-window-mode keymap.

Default keys are:
    - \"C-p\" and \"<up>\" (up arrow) to enlarge your current window vertically.
    - \"C-n\" and \"<down>\" to shrink your current window vertically.
    - \"C-b\" and \"<left>\" to shrink your current window horizontally.
    - \"C-f\" and \"<right>\" to enlarge your current window horizontally.
    - \"C-g\" and \"<escape>\" exit resize-window-mode

To disable these keys, (setq rwm-enable-default-keys nil)

To add your own custom keys, evaluate or add this to your .emacs:
    - (define-key resize-window-mode-map (kbd \"KEY_NAME\") 'KEY_ACTION)

Where KEY_NAME might be something like \"C-l\" or \"M-n\" and
KEY_ACTION would be the name of the function you're trying to use like
'resize-window-mode or 'rwm-shrink-window-horizontally.

To change the number of resize actions per keystroke, change
rwm-margin accordingly. Default value is 3. Recommended values
are 1-5.
")

;; The actual mode
(define-minor-mode resize-window-mode
  nil
  nil
  resize-window-mode-map
  "Toggle resize-window mode.")

(provide 'resize-window-mode)
;;; resize-window-mode ends here

;; (define-key resize-window-mode-map (kbd "M-p") 'rwm-enlarge-window-vertically)
