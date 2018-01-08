;;; window-manage-mode.el --- Manage and navigate through windows easily
;; Copyright (C) 2017 Colton Powell

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Colton Powell
;; Package-Requires: ((ace-window "0.9.0"))

;;; Code:
(require 'ace-window)

;; Determines the number of resize actions per keypress.
(defcustom wmm-multiplier 3
  "Multiplies the number of actions for the `window-manage-mode' resize
functions. Default value is 3. Recommended values are 1-5."
  :type 'integer)

;; Set the default multiplier from the minibuffer
(defun wmm-set-multiplier (new-multiplier)
  "Set a new multiplier for window-manage-mode."
  (interactive (list (read-number "Enter a new multiplier: " wmm-multiplier)))
  (setq wmm-multiplier new-multiplier)
  (message "%d" wmm-multiplier))

;; Functions for resizing the windows interactively
(defun wmm-enlarge-window-vertically()
  "Enlarge the window vertically `wmm-multiplier' times."
  (interactive)
  (message "Enlarging window vertically %d times." wmm-multiplier)
  (enlarge-window wmm-multiplier))

(defun wmm-shrink-window-vertically()
  "Shrink the window vertically `wmm-multiplier' times."
  (interactive)
  (message "Shrinking window vertically %d times." wmm-multiplier)
  (shrink-window wmm-multiplier))

(defun wmm-shrink-window-horizontally()
  "Shrink the window horizontally `wmm-multiplier' times."
  (interactive)
  (message "Shrinking window horizontally %d times." wmm-multiplier)
  (shrink-window-horizontally wmm-multiplier))

(defun wmm-enlarge-window-horizontally()
  "Enlarge the window vertically `wmm-multiplier' times."
  (interactive)
  (message "Enlarging window horizontally %d times." wmm-multiplier)
  (enlarge-window-horizontally wmm-multiplier))

;; Determines whether default keys are toggled on/off
(defcustom wmm-default-keys-on t
  "This variable tracks the state of the default keybindings, and
is used in `wmm-toggle-default-keys'. DO NOT modify the value of
this variable."
  :type 'boolean)

;; window-manage-mode's keymap. A blank map by default, customizable
;; by the user or by (wmm-toggle-default-keys)
(defcustom window-manage-mode-map
  (if wmm-default-keys-on
  (let ((map (make-sparse-keymap)))
    ;; ========== Enable Window Sizing Keys ==========
    ;; Enable default vertical enlarge keys
    (define-key map (kbd "<M-up>") 'wmm-enlarge-window-vertically)
    (define-key map (kbd "M-p") 'wmm-enlarge-window-vertically)

    ;; Enable default vertical shrink keys
    (define-key map (kbd "<M-down>") 'wmm-shrink-window-vertically)
    (define-key map (kbd "M-n") 'wmm-shrink-window-vertically)

    ;; Enable default horizontal enlarge keys
    (define-key map (kbd "<M-right>") 'wmm-enlarge-window-horizontally)
    (define-key map (kbd "M-f") 'wmm-enlarge-window-horizontally)

    ;; Enable default horizontal shrink keys
    (define-key map (kbd "<M-left>") 'wmm-shrink-window-horizontally)
    (define-key map (kbd "M-b") 'wmm-shrink-window-horizontally)

    ;; ========== Enable Window Switching Keys ==========
    ;; Enable default up window switch keys
    (define-key map (kbd "<C-up>") 'windmove-up)
    (define-key map (kbd "C-p") 'windmove-up)
    
    ;; Enable default down window switch keys
    (define-key map (kbd "<C-down>") 'windmove-down)
    (define-key map (kbd "C-n") 'windmove-down)
    
    ;; Enable default left window switch keys
    (define-key map (kbd "<C-right>") 'windmove-right)
    (define-key map (kbd "C-f") 'windmove-right)
    
    ;; Enable default right window switch keys
    (define-key map (kbd "<C-left>") 'windmove-left)
    (define-key map (kbd "C-b") 'windmove-left)

    ;; Enable easier window swap key
    (define-key map (kbd "o") 'other-window)

    ;; Delete current window
    (define-key map (kbd "0") 'delete-window)
 
    ;; ========== Enable ace-window keys ==========
    (define-key map (kbd "a") 'ace-window)
    (define-key map (kbd "s") 'ace-select-window)
    (define-key map (kbd "d") 'ace-delete-window)
    (define-key map (kbd "f") 'ace-swap-window)

    ;; ========== Enable Exit Keys ==========
    (define-key map (kbd "C-g") 'window-manage-mode)
    (define-key map (kbd "<escape>") 'window-manage-mode)
    
    map)
  (make-sparse-keymap))
  "The `window-manage-mode' keymap. Set wmm-require-ace-window to t to
  enable ace-window keybinds. Use (describe-function
  'window-manage-mode) to see keybinds.")

(defun window-manage-mode-on()
  (unless (minibufferp)
    (window-manage-mode))
  "Function used by `global-window-manage-mode' to activate
  `window-manage-mode' in every buffer.")

(define-minor-mode window-manage-mode
  nil
  nil
  window-manage-mode-map
  "DO NOT USE! Use `global-window-manage-mode' instead")

;; Could use autoload magic comment here?
(define-global-minor-mode global-window-manage-mode
  window-manage-mode
  window-manage-mode-on
  "Activates `window-manage-mode' in all buffers.")

(provide 'window-manage-mode)
;;; window-manage-mode ends here
