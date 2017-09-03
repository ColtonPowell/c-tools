;;;nav-tools.el --- A few things that may help you get around a bit
;;;easier.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; navigate to the end of the current block of text

;; BREAKS ON EOF
;; navigate to the beginning of the next block of text
(defun next-block()
  (interactive)
  ;;(save-excursion
    (while (not (current-line-blank-p))
      (forward-line))
    (while (current-line-blank-p)
      (forward-line))
    ;;(forward-line)
    );;)

;; navigate to the beginning of the previous block of text


;; determine if the current line is blank
(defun current-line-blank-p()
  (catch 'blank-line
    ;; store the current line (under point) in current-line
    (let ((current-line (thing-at-point 'line)))
      ;; if the starting point of the line has all whitespace after it
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  ;; return t
	  (throw 'blank-line t)))))

(provide 'nav-tools)
;;;nav-tools.el ends here.
