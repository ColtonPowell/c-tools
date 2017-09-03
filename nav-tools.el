;;;nav-tools.el --- A few things that may help you get around a bit
;;;easier.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; Return the number of lines on the page, taken right out of
;; simple.el with a few mods. Just returns total instead of messaging
;; everything.
(defun get-num-page-lines ()
  "Return the number of lines on current page. A modified version of
count-lines-page."
  (save-excursion
    (let ((opoint (point)) beg end
	  total)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end))
      total)))

(defun skip-block())
(defun skip-ws())
(defun next-section())
(defun previous-section()) 




;; navigate to the beginning of the next block of text
(defun next-block()
  "Navigate to the beginning of the next block of text."
  (interactive)
  ;; Uses lines-in-buffer to avoid continually calling
  ;; get-num-page-lines. Needs at-eof to check when eof reached.
  (let* ((lines-in-buffer (get-num-page-lines))
	(at-eof '(> (line-number-at-pos) lines-in-buffer)))
    ;; While cur line not blank and not EOF
    (while (and (not (current-line-blank-p)) (not (eval at-eof)))
      (forward-line))
    ;; While cur line blank and not EOF
    (while (and (current-line-blank-p) (not (eval at-eof)))
      (forward-line))
    ;; Notify user of error at EOF
    (if (eval at-eof)
    (message "End of file reached."))))

;; navigate to the end of the previous block of text
(defun previous-block()
  "Navigate to the end of the previous block of text."
  (interactive)
  ;; Need to check when beginning of file (bof) reached
  (let* ((at-bof '(= (line-number-at-pos) 0)))
    ;; Move through the current block
    (while (and (not (current-line-blank-p)) (not (eval at-bof)))
      (forward-line -1))
    ;; While cur line blank and not BOF
    (while (and (current-line-blank-p) (not (eval at-bof)))
      (forward-line -1))
    ;; Notify user of error at BOF
    (if (eval at-bof)
    (message "Beginning of file reached."))))

;; determine if the current line is blank
(defun is-current-line-blank-p()
  (catch 'blank-line
    ;; store the current line (under point) in current-line
    (let ((current-line (thing-at-point 'line)))
      ;; if the starting point of the line has all whitespace after it
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  ;; return t
	  (throw 'blank-line t)))))

(provide 'nav-tools)
;;; nav-tools.el ends here.
