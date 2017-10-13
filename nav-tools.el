;;;nav-tools.el --- A few things that may help you get around a bit
;;;easier.
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

;; The actual functions called to move to the next/previous section(s):
(defun next-section(&optional n)
  "Move the cursor to the n'th next section. If n is nil, then
move the cursor to the next section. 

A section is the line at the beginning or end of a block of text:

This is a section
This is NOT a section
This is NOT a section
This is NOT a section
This is a section

This is a section

This is a section
This is a section

This tool allows you to navigate more quickly by traversing entire
blocks of text with a single key."
  
  (interactive)
  (let ((orig-column (current-column)))
  (if (not n)
      (setq n 1))
  (while (> n 0)
    (if (current-line-blank-p)
	(next-section-from-ws)
      (next-section-from-text))
    (setq n (- n 1)))
  ;; After reaching the proper line, move to the original position on
  ;; it (or to the point at eol)
  (while (and (< (current-column) orig-column)
	      (< (point) (point-at-eol)))
    (forward-char))))

(defun previous-section(&optional n)
  "Move the cursor to the n'th previous section. If n is nil, then
move the cursor to the previous section. 

A section is the line at the beginning or end of a block of text:

This is a section
This is NOT a section
This is NOT a section
This is NOT a section
This is a section

This is a section

This is a section
This is a section

This tool allows you to navigate more quickly by traversing entire
blocks of text with a single key."
  
  (interactive)
  ;; Keep the original column number to readjust cursor x coordinates
  ;; after the move
  (let ((orig-column (current-column)))
  (if (not n)
      (setq n 1))
  (while (> n 0)
    (if (current-line-blank-p)
        (previous-section-from-ws)
      (previous-section-from-text))
    (setq n (- n 1)))

  ;; After reaching the proper line, move to the original position on
  ;; it (or to the point at eol)
  (while (and (< (current-column) orig-column)
	      (< (point) (point-at-eol)))
    (forward-char))))

(defun next-section-from-text()
  "Move the cursor to the next section assuming it is placed in a
  line of text."
  ;; eval (get-num-page-lines) here to avoid repeated, unnecessary evals
  (let ((lines-in-buffer (get-num-page-lines))
	(at-eof '(> (line-number-at-pos) lines-in-buffer)))

    ;; Cases (must always be (not at-eof)):
    ;; - Next line is blank -> move to it and (next-section-from-ws)
    (if (and (next-line-blank-p) (not (eval at-eof)))
	(progn
	  (forward-line)
	  (next-section-from-ws))
    ;; - Next line is not blank -> Move to last non-ws line that is
    ;; not eof
      (while (and (not (next-line-blank-p)) (not (eval at-eof)))
	(forward-line)))

    ;; Notify user if eof reached.
    (if (eval at-eof)
	(message "Reached end of file."))))

(defun next-section-from-ws()
  "Move the cursor to the next section assuming it is placed in a
  line of only whitespace."
  (let ((lines-in-buffer (get-num-page-lines))
	(at-eof '(> (line-number-at-pos) lines-in-buffer)))
    
    ;; move to next line while current line is blank and not at-eof
    (while (and (current-line-blank-p) (not (eval at-eof)))
      (forward-line))
    
    ;; Notify user when limit reached
    (if (eval at-eof)
	(message "Reached end of file."))))

(defun previous-section-from-text()
  "Move the cursor to the previous section assuming it is placed in a
  line of text."
  (let ((at-bof '(= (line-number-at-pos) 1)))
    ;; Cases (must always be (not at-bof)):
    ;; - Previous line is blank -> move to it and (previous-section-from-ws)
    (if (and (previous-line-blank-p) (not (eval at-bof)))
	(progn
	  (forward-line -1)
	  (previous-section-from-ws))
    ;; - Previous line is not blank -> Move to last non-ws line that is
    ;; not bof
      (while (and (not (previous-line-blank-p)) (not (eval at-bof)))
	(forward-line -1)))

    ;; Notify user if bof reached.
    (if (eval at-bof)
	(message "Reached beginning of file."))))

(defun previous-section-from-ws()
  "Move the cursor to the previous section assuming it is placed in a
  line of only whitespace."
  (let ((at-bof '(= (line-number-at-pos) 1)))
    ;; move to previous line while current line is blank and not at-eof
    (while (and (current-line-blank-p) (not (eval at-bof)))
      (forward-line -1))
    ;; Notify user when bof reached
    (if (eval at-bof)
	(message "Reached beginning of file."))))

(defun current-line-blank-p()
  "Returns t if the current line is whitespace."
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t)))

(defun next-line-blank-p()
  "Returns t if the next line is whitespace."
  (save-excursion
    (forward-line)
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t))))

(defun previous-line-blank-p()
  "Returns t if the previous line is whitespace."
  (save-excursion
    (forward-line -1)
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t))))

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

(provide 'nav-tools)
;;; nav-tools.el ends here.
