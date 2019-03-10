;;; nav-tools.el --- Navigate through files easier!
;; Copyright (C) 2017 Colton Powell
;; Author: Colton Powell

;;; Code:

(defun next-block-soe(&optional n)
  "Move cursor to start or end (soe) of the next block of text.
Passing optional arg N performs this action N times."
  
  (interactive)
  (let ((orig-column (current-column)))
  (if (not n)
      (setq n 1))
  (while (> n 0)
    (if (current-line-blank-p)
	(next-block-soe-from-ws)
      (next-block-soe-from-text))
    (setq n (- n 1)))
  ;; After reaching the proper line, move to the original position on
  ;; it (or to the point at eol)
  (while (and (< (current-column) orig-column)
	      (< (point) (point-at-eol)))
    (forward-char))))

(defun previous-block-soe(&optional n)
  "Move cursor to start or end (soe) of the next block of text.
Passing optional arg N performs this action N times."
  
  (interactive)
  (let ((orig-column (current-column)))
  (if (not n)
      (setq n 1))
  (while (> n 0)
    (if (current-line-blank-p)
        (previous-block-soe-from-ws)
      (previous-block-soe-from-text))
    (setq n (- n 1)))

  ;; After reaching the proper line, move to the original position on
  ;; it (or to the point at eol)
  (while (and (< (current-column) orig-column)
	      (< (point) (point-at-eol)))
    (forward-char))))

(defun next-block-soe-from-text()
  "Move cursor to start or end (soe) of the next text block.
Should ONLY be used when the cursor is on a line with ONLY text.
This is a helper function for next-block-soe."
  ;; eval (get-num-page-lines) here to avoid repeated, unnecessary evals
  (let ((lines-in-buffer (get-num-page-lines))
	(at-eof '(>= (line-number-at-pos) lines-in-buffer)))

    ;; Cases (must always be (not at-eof)):
    ;; - Next line is blank -> move to it and (next-block-soe-from-ws)
    (if (and (next-line-blank-p) (not (eval at-eof)))
	(progn
	  (forward-line)
	  (next-block-soe-from-ws))
    ;; - Next line is not blank -> Move to last non-ws line that is
    ;; not eof
      (while (and (not (next-line-blank-p)) (not (eval at-eof)))
	(forward-line)))

    ;; Notify user if eof reached.
    (if (eval at-eof)
	(message "Reached end of file."))))

(defun next-block-soe-from-ws()
  "Move cursor to start or end (soe) of the next text block.
Should ONLY be used when the cursor is on a line with ONLY
whitespace and no text.  This is a helper function for
next-block-soe."
  (let ((lines-in-buffer (get-num-page-lines))
	(at-eof '(>= (line-number-at-pos) lines-in-buffer)))
    
    ;; move to next line while current line is blank and not at-eof
    (while (and (current-line-blank-p) (not (eval at-eof)))
      (forward-line))
    
    ;; Notify user when limit reached
    (if (eval at-eof)
	(message "Reached end of file."))))

(defun previous-block-soe-from-text()
  "Move cursor to start or end (soe) of the previous text block.
Should ONLY be used when the cursor is on a line with text.  This
is a helper function for previous-block-soe."
  (let ((at-bof '(= (line-number-at-pos) 1)))
    ;; Cases (must always be (not at-bof)):
    ;; - Previous line is blank -> move to it and (previous-block-soe-from-ws)
    (if (and (previous-line-blank-p) (not (eval at-bof)))
	(progn
	  (forward-line -1)
	  (previous-block-soe-from-ws))
    ;; - Previous line is not blank -> Move to last non-ws line that is
    ;; not bof
      (while (and (not (previous-line-blank-p)) (not (eval at-bof)))
	(forward-line -1)))

    ;; Notify user if bof reached.
    (if (eval at-bof)
	(message "Reached beginning of file."))))

(defun previous-block-soe-from-ws()
  "Move cursor to start or end (soe) of the previous text block.
Should ONLY be used when the cursor is on a line with ONLY
whitespace and no text.  This is a helper function for previous-block-soe."
  (let ((at-bof '(= (line-number-at-pos) 1)))
    ;; move to previous line while current line is blank and not at-eof
    (while (and (current-line-blank-p) (not (eval at-bof)))
      (forward-line -1))
    ;; Notify user when bof reached
    (if (eval at-bof)
	(message "Reached beginning of file."))))

(defun current-line-blank-p()
  "Return t if the current line is whitespace."
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t)))

(defun next-line-blank-p()
  "Return t if the next line is whitespace."
  (save-excursion
    (forward-line)
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t))))

(defun previous-line-blank-p()
  "Return t if the previous line is whitespace."
  (save-excursion
    (forward-line -1)
    (let ((current-line (thing-at-point 'line)))
      (if (eq (string-match-p "^[[:space:]]*$" current-line) 0)
	  t))))

(defun get-num-page-lines ()
  "Return the number of lines on the current page.
A modified version of \"count-lines-page\"."
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

(defun delete-word(arg)
  "Delete ARG words."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; Because I got tired of backward-kill-word cluttering my precious clipboard
(defun backward-delete-word(arg)
  "Delete ARG words backwards."
  (interactive "p")
  (delete-word (- arg)))

(defun path-to-clip()
  "Copy path to file/dir open in the buffer to the clipboard.
Use (setq select-enable-clipboard t) for best results."
  (interactive)
  (kill-new (buffer-file-name)))

(defun back-window()
  "Move backwards a window."
  (interactive)
  (other-window -1))

(provide 'nav-tools)
;;; nav-tools.el ends here
