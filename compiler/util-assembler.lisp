(defun empty-string-p (string)
  (string= string ""))

(defun comment-string-p (string)
  (char= (char string 0) #\#))

(defun remove-lines (lines)
  "Remove all comment lines and blank lines from the source file string list"
  (remove-if #'comment-string-p (remove-if #'empty-string-p lines)))
