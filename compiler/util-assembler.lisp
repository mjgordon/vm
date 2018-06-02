(defun empty-string-p (string)
  "Predicate for whether a string is empty"
  (string= string ""))

(defun comment-string-p (string)
  "Predicate for if a string is an assembly comment (begins with '#')"
  (char= (char string 0) #\#))

(defun remove-lines (lines)
  "Remove all comment lines and blank lines from the source file string list"
  (remove-if #'comment-string-p (remove-if #'empty-string-p lines)))

(defun convert-address (address)
  "Custom expansion of address references, converts to 16-byte hex value"
  (list (logand (ash address -12) #xF)
	'PUSH
	(logand (ash address -8) #xF)
	'PUSH
	(logand (ash address -4) #xF)
	'PUSH
	(logand address #xF)))
