;; Predicates

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

(defun print-table (table)
  (mapcar (lambda (key)
	    (format t "~a ~a ~%" key (gethash key table)))
	  (loop for key being the hash-keys of table collect key)))



;; Working with token sets and tables
(defmacro insert-set (set-name)
  `(let ((tokens (if (listp input)
		     input
		     (list input))))
     (mapcar (lambda (token)
	       (setf ,set-name (adjoin token ,set-name)))
	     tokens)))
  
(defmacro insert-map (map-name)
  `(setf (gethash key ,map-name) def))

(defun insert-label-set (input)
  (insert-set *label-list*))

(defun insert-ref-set (input)
  (insert-set *ref-list*))

(defun insert-label-map (key def)
  (insert-map *label-table*))

(defun insert-ref-map (key def)
  (insert-map *ref-table*))

(defun insert-return-map (key def)
  (insert-map *return-table*))
  

  
