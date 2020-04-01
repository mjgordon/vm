(in-package :compiler)

(defparameter *verbose* t)

;; IO
(defun load-file-as-strings (filename)
  "Load a hxc file as a list of strings
Inserts trailing spaces at the end of all lines to normalize lexing later. This may prove unwise?"
  (with-open-file (stream filename)
    (let ((lines (loop for line = (read-line stream nil)
		    while line
		    collect (concatenate 'string line " "))))
		    ;;collect line)))
      (remove-lines lines))))

;;; Predicates

(defun empty-string-p (string)
  "Predicate for whether a string is empty"
  (string= string ""))

(defun comment-string-p (string)
  "Predicate for if a string is a comment (begins with '//')"
  (and (>= (length string) 2)
       (string= (subseq string 0 2) "//")))

(defun remove-lines (lines)
  "Remove all comment lines and blank lines from the source file string list"
  (remove-if #'comment-string-p (remove-if #'empty-string-p lines)))
	

;; Printing
(defun print-list (l)
  (mapc (lambda (item)
	  (format t "~a ~%" item))
	l))

(defun print-tokens (tokens)
  (mapc (lambda (token)
	  (format t "~a ~a~%" (token-type token) (token-value token)))
	tokens))


(defun print-token-tree (tree &optional (depth 0) (prefix ""))
  "Print the AST with indents and box characters"
  (loop for (token token-next) on tree do
       (format t "~%")
       (format t "~a" prefix)
       (when (not (zerop depth))
	 (format t "~a" (if token-next
			    #\BOX_DRAWINGS_LIGHT_VERTICAL_AND_RIGHT
			    #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT)))
       (format t "~a" (token-type token))
						    
       (if (eq (type-of (token-value token)) 'cons)
	   (print-token-tree (token-value token)
			     (+ 1 depth)
			     (concatenate 'string prefix (if token-next
							     (string #\BOX_DRAWINGS_LIGHT_VERTICAL)
							     " ")))
	   (format t " (~a)" (token-value token))))
  (when (= depth 0)
    (format t "~%~%")))
	    

(defun print-spaces (amt)
  "Print 'amt' spaces"
  (print-chars " " amt))

(defun print-chars (c amt)
  "Print character 'c' 'amt' number of times"
  (format t "~v@{~A~:*~}" amt c))


;;; Debugging

(defun print-table (table)
  "Lists keys and values of a hashtable. Returns the size"
  (mapcar (lambda (key)
	    (format t "~a ~a ~%" key (gethash key table)))
	  (hash-keys table))
  (hash-table-count table))


;; Lists

(defun ensure-list (var)
  "Puts the variable in a single-item list if its not already a list"
  (if (listp var)
      var
      (list var)))

;; TODO : name this as a predicate
;; TODO : maybe use something like (some (lambda (item) (not (null item))) list) instead? Check speed
(defun list-all-nil (list)
  (loop for e in list never e))


(defun remove-multiple (remove-list input-list)
  (remove-if (lambda (item)
	       (member item remove-list))
	     input-list))


;; Strings

(defmacro pop-string (string)
  "Returns the first character of a string, nil if empty. Removes that character from the string"
  `(if (= 0 (length ,string))
       nil
       (prog1 (schar ,string 0) (setq ,string (subseq ,string 1)))))
  

;; Trees

(defun hash-keys (hash-table)
  "Returns a list of the keys in a hash table"
  (loop for key being the hash-keys of hash-table collect key))
