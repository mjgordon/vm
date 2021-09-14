(in-package :compiler)

(defparameter *verbose* t)

;; Trees

(defun hash-keys (hash-table)
  "Returns a list of the keys in a hash table"
  (loop for key being the hash-keys of hash-table collect key))

;; Datatypes

(let ((type-values (make-hash-table :test 'eq))
      (reverse-lookup (make-hash-table :test 'eq)))
  (mapcar (lambda (def)
	    (setf (gethash (car def) type-values) (cadr def)))
	  '((int4 4)
	    (int8 8)
	    (int12 12)
	    (int16 16)))
  (mapcar (lambda (key)
	    (setf (gethash (gethash key type-values) reverse-lookup) key))
	  (hash-keys type-values))

  (defun get-datatype-size (type)
    (gethash type type-values))

  (defun add-datatype-sizes (a b)
    (gethash (+ (gethash a type-values)
		(gethash b type-values))
	     reverse-lookup))

  (defun get-type-swap (a b)
    "Returns the appropiate swap mnemonic for the supplied datatypes"
    (format nil "SWAP_~a_~a "
	    (gethash a type-values)
	    (gethash b type-values)))
  
  (defun compare-types (a b)
    "Returns true if type a is larger than type b"
    (> (gethash a type-values)
       (gethash b type-values)))

  
  (defun max-datatype(a b)
    "Returns the maximum bit length of two datatypes"
    (gethash (max (gethash a type-values)
		  (gethash b type-values))
	     reverse-lookup))

  
  (defun order-datatype-sizes (a b)
    (mapcar #'get-datatype-size
	    (if (compare-types a b)
		(list a b)
		(reverse (list a b)))))

  (defun get-cast (a b)
    "Returns the assembly invocation to cast a->b if it exists"
    ;;(format t "~a -> ~a" a b)
    (let ((word-size-a (/ (gethash a type-values) 4))
	  (word-size-b (/ (gethash b type-values) 4)))
      (cond ((= word-size-a word-size-b) "")
	    ((> word-size-a word-size-b)
	     (format nil "~{~a~}" (make-list (- word-size-a word-size-b) :initial-element "DROP1 ")))
	    ((< word-size-a word-size-b)
	     (format nil "~{~a~}" (make-list (- word-size-b word-size-a) :initial-element "RSTK POP LIT PUSH 0 RSTK PUSH ")))))))

(defun intern-datatype (name)
  (intern (string-upcase name)))





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


(defun remove-lines (lines)
  "Remove all comment lines and blank lines from the source file string list"
  (remove-if #'comment-string-p (remove-if #'empty-string-p lines)))


;;; Predicates

(defun empty-string-p (string)
  "Predicate for whether a string is empty"
  (string= string ""))


(defun comment-string-p (string)
  "Predicate for if a string is a comment (begins with '//')"
  (and (>= (length string) 2)
       (string= (subseq string 0 2) "//")))


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
  


