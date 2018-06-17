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
	  (loop for key being the hash-keys of table collect key))
  (hash-table-count table))

(defun get-gensyms (count)
  (let ((output ()))
    (dotimes (i count output)
      (setf output (cons (gensym) output)))))

;; Tree functions

(defun pair-tree-create (input)
  (loop for (a b) on input by #'cddr collect (list a b)))

(defun pair-tree-find (input tree)
  (let ((output nil))
    (loop for pair in tree and id from 0 do
	 (cond ((eq input (car pair)) (setf output (list id 0)))
	       ((eq input (cadr pair)) (setf output (list id 1)))))
    output))
  

(defun pair-tree-retrieve (input tree)
  (nth (cadr input) (nth (car input) tree)))

(defun get-local-names (count)
  (loop for i upto (- count 1) collect
       (list (intern (concatenate 'string "%" (write-to-string i)))
	     (intern (concatenate 'string ">" (write-to-string i))))))

;; Working with token sets and tables
(defun clear-tables ()
  (setf *label-set* ())
  (setf *label-table* (make-hash-table :test 'eq))
  (setf *ref-set* ())
  (setf *ref-table* (make-hash-table :test 'eq))
  (setf *return-table* (make-hash-table :test 'eq)))

(defmacro insert-set (set-name)
  `(let ((tokens (if (listp input)
		     input
		     (list input))))
     (mapcar (lambda (token)
	       (setf ,set-name (adjoin token ,set-name)))
	     tokens)))
  
(defmacro insert-table (table-name)
  `(setf (gethash key ,table-name) def))

(defun insert-label-set (input)
  (insert-set *label-set*))

(defun insert-ref-set (input)
  (insert-set *ref-set*))

(defun insert-label-table (key def)
  (insert-table *label-table*))

(defun insert-ref-table (key def)
  (insert-table *ref-table*))

(defun insert-return-table (key def)
  (insert-table *return-table*))
  

  
