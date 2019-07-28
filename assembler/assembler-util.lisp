(in-package :assembler)

;;; Predicates

(defun empty-string-p (string)
  "Predicate for whether a string is empty"
  (string= string ""))

(defun comment-string-p (string)
  "Predicate for if a string is an assembly comment (begins with '#')"
  (char= (char string 0) #\#))

(defun remove-lines (lines)
  "Remove all comment lines and blank lines from the source file string list"
  (remove-if #'comment-string-p (remove-if #'empty-string-p lines)))


;;; Addresses

(defun convert-address (address)
  "Custom expansion of address references, converts to 16-bit hex value"
  (list (logand (ash address -12) #xF)
	'OPCODES::PUSH
	(logand (ash address -8) #xF)
	'OPCODES::PUSH
	(logand (ash address -4) #xF)
	'OPCODES::PUSH
	(logand address #xF)))


;;; Debugging

(defun print-table (table)
  "Lists keys and values of a hashtable. Returns the size"
  (mapcar (lambda (key)
	    (format t "~a ~a ~%" key (gethash key table)))
	  (hash-keys table))
  (hash-table-count table))


;; Trees

(defun hash-keys (hash-table)
  "Returns a list of the keys in a hash table"
  (loop for key being the hash-keys of hash-table collect key))

(defun get-gensyms (count)
  "Returns a list of the requested number of gensyms"
  (let ((output ()))
    (dotimes (i count output)
      (setf output (cons (gensym) output)))))


(defun pair-tree-create (input)
  "Divides a list into a shallow tree of two-item branches"
  (loop for (a b) on input by #'cddr collect (list a b)))

(defun pair-tree-find (input tree)
  "Returns the shallow pair descriptor of the requested item in a tree"
  (let ((output nil))
    (loop for pair in tree and id from 0 do
	 (cond ((eq input (car pair)) (setf output (list id 0)))
	       ((eq input (cadr pair)) (setf output (list id 1)))))
    output))
  

(defun pair-tree-retrieve (input tree)
  "Returns the specified item from a shallow pair tree"
  (nth (cadr input) (nth (car input) tree)))


(defun get-local-names (count)
  "Creates a shallow pair tree of the requested number of associated local symbol and reference names"
  (loop for i upto (- count 1) collect
       (list (intern (concatenate 'string "%" (write-to-string i)) :opcodes)
	     (intern (concatenate 'string ">" (write-to-string i)) :opcodes))))


;; Token sets and tables

(defun clear-tables ()
  "Sets all symbol lists to their empty state"
  (setf *error-flag* 0)
  (setf *label-set* ())
  (setf *label-table* (make-hash-table :test 'eq))
  (setf *ref-set* ())
  (setf *ref-table* (make-hash-table :test 'eq))
  (setf *return-table* (make-hash-table :test 'eq)))


(defmacro insert-set (set-name)
  "Macro for insertion functions for specific sets"
  `(let ((tokens (if (listp input)
		     input
		     (list input))))
     (mapcar (lambda (token)
	       (setf ,set-name (adjoin token ,set-name)))
	     tokens)))


(defmacro insert-table (table-name)
  "Macro for insertion functions for specific tables"
  `(setf (gethash key ,table-name) def))


(defun insert-label-set (input)
  "Insert the item or list of items into the label set"
  (insert-set *label-set*))


(defun insert-ref-set (input)
  "Insert the item or list of items into the reference set"
  (insert-set *ref-set*))


(defun insert-label-table (key def)
  "Insert the key-value pair into the label table"
  (insert-table *label-table*))


(defun insert-ref-table (key def)
  "Insert the key-value pair into the reference table"
  (insert-table *ref-table*))


(defun insert-return-table (key def)
  "Insert the key-value pair into the call return table"
  (insert-table *return-table*))
  

  
