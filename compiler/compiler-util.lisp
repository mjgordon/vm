(in-package :compiler)

(defmacro get-hash (key hashtable)
  `(gethash ,key ,hashtable))

(defun print-list (l)
  (mapcar (lambda (item)
	    (format t "~a ~%" item))
	  l))

(defun print-tokens (tokens)
  (mapcar (lambda (token)
	    (format t "~a ~a~%" (token-type token) (token-value token)))
	  tokens))


(defun print-token-tree (tree &optional (depth 0))
  (mapcar (lambda (token)
	    (format t "~v@{~A~:*~}" depth " ")
	    (format t "~a~%" (token-type token))
	    (if (eq (type-of (token-value token)) 'cons)
		(print-token-tree (token-value token) (+ 1 depth))
		(progn
		  (format t "~v@{~A~:*~}" (+ 1 depth) " ")
		  (format t "~a~%" (token-value token)))))
	    
	  tree)
  nil)


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





    
