(in-package :compiler)


(defun load-file-as-strings (filename)
  "Load a hxc file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))


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
	    (format t "~%")
	    (print-spaces depth)
	    (format t "~a" (token-type token))
	    (if (eq (type-of (token-value token)) 'cons)
		(print-token-tree (token-value token) (+ 1 depth))
		(format t " (~a)" (token-value token))))
	    
	  tree)
  nil)

(defun print-spaces (amt)
  (format t "~v@{~A~:*~}" amt " "))

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






