(in-package :compiler)

(defun get-parser-rule-checker ()
  "Returns a list of rules for parsing"
  (let ((rule-table (make-hash-table)))
    (mapcar (lambda (rule)
	      (setf (gethash (first rule) rule-table) (second rule)))
	    
	    '(([program] ([func]))
	      ([func] (key-int4 identifier open-paren close-paren open-brace [statement] close-brace))
	      ([statement] (key-return [exp] semicolon))
	      ([exp] (literal-int))))
    (lambda (token-name)
      (gethash token-name rule-table))))

(defun parse (tokens)
  "Parse program tokens into an AST"
  (let ((program-tree '([program] . ()))
	(get-rule (get-parser-rule-checker))
	(get-token (lambda () (pop tokens))))
    (remove nil (parse-r program-tree get-token get-rule))))

(defun parse-r (tree get-token get-rule)
  "Recurvise part of the parse function"
  (mapcar (lambda (branch)
	    (let ((token-rule (funcall get-rule branch)))
	      "If the token is an expandable rule, recurse the function"
	      "Otherwise, check if it fits in the current rule"
	      (if token-rule
		  (make-token :type branch :value (remove nil (parse-r token-rule get-token get-rule)))
		  (let* ((token (funcall get-token))
			 (type (token-type token)))
		    (if (eq type branch)
			(if (token-semantic token)
			    token
			    nil)
			(format t "Bad token : ~a | Expected : ~a ~%" type branch))))))
	  tree))

