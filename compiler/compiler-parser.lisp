(in-package :compiler)

(defun get-parser-rule-checker ()
  "Returns a list of rules for parsing"
  (let ((rule-table (make-hash-table)))
    (mapcar (lambda (rule)
	      (setf (gethash (first rule) rule-table) (second rule)))
	    
	    '(([program] ([func]))
	      ([func] ([datatype] identifier open-paren close-paren open-brace [statement] close-brace))
	      ([datatype] ((key-int4 key-int8)))
	      ([statement] (key-return [exp] semicolon))
	      ([exp] (literal-int))))
    (lambda (token-name)
      (gethash token-name rule-table))))

(defun parse (tokens)
  (let ((program-tree '([program] .()))
	(get-rule (get-parser-rule-checker)))
    (remove nil (first (parse-r program-tree tokens get-rule)))))

(defun parse-r (tree-level tokens get-rule)
  "Recurse parse function. Returns list [0] new subtree (nil if failed) [1] remaining token list"
  (list
   (mapcar (lambda (branch)
	     (let* ((options (if (listp branch)
				 branch
				 (list branch)))
		    (rules (mapcar get-rule options)))
	       (loop for option in options for rule in rules do
		    (if rule
			;; If the option is a rule, parse its subtree, and pickup where it left off on the token list
			(let* ((option-result (parse-r rule tokens get-rule))
			       (result-subtree (first option-result))
			       (result-tokens (second option-result)))
			  (when result-subtree
			    (setf tokens result-tokens)
			    (return (make-token :type branch :value (remove nil result-subtree)))))
			
			;;Otherwise, check if the next token matches the option
			(let* ((token (first tokens))
			       (type (token-type token)))
			  (when (eq type option)
			    (pop tokens)
			    (return (if (token-semantic token)
					token
					nil))))))))
	   tree-level)
   tokens))
