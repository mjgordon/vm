(in-package :compiler)


;;; === Parser Rule Syntax===
;; Each rule is defined as a list taking the form of (<name> (expansion))
;; Options are shown as a list within the expansion e.g. (<datatype> ((key-int4 key-int8)))
;; expands the <datatype> token to either key-int4 or key-int8
;; By default each token is expected once. Tokens can be expected 0-infinity times with an &
;; after the token e.g. (<func> (<datatype> identifier open-paren close-paren open-brace <statement> & close-brace))
;; expects an arbitrary number of statements.
;; The repeat symbol can't follow option lists, however the same effect can be implied e.g.
;; (<rule-a> (<repeating-options> &))
;; (<repeating-options> ((sym-a sym-b sym-c...)))

;;; === Criticality ===
;; Whether a failure to parse for a rule or token should cause an error and exit
;; <program> starts with criticality
;; criticality is normally passed down however:
;;
;; (<example-rule> a b & c (d e f) g)
;;
;; a, c, and g have criticality
;; b does not
;; it is critical that one of d, e, or f, succeed, however individually they do not have criticality


(let ((rule-table (make-hash-table)))
  (mapcar (lambda (rule)
	    (let ((rule-name (first rule))
		  (rule-content (second rule)))
	      (setf (gethash rule-name rule-table) rule-content)))
	  
	  '((<program> (<func>))
	    (<func> (<datatype> identifier open-paren close-paren open-brace <statement> & close-brace))
	    (<datatype> ((key-int4 key-int8)))
	    (<statement> (key-return <exp> semicolon))
	    (<exp> (literal-int))))
  (defun get-rule (token-name)
    "Returns the rule expansion list associated with the token name"
    (gethash token-name rule-table)))
  

(defun parse (tokens)
  "Parser entry. Accepts a list of tokens and returns an AST"
  (let ((program-tree '(<program> .())))
    (remove 'syntax (first (parse-subtree program-tree tokens)))))


(defun parse-subtree (tree-level tokens)
  "Parse for a single subtree (usually a rule expansion)
Returns a list of : a list of the tokens matching the subtree, and a list of the remaining tokens.
Any subtree tokens that are not semantic get replaced with the symbol 'syntax for later removal"
  (let ((tree-level-result
	 (loop while tree-level collect
	      (block branch-block
		(let* ((branch (pop tree-level))
		       (is-repeating (eq '& (first tree-level)))
		       (repeating-option (when is-repeating branch))
		       (main-options (if is-repeating
					 (second tree-level)
					 branch))
		       (options (if (listp main-options)
				    main-options
				    (list main-options))))
		  (when is-repeating
		    (if-option repeating-option
			       (setf tree-level (cons branch tree-level))
			       (progn (pop tree-level) (pop tree-level))))
		  (loop for option in options do
		       (if-option option)))))))
    (list tree-level-result tokens)))
	  

(defmacro if-option (option &optional (success-form nil) (failure-form nil))
  "Used within parse-subtree, finds the results of a single call to parse-option, and 
executes one of two forms based on whether the option was valid"
  `(let* ((option-check (parse-option ,option tokens))
	  (option-result (first option-check))
	  (option-tokens (second option-check)))
     (if option-result
	 (progn
	   (setf tokens option-tokens)
	   ,success-form
	   (return-from branch-block (if (token-semantic option-result)
					 option-result
					 'syntax)))
	 ,failure-form)))


(defun parse-option (option tokens)
  "Parses for a single option in the current subtree, recursing if necessary.
Returns a list of: the result and the list of remaining tokens.
Result is nil if the option was not valid."
  (let ((rule (get-rule option)))
    (if rule
	;; If the option is a rule, parse the subtree for its expansion
	(let* ((option-result (parse-subtree rule tokens))
	       (result-subtree (first option-result))
	       (result-tokens (second option-result)))
	  (if (and result-subtree (not (list-all-nil result-subtree)))
	      (list (make-token :type option :value (remove 'syntax result-subtree) :semantic t) result-tokens)
	      (list nil tokens)))
	
	;;Otherwise, parse for a single token
	(let* ((token (first tokens))
	       (type (token-type token)))
	  (if (eq type option)
	      (list token (rest tokens))
	      (list nil tokens))))))
