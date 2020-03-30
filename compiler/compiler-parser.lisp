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
	    (<datatype> ((key-int4
			  key-int8)))
	    (<statement> (key-return <exp> semicolon))
	    (<exp> ((<unop-exp>
		     literal-int)))
	    (<unop-exp> (unop-negation <exp>))))
  (defun get-rule (token-name)
    "Returns the rule expansion list associated with the token name"
    (gethash token-name rule-table)))
  


(defmacro if-branch (test-fun branch tokens &optional (success-form nil) (failure-form nil))
  `(let* ((b-check (funcall ,test-fun ,branch ,tokens))
	  (b-result (first b-check))
	  (b-tokens (second b-check)))
     (if b-result
	 (progn
	   (setf ,tokens b-tokens)
	   ,success-form
	   b-result)
	 ,failure-form)))

(defun parse-for-branch (branch tokens)
  (let ((rule (get-rule branch)))
    (if rule
	;; If a rule, call parse subtree again
	(let ((rule-subtree (parse-subtree-new rule tokens)))
	  (if (and (first rule-subtree)
		   (not (list-all-nil (first rule-subtree)))
		   (not (eq (first rule-subtree) 'repeat)))
	      (list (make-token :type branch :value (remove-multiple '(repeat syntax) (first rule-subtree)))
		    (second rule-subtree))
	      (list nil tokens)))
	;; Else, check symbol against current token type
	(if (eq (token-type (first tokens)) branch)
	    (list (if (token-semantic (first tokens))
		      (first tokens)
		      'syntax)
		  (rest tokens))
	    (list nil tokens)))))
	    
(defun parse-for-options (options tokens)
  (loop for option in options do
       (let ((parse-result (parse-for-branch option tokens)))
	 (when (first parse-result)
	   (return-from parse-for-options parse-result))))
  (list nil tokens))


(defun parse-subtree-new (input-tree tokens)
  "Parses a list of input tokens (likely from a rule expansion
These tokens will either be a repeating token, a list of token options, or a normal token.
Returns  ( list-of-resulting-tokens list-of-remaining-source-tokens"
  
  (list (loop while input-tree collect
	     (let ((branch (pop input-tree)))
	       (cond
		 ;; Current branch is a repeat
		 ((eq '& (first input-tree))
		  (if-branch #'parse-for-branch branch tokens
			     (push branch input-tree)
			     (progn
			       (pop input-tree)
			       'repeat)))
		 ;; Current branch is an option list
		 ((listp branch)
		  (if-branch #'parse-for-options branch tokens))
		 ;; Default - Current branch is normal symbol
		 (t
		  (if-branch #'parse-for-branch branch tokens
			     nil
			     (return-from parse-subtree-new (list nil tokens)))))))
	tokens))
	     







	 
(defun parse (tokens)
  "Parser entry. Accepts a list of tokens and returns an AST"
  (let ((program-tree '(<program> .())))
    (remove-multiple '(syntax repeat) (first (parse-subtree-new program-tree tokens)))))
