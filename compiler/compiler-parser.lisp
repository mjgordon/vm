(in-package :compiler)

#| 
=== Terminology ===
- Subtree : A list of tokens at an arbitrary level of the AST. Usually comes from a rule expansion


             +-----+          
             |PARSE|          
             +-----+          
                |             
                v             
         +-------------+      
     +-->|PARSE-SUBTREE|      
     |   +-------------+      
     |     |        |         
     |     v        |         
+----------------+  |         
|PARSE-FOR-BRANCH|  |         
+----------------+  |         
     ^              v         
     |     +-----------------+
     +---- |PARSE-FOR-OPTIONS|
           +-----------------+
                              
                              
                              


=== Structure ===
- Most parser operations return a list of 
a) Some sort of result be it a single successful token or a list of tokens. 
b) The remaining source tokens after that operation. 

|#
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
;; b, being a 'repeat' does not
;; It is critical that one of d, e, or f, succeed, however individually they do not have criticality

;; This needs to change somewhat. While no particular iteration of a repeat 'needs' to complete, once its started,
;; we still need to catch relevant errors inside of it

#|
Instead of the concept of criticality, maybe it should be described as 'structured'
At any point, the system may know exactly what its looking for or not. Cases with a repeat or 
an option list (?) are initially unstructured, however as soon as the system *starts* to complete one of these rules,
they should become structured. e.g. once a return statement is started, the rest of it should be expected regardless
of the fact that as a statement its technically 'repeat optional
Not sure what they may be, but there may be situations where two rule expansions look the same up to a point, so 
you need to check multiple tokens in to set structured-ness? 

Also we need a better word for the 'tag item. Optional? Uncounted?

Also maybe the rules list should be rewritten as a DSL? Seems like the ideal situation for one. 

Can we use (values) and (multiple-value-bind) to get around the constant need for lets and (first/second)ing? 

|#

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
    "Returns the rule expansion list associated with the token name
Returns nil if token-name is not a rule"
    (gethash token-name rule-table)))
  


(defmacro if-branch (test-fun branch tokens criticality &optional (success-form nil) (failure-form nil))
  "Specialized if form. Makes checking for successful branch completions easier"
  (let ((b-result (gensym))
	(b-tokens (gensym)))
    `(multiple-value-bind (,b-result ,b-tokens)
	 (funcall ,test-fun ,branch ,tokens ,criticality)
       (if ,b-result
	   (progn
	     (setf ,tokens ,b-tokens)
	     ,success-form
	     ,b-result)
	   ,failure-form))))


(defun parse-for-branch (branch tokens critical)
  "Check a single branch in a subtree. This branch may either describe a rule, or be a single normal token"
  (let ((rule (get-rule branch)))
    (if rule
	;; If a rule, call parse subtree again
	(multiple-value-bind (rule-results rule-tokens)
	    (parse-subtree rule tokens critical)
	  ;; Check that the results of the rule expansion are not an empty list, a list of all nils, or 'repeat?
	  ;; Wait why would it be just 'repeat? hmmm
	  (if (and rule-results
		   (not (list-all-nil rule-results))
		   (not (eq rule-results 'repeat)))
	      ;; If the rule is completed, add it as a token with its expansion as its value
	      (values (make-token :type branch :value (remove-multiple '(repeat syntax) rule-results))
		      rule-tokens)
	      ;; Else return nil
	      (values nil tokens)))
	;; Else, check the first token in the list against the requested type
	;; The logic for every token eventually arrives here. 
	(if (eq (token-type (first tokens)) branch)
	    ;; If the matching symbol is found, either return it or a syntax maker
	    (values (if (token-semantic (first tokens))
			(first tokens)
			'syntax)
		    (rest tokens))
	    ;; Else return nil and log an error
	    (progn
	      (when critical
		(print branch)
		(log-error (get-error-type branch) (first tokens)))
	      (values nil tokens))))))
	    
(defun parse-for-options (options tokens critical)
  "Check the branches in an options list.
Returns the first options that succeeds. Otherwise returns nil"
  (loop for option in options do
       (multiple-value-bind (option-result option-tokens)
	   (parse-for-branch option tokens nil)
	 (when option-result
	   (return-from parse-for-options (values option-result option-tokens)))))
  ;; If none of the options succeed and the option list is critical, log an error.
  ;; Note the individual options never had criticality"
  (when critical
    (log-error 'error-options-failed options))
  (values nil tokens))


(defun parse-subtree (subtree tokens critical)
  "Parses a list of input tokens (usually from a rule expansion)
These tokens will either be rule name, list of options, or normal token, and may be repeating.
Returns  (result-tokens remaining-source-tokens)"
  (values (loop while subtree collect
	       (let ((branch (pop subtree)))
		 (cond
		   ;; Current branch is a repeat
		   ((eq '& (first subtree))
		    (if-branch #'parse-for-branch branch tokens nil
			       (push branch subtree)
			       (progn
				 (pop subtree)
				 'repeat)))
		   ;; Current branch is an option list
		   ((listp branch)
		    (if-branch #'parse-for-options branch tokens critical))
		   ;; Default - Current branch is normal token or rule name
		   (t
		    (if-branch #'parse-for-branch branch tokens critical
			       nil
			       (return-from parse-subtree (values nil tokens)))))))
	  tokens))
	     

;; TODO: May be able to take out the remove-multiple here? 
(defun parse (tokens)
  "Parser entry. Accepts a list of tokens and returns an AST"
  (when *verbose*
    (format t "~a~%" tokens))
  (let ((program-tree '(<program> .())))
    (remove-multiple '(syntax repeat) (parse-subtree program-tree tokens t))))
