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
    (gethash token-name rule-table)))



  

(defun parse (tokens)
  "Parser entry. Accepts a list of tokens and returns an AST"
  (let ((program-tree '(<program> .())))
    (remove 'syntax (first (parse-r program-tree tokens)))))

(defun parse-r (tree-level tokens)
  "Parse a single tree-level (i.e. rule expansion)"
  (list
   (loop while tree-level collect
	(block branch-block
	  (let* ((branch (pop tree-level))
		 (is-repeating (eq '& (first tree-level)))
		 (repeating-option (when is-repeating branch))
		 (repeating-rule (when is-repeating (get-rule repeating-option)))
		 (main-options (if is-repeating
				   (second tree-level)
				   branch))
		 (options (if (listp main-options)
			      main-options
			      (list main-options)))
		 (rules (mapcar #'get-rule options)))
	    (when is-repeating
	      (let ((option-check (parse-check-option repeating-option repeating-rule tokens)))
		(if (option-result option-check)
		    (progn
		      (setf tokens (option-tokens option-check))
		      (setf tree-level (cons branch tree-level))
		      (return-from branch-block (if (token-semantic (option-result option-check))
						    (option-result option-check)
						    'syntax)))
		    (progn
		      (pop tree-level)
		      (pop tree-level)))))
	    (loop
	       for option in options
	       for rule in rules
	       do
		 (let ((option-check (parse-check-option option rule tokens)))
		   (when (option-result option-check)
		     (setf tokens (option-tokens option-check))
		     (return-from branch-block (if (token-semantic (option-result option-check))
						    (option-result option-check)
						    'syntax))))))))
      tokens))
		      
			     

;; Returns list [0] the result (nil if failed) [1] remaining token list
(defun parse-check-option (option rule tokens)
  "Checks a single option at the current tree level, recursing if necessary"
  (if rule
      ;; If the option is a rule, parse its subtree, and pickup where it left off on the token list
      (let* ((option-result (parse-r rule tokens))
	     (result-subtree (first option-result))
	     (result-tokens (second option-result)))
	(if (and result-subtree (not (list-all-nil result-subtree)))
	    (list (make-token :type option :value (remove 'syntax result-subtree) :semantic t) result-tokens)
	    (list nil tokens)))
      
      ;;Otherwise, check if the next token matches the option
      (let* ((token (first tokens))
	     (type (token-type token)))
	(if (eq type option)
	    (list token (rest tokens))
	    (list nil tokens)))))
  



