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
(defun get-parser-rule-checker ()
  "Returns a list of rules for parsing"
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
    (lambda (token-name)
      (gethash token-name rule-table))))

(defun parse (tokens)
  "Parser entry. Accepts a list of tokens and returns an AST"
  ;;(format t "Tokens : ~a~%" tokens)
  (let ((program-tree '(<program> .()))
	(get-rule (get-parser-rule-checker)))
    (remove 'syntax (first (parse-r program-tree tokens get-rule)))))


;; TODO : We can add repeating options by using '(loop while branches' etc.
;; At start of loop, check the next branch symbol. If its a &, the current branch is considered the special
;; repeating option, and the normal options are decided by the next branch after that.
;; E. G.
;; (<program> (<decl> & <func> & EOF)
;; The program can contain any number of variable declarations and function definitions.
;; For now you can't repeat a normal option, we'll see if there's ever a situation where you'd need to
;; Checking an option needs to be moved to a function, so that you can differently call it for the special 
;; repeating option vs the normal options. Also help clean up the big old parse-r function
(defun parse-r (tree-level tokens get-rule)
  ;(break)
  (list
   (loop while tree-level collect
	(block branch-block
	  (let* ((branch (pop tree-level))
		 (is-repeating (eq '& (first tree-level)))
		 (repeating-option (when is-repeating branch))
		 (repeating-rule (when is-repeating (funcall get-rule repeating-option)))
		 (main-options (if is-repeating
				   (second tree-level)
				   branch))
		 (options (if (listp main-options)
			      main-options
			      (list main-options)))
		 (rules (mapcar get-rule options)))
	    (when is-repeating
	      (format t "Repeating : ~a~%" repeating-option)
	      (let ((option-check (parse-check-option repeating-option repeating-rule tokens get-rule)))
		(if (option-result option-check)
		    (progn
		      (setf tokens (option-tokens option-check))
		      (setf tree-level (cons branch tree-level))
		      (format t "Option Result : ~a~%" (option-result option-check))
		      (format t "After replacing : ~a~%" tree-level)
		      (return-from branch-block (if (token-semantic (option-result option-check))
						    (option-result option-check)
						    'syntax)))
		    (progn
		      (pop tree-level)
		      (pop tree-level)
		      (format t "After popping : ~a~%" tree-level)))))
	    (loop
	       for option in options
	       for rule in rules
	       do
		 (let ((option-check (parse-check-option option rule tokens get-rule)))
		   (when (option-result option-check)
		     (setf tokens (option-tokens option-check))
		     (return-from branch-block (if (token-semantic (option-result option-check))
						    (option-result option-check)
						    'syntax))))))))
      tokens))
		      
			     

;; Returns list [0] the result (nil if failed) [1] remaining token list
(defun parse-check-option (option rule tokens get-rule)
  "Checks a single option at the current tree level, recursing if necessary"
  (if rule
      ;; If the option is a rule, parse its subtree, and pickup where it left off on the token list
      (let* ((option-result (parse-r rule tokens get-rule))
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
  



