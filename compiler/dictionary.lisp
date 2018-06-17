(defun expand-pass (tokens expander)
  "Runs a single expansion pass over the tokens"
  (let ((expansion (recursive-expand-pass tokens expander () nil)))
    (cons (car expansion)
	  (apply #'append (reverse (rest expansion))))))

  
(defun recursive-expand-pass (tokens expander result flag)
  "Steps through each the token list with a tail recursion, expanding or skipping as necessary"
  (let* ((token (car tokens))
	 (expansion-result (funcall expander (cons token (cadr tokens))))
	 (complex (car expansion-result))
	 (expansion (cdr expansion-result)))
	 
    (when (not (equal expansion (list token)))
      (setf flag t))
    (when (car expansion)
      (setf result (cons expansion result)))
    (if (rest tokens)
	(recursive-expand-pass (if complex (cddr tokens) (rest tokens)) expander result flag)
	(cons flag result))))
	  
	
  

(defun get-dictionary-expander ()
  "Returns a function to get the expansion of a folded opcode. The hashmap is enclosed"
  (let ((definitions (make-hash-table :test 'eq)))
    (mapcar (lambda (def)
	      (setf (gethash (car def) definitions) (cdr def)))
	    '(
	      ;; STACK OPERATIONS
	      (DROP
	       (LIT POP))
	      (DUP
	       (PEEK 0))
	      (SWAP
	       (PEEK 1 RSTK POP POP DROP RSTK PUSH PUSH))
	      ;; LOGIC
	      (TRUE
	       (LIT PUSH >0 COND
		LIT PUSH 15
		GOTO >1
		%0
		LIT PUSH 0
		%1)
	       local-labels 2)
	      (AND
	       (MEM POP DUP NOR PUSH DUP NOR NOR))
	      (OR
	       (NOR DUP NOR))
	      (NOT
	       (DUP NOR))
	      ;; MATH : ADDITION
	      (ADD21
	       (ADD POP RSTK POP ADD PUSH ADD
		ADD POP RSTK PUSH))

	      ;; MATH : SUBTRACTION
	      (SUB21
	       (SUB POP RSTK POP SUB PUSH SUB POP RSTK PUSH))
	      ;; MATH : MULTIPLICATION
	      (MULT
	       (RSTK POP POP LIT PUSH 0 PUSH 0
		%0 RSTK PUSH DUP RSTK PUSH DUP LIT PUSH >1 COND
		LIT PUSH 1 SUB POP RSTK POP POP ADD POP RSTK POP ADD PUSH POP RSTK PUSH GOTO >0
		%1 DROP DROP DROP)
	       local-labels 2)
	      (MULT21
	       (RSTK POP SWAP RSTK PUSH DUP RSTK POP MULT
		PEEK 2 RSTK POP POP POP DROP RSTK PUSH PUSH PUSH PUSH MULT
		RSTK POP ADD POP
		RSTK POP ADD PUSH
		ADD POP RSTK PUSH PUSH))
	      (MULT22
	       (RSTK POP PEEK 2 PEEK 2 RSTK POP POP MULT21
		RSTK PUSH PUSH PUSH MULT21
		RSTK POP POP SWAP RSTK POP ADD21
		RSTK PUSH PUSH ADD21
		RSTK PUSH))
	      ;; MATH : DIVISION
	      (DIV
	       (DUP LIT PUSH 1 SUB POP
		DROP SUB PUSH TRUE NOT LIT PUSH >1 COND
		DUP RSTK POP POP POP LIT PUSH 0
		RSTK PUSH PUSH
		%0
		SUB POP RSTK POP SUB PUSH TRUE NOT LIT PUSH >2 COND
		LIT PUSH 1 ADD POP RSTK PUSH PUSH DUP RSTK POP GOTO >0
		%1
		DROP DROP LIT PUSH 0 PUSH 0 PUSH 1 GOTO >3
		%2
		RSTK PUSH PUSH DROP DROP LIT PUSH 1 PUSH 0
		%3
		SUB POP DROP)
	       local-labels 4)
	      (DIV21
	       (DUP LIT PUSH 1 SUB POP DROP SUB PUSH TRUE NOT LIT PUSH >1 COND
		DUP RSTK POP POP POP POP LIT PUSH 0 PUSH 0 RSTK PUSH PUSH PUSH
		%0
		SUB21 RSTK POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
		LIT PUSH 1 ADD21 RSTK PUSH PUSH PUSH DUP RSTK POP GOTO >0
		%1
		DROP DROP DROP LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1 GOTO >3
		%2
		RSTK PUSH PUSH PUSH DROP DROP DROP LIT PUSH 1 PUSH 0
		%3
		SUB POP DROP)
	       local-labels 4)
	      ;; PROGRAM FLOW
	      (GOTO
	       (LIT PUSH)
	       next-token
	       (PC POP))
	      (CALL
	       (LIT PUSH)
	       offset-label 23
	       (RSTK POP POP POP POP GOTO))	      
	      (RET
	       (RSTK PUSH PUSH PUSH PUSH PC POP))
	      ;; IO
	      (OUT
	       (LIT PUSH 1 IO POP POP))
	      (OUT8
	       (LIT PUSH 2 IO POP POP POP))
	      (OUT12
	       (LIT PUSH 3 IO POP POP POP POP))
	      (OUT16
	       (LIT PUSH 4 IO POP POP POP POP POP))
	      ))
    (lambda (tokens)
      (let* ((token (car tokens))
	     (next (cdr tokens))
	     (value (gethash token definitions))
	     (def (car value))
	     (type (cadr value)))
	(cond ((not def) (cons nil (list token)))
	      ((eq type 'next-token) (cons t (append def (list next) (caddr value))))
	      ((eq type 'offset-label) (let ((sym (gensym)))
					 (insert-return-table sym (caddr value))
					 (cons nil (append def (list sym) (cadddr value)))))
	      ((eq type 'local-labels) (let* ((local-count (caddr value))
					      (local-symbols (pair-tree-create (get-gensyms (* local-count 2))))
					      (local-names (get-local-names local-count)))
					 (mapcar (lambda (local-symbol)
						   (insert-label-set (car local-symbol))
						   (insert-ref-set (cadr local-symbol))
						   (apply #'insert-ref-table (reverse local-symbol)))						 local-symbols)
					 (cons nil (mapcar (lambda (token)
							     (let ((position (pair-tree-find token local-names)))
							       (if position
								   (pair-tree-retrieve position local-symbols)
								   token)))
							   def))))
	      (t (cons nil def)))))))


(defun get-bytecodes ()
  "Returns a hashtable of the mnemonic symbols to their associated bytecodes"
  (let ((bytecodes (make-hash-table :test 'eq)))
    (mapcar #'(lambda (def)
		(setf (gethash (car def) bytecodes) (cadr def)))
	    '((COLOR #x0)
	      (X #x1)
	      (Y #x2)
	      (PC #x3)
	      (MEM #x4)
	      (IO #x5)
	      (RSTK #x6)
	      (LIT #x7)
	      (ADD #x8)
	      (SUB #x9)
	      (PUSH #xA)
	      (POP #xB)
	      (PEEK #xC)
	      (COND #xD)
	      (NOR #xE)
	      (MOVE #xF)))
    bytecodes))
