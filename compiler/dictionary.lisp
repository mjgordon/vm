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
	      (AND
	       (MEM POP DUP NOR PUSH DUP NOR NOR))
	      (OR
	       (NOR DUP NOR))
	      (NOT
	       (DUP NOR))
	      ;; ADDITION
	      (ADDC
	       (ADD POP RSTK POP ADD PUSH ADD
		ADD POP RSTK PUSH))
	      ;; MULTIPLICATION
	      (MULT
	       (RSTK POP POP LIT PUSH 0 PUSH 0
		%0 RSTK PUSH DUP RSTK PUSH DUP LIT PUSH >1 COND
		LIT PUSH 1 SUB POP RSTK POP POP ADD POP RSTK POP ADD PUSH POP RSTK PUSH GOTO >0
		%1 DROP DROP DROP)
	       local-labels)
	      (MULT21
	       (RSTK POP SWAP RSTK PUSH DUP RSTK POP MULT
		PEEK 2 RSTK POP POP POP DROP RSTK PUSH PUSH PUSH PUSH MULT
		RSTK POP ADD POP
		RSTK POP ADD PUSH
		ADD POP RSTK PUSH PUSH))
	      (MULT22
	       (RSTK POP PEEK 2 PEEK 2 RSTK POP POP MULT21
		RSTK PUSH PUSH PUSH MULT21
		RSTK POP POP SWAP RSTK POP ADDC
		RSTK PUSH PUSH ADDC
		RSTK PUSH))
	      ;; PROGRAM FLOW
	      (GOTO
	       (LIT PUSH)
	       next-token
	       (PC POP))
	      (CALL
	       (LIT PUSH)
	       offset-label
	       23
	       (RSTK POP POP POP POP GOTO))	      
	      (RET
	       (RSTK PUSH PUSH PUSH PUSH PC POP))
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
					 (insert-return-map sym (caddr value))
					 (cons nil (append def (list sym) (cadddr value)))))
	      ((eq type 'local-labels) (let ((l0 (gensym))
					     (l1 (gensym))
					     (l2 (gensym))
					     (r0 (gensym))
					     (r1 (gensym))
					     (r2 (gensym)))
					 (insert-label-set (list l0 l1 l2))
					 (insert-ref-set (list r0 r1 r2))
					 (insert-ref-map r0 l0)
					 (insert-ref-map r1 l1)
					 (insert-ref-map r2 l2)
					 (cons nil (mapcar (lambda (token)
							     (cond ((eq token '%0) l0)
								   ((eq token '%1) l1)
								   ((eq token '%2) l2)
								   ((eq token '>0) r0)
								   ((eq token '>1) r1)
								   ((eq token '>2) r2)
								   (t token)))
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
