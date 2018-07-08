(defun expand-pass (tokens expander)
  "Runs a single expansion pass over a token list"
  (let* ((expansion (recursive-expand-pass tokens *output-map* expander () () nil))
	 (expansion-flag (first expansion))
	 (expansion-content (second expansion))
	 (output-map-new (third expansion)))

    (setf *output-map* (reverse output-map-new))
    (cons expansion-flag
	  (apply #'append (reverse expansion-content)))))

  
(defun recursive-expand-pass (tokens map-input expander result map-result flag)
  "Steps through the token list with tail recursion, expanding or skipping as necessary"
  (let* ((token (first tokens))
	 (next (second tokens))
	 (expansion-result (funcall expander (cons token next)))
	 (takes-ref (car expansion-result))
	 (expansion (cdr expansion-result))
	 (map-item (first map-input)))
    ;; Mark that an expansion has occurred this pass
    (unless (equal expansion (list token))
      (setf flag t))

    ;; Add the expansion to the output list
    (when (car expansion)
      (setf result (cons expansion result)))

    (setf map-result (append (make-list (length expansion) :initial-element map-item) map-result))    

    ;; Call the function again if there are more tokens to process, else prepend the expansion flag and return
    (if (rest tokens)
	(recursive-expand-pass (if takes-ref (cddr tokens) (rest tokens))
			       (if takes-ref (cddr map-input) (rest map-input))
			       expander
			       result
			       map-result
			       flag)
	(list flag result map-result))))
	

(defun get-dictionary-expander ()
  "Returns a function to get the expansion of a folded opcode. The dictionary hashmap is closured"
  (let ((definitions (make-hash-table :test 'eq)))
    (mapcar (lambda (def)
	      (setf (gethash (car def) definitions) (cdr def)))
	    (get-dictionary))
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
						   (apply #'insert-ref-table (reverse local-symbol)))
						 local-symbols)
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



(defun get-dictionary ()
  "Returns the raw data of the expansion dictionary. Gets closured into the dictionary expander function"
  '(
    ;; STACK OPERATIONS
    (DROP
     (LIT POP))
    (DUP
     (PEEK 0))
    (SWAP
     (PEEK 1 RSTK POP POP DROP RSTK PUSH PUSH))
    (CONST3_0
     (LIT PUSH 0 PUSH 0 PUSH 0))
    (CONST4_0
     (LIT PUSH 0 PUSH 0 PUSH 0 PUSH 0))
    (CONST3_1
     (LIT PUSH 0 PUSH 0 PUSH 1))
    (CONST4_1
     (LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1))
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
    (ZERO2
     (ADD POP PUSH POP))
    ;; MATH : COMPARISON
    (EQUAL3
     (PEEK 3 SUB POP TRUE NOT LIT PUSH >0 COND
      PEEK 3 SUB POP TRUE NOT LIT PUSH >1 COND
      PEEK 3 SUB POP TRUE NOT LIT PUSH >2 COND
      DROP DROP DROP LIT PUSH 15 GOTO >3
      %0 DROP %1 DROP %2 DROP DROP DROP LIT PUSH 0
      %3)
     local-labels 4)
    ;; MATH : ADDITION
    (ADDU11
     (ADD POP RSTK POP ADD PUSH))
    (ADD21
     (ADDU11
      ADD POP RSTK PUSH))
    (ADD22
     (SWAP RSTK POP ADD21 RSTK PUSH SWAP RSTK POP ADD POP RSTK PUSH))
    (ADD31
     (ADDU11 ADDU11
      ADD POP RSTK PUSH PUSH))
    (ADDC3_1
     (LIT PUSH 1 ADD31))
    (ADDC4_1
     (LIT PUSH 1 ADDU11 ADDU11 ADDU11
      ADD POP RSTK PUSH PUSH PUSH))

    ;; MATH : SUBTRACTION
    (SUB21
     (SUB POP RSTK POP SUB PUSH SUB POP RSTK PUSH))
    (SUB22
     (SWAP RSTK POP SUB21 RSTK PUSH SWAP SUB PUSH RSTK POP POP SUB POP RSTK PUSH LIT PUSH 0
      SUB PUSH RSTK PUSH ADD POP SUB POP DROP))
    (SUB31
     (SUB POP RSTK POP SUB PUSH
      SUB POP RSTK POP SUB PUSH
      SUB POP RSTK PUSH PUSH))
    (SUB33
     (PEEK 2 RSTK POP POP POP DROP RSTK PUSH PUSH SUB22
      RSTK PUSH PEEK 2 PEEK 2 RSTK POP POP POP DROP DROP SUB PUSH POP RSTK PUSH SUB POP RSTK PUSH PUSH))
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
     (DUP LIT PUSH >1 COND
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
     (DUP LIT PUSH >1 COND
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
    (DIV22
     (PEEK 1 PEEK 1 ZERO2 LIT PUSH >1 COND
      PEEK 1 PEEK 1 RSTK POP POP POP POP POP POP LIT PUSH 0 PUSH 0 RSTK PUSH PUSH PUSH PUSH
      %0
      SUB22 RSTK POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      LIT PUSH 1 ADD21 RSTK PUSH PUSH PUSH PUSH PEEK 1 PEEK 1 RSTK POP POP GOTO >0
      %1
      DROP DROP DROP DROP
      LIT PUSH 0 PUSH 0 PUSH 0 PUSH 1 GOTO >3
      %2
      RSTK PUSH PUSH PUSH PUSH DROP DROP DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    (DIV31
     (DUP LIT PUSH >1 COND
      DUP RSTK POP POP POP POP POP CONST3_0 RSTK PUSH PUSH PUSH PUSH
      %0
      SUB31 RSTK POP POP POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      ADDC3_1 RSTK PUSH PUSH PUSH PUSH DUP RSTK POP GOTO >0
      %1
      DROP DROP DROP DROP LIT PUSH 0 CONST4_1 GOTO >3
      %2
      RSTK PUSH PUSH PUSH PUSH DROP DROP DROP DROP LIT PUSH 1 PUSH 0
      %3
      SUB POP DROP)
     local-labels 4)
    ;; MATH : MODULO
    (MOD
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB POP SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD POP)
     local-labels 3)

    (MOD21
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB21 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD21)
     local-labels 3)
    
    (MOD22
     (PEEK 1 PEEK 1 ZERO2 LIT PUSH >1 COND
      PEEK 1 PEEK 1 RSTK POP POP
      %0
      SUB22 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH PUSH PEEK 1 PEEK 1 RSTK POP POP GOTO >0
      %1 RSTK POP POP
      %2 RSTK PUSH PUSH ADD22)
     local-labels 3)
    (MOD31
     (DUP LIT PUSH >1 COND
      DUP RSTK POP
      %0
      SUB31 SUB PUSH TRUE NOT LIT PUSH >2 COND
      RSTK PUSH DUP RSTK POP GOTO >0
      %1 RSTK POP
      %2 RSTK PUSH ADD31)
     local-labels 3)
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