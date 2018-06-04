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
	      (setf (gethash (car def) definitions) (cadr def)))
	    '((AND
	       ((MEM POP DUP NOR PUSH DUP NOR NOR)  nil))
	      (OR
	       ((NOR DUP NOR)  nil))
	      (NOT
	       ((DUP NOR)  nil)) 
	      (DUP
	       ((PEEK 0)  nil))
	      (RET
	       ((RSTK PUSH PUSH PUSH PUSH PC POP) nil))
	      (CALL
	       ((LIT PUSH +29 RSTK PUSH PUSH PUSH PUSH GOTO)  nil))
	      (DROP
	       ((COLOR PUSH RSTK POP COLOR POP RSTK PUSH COLOR POP) nil))
	      (SWAP
	       ((PEEK 1 RSTK POP POP DROP RSTK PUSH PUSH) nil))
	      (GOTO
	       ((LIT PUSH)  (PC POP)))))
    (lambda (tokens)
      (let* ((token (car tokens))
	     (next (cdr tokens))
	     (value (gethash token definitions))
	     (def (car value))
	     (complex (not (eq (cadr value) nil))))
	(cond ((not def) (cons nil (list token)))
	      (complex (cons t (append def (list next) (cadr value))))
	      (t (cons nil def)))))))


(defun get-bytecodes ()
  "Returns a hashtable of the bytecodes and their associated mnemonics"
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
