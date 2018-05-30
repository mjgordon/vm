(defun expand-pass (tokens)
  (let* ((expander (get-dictionary-expander))
	(expansion (recursive-expand-pass tokens expander () nil)))
    (cons (car expansion)
	  (apply #'append (reverse (rest expansion))))))

  
(defun recursive-expand-pass (tokens expander result flag)
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
	       ((PC PUSH LIT PUSH 14 ADD POP RSTK POP LIT PUSH 1 ADD PUSH POP POP RSTK POP POP POP GOTO)  nil))
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
  "Generates a hashtable of the opcodes and their mnemonics"
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
