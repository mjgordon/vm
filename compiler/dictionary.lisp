(defun get-dictionary ()
  "Generates a hashtable of symbols to lists of expanded symbols"
  (let ((bytecodes (make-hash-table :test 'eq)))
    (mapcar #'(lambda (def)
		(setf (gethash (car def) bytecodes) (cadr def)))
	    '((AND
	       (MEM POP DUP NOR PUSH DUP NOR NOR))
	      (OR
	       (NOR DUP NOR))
	      (NOT
	       (DUP NOR))
	      (DUP
	       (PEEK 0))))
    
    bytecodes))

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
