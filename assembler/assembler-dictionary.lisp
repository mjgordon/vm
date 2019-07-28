(in-package :assembler)

(defun expand-flag (flags)
  (first flags))

(defun (setf expand-flag) (value flags)
  (setf (first flags) value))

(defun ref-flag (flags)
  (second flags))

(defun (setf ref-flag) (value flags)
  (setf (second flags) value))


(defun expand-pass (tokens)
  "Runs a single expansion pass over a token list
Returns a cons of an expansion flag, and a new list which may or may not still contain expansions"
  (let* ((expansion (recursive-expand-pass tokens () nil))	 
	 (expansion-flag (car expansion))
	 (expansion-content (cdr expansion)))
    (cons expansion-flag
	  (apply #'append (reverse expansion-content)))))


  
(defun recursive-expand-pass (tokens result pass-flag)
  "Steps through the token list with tail recursion, expanding or skipping as necessary"
  (let* ((token (first tokens))
	 (token-next (second tokens))
	 (expansion-result (dictionary-expand token token-next))
	 (flags (car expansion-result))
	 (expansion (cdr expansion-result)))
    
    ;; Mark that an expansion has occurred this pass
    (when (expand-flag flags)
      (setf pass-flag t))

    ;; Add either the expansion or (token) to the result
    (setf result (cons expansion result))

    ;; Call the function again if there are more tokens to process, else return the result and flag
    (if (rest tokens)
	(recursive-expand-pass (if (ref-flag flags) (cddr tokens) (rest tokens))
			       result
			       pass-flag)
	(cons pass-flag result))))
	

(let ((definitions (make-hash-table :test 'eq)))
  (loop for def in (opcodes:get-dictionary) do
       (setf (gethash (first def) definitions) (rest def)))
  (defun dictionary-expand (token token-next)
    "Returns a list of resulting token values, given the current token and the next token
Currently has hella side effects. Might want to put those elsewhere"
    (let* ((value (gethash (token-value token) definitions))
	   (def (first value))
	   (type (second value))
	   (flags (list nil nil))
	   (result (list token))
	   (source-id (token-source-id token)))
      
      (when def
	(setf result (make-tokens def source-id))
	(setf (expand-flag flags) t))

      (cond	
	;; Insert the next token into the definition
	((eq type 'opcodes::next-token)
	 (progn
	   (setf (ref-flag flags) t)
	   (setf result (append (make-tokens def source-id)
				(list token-next)
				(make-tokens (third value) source-id)))))
	
	;; Sets offset label with its initial offset value in the return table
	((eq type 'opcodes::offset-label)
	 (let ((sym (gensym)))
	   (insert-return-table sym (third value))
	   (setf result (append (make-tokens def source-id)
				(make-tokens (list sym) source-id)
				(make-tokens (fourth value) source-id)))))
	;; Sets up local labels if applicable
	((eq type 'opcodes::local-labels)
	 (let* ((local-count (third value))
		(local-symbols (pair-tree-create (get-gensyms (* local-count 2))))
		(local-names (get-local-names local-count)))
	   (mapcar (lambda (local-symbol)
		     (insert-ref-set (first local-symbol))
		     (insert-label-set (second local-symbol))
		     (apply #'insert-ref-table  local-symbol))
		   local-symbols)
	   (setf result (make-tokens (mapcar (lambda (val)
					       (let ((position (pair-tree-find val local-names)))
						 (if position
						     (pair-tree-retrieve position local-symbols)
						     val)))
					     def)
				     source-id))))
	(def
	 (setf result (make-tokens def source-id))
	    (setf (expand-flag flags) t)))
	    
      (cons flags result))))
	


(let ((bytecodes (make-hash-table :test 'eq)))
  (mapcar (lambda (def)
	    (setf (gethash (car def) bytecodes) (cadr def)))
	  (opcodes:get-bytecodes-raw))
  (defun get-bytecode (token)
      "Returns the associated bytecode for a basic opcode token"
    (gethash token bytecodes)))

(let ((test-a 0))
  (defun testfun ()
    (let ((test-b (evenp test-a))
	  (test-flag nil))
      (incf test-a)
      (when test-b
	(setf test-flag t))
      (format t "~a ~a ~a ~%" test-a test-b test-flag))))
