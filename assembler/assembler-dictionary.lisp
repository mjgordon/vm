(in-package :assembler)

#||
=== Dictionary Flow === 
expand-tokens : calls itself recursively to call each expansion pass
recursive-expand-pass : steps through the token list with tail recursion, returns a cons's list of expansions
dictionary-expand : returns a list of expanded tokens, given the current and next token. currently has hella side effects.
||#

  
(defun recursive-expand-pass (tokens result pass-flag)
  "Steps through the token list with tail recursion, expanding or skipping as necessary"
  (let* ((expansion-result (dictionary-expand tokens))
	 (expand-flag (first expansion-result))
	 (tokens-new (second expansion-result))
	 (expansion (third expansion-result)))
    
    ;; Mark that an expansion has occurred this pass
    (when expand-flag
      (setf pass-flag t))

    ;; Add either the expansion or (token) to the result
    (setf result (cons expansion result))

    ;; Call the function again if there are more tokens to process, else return the result and flag
    (if tokens-new
	(recursive-expand-pass tokens-new
			       result
			       pass-flag)
	(cons pass-flag result))))


		  
(let ((definitions (make-hash-table :test 'eq)))
  (loop for def in (opcodes:get-dictionary) do
       (setf (gethash (first def) definitions) (rest def)))
  (defun dictionary-expand (tokens)
    "Performs a macro expansion based on the first token in the input, returns an expasion flag, the list of 
remaining tokens, and a list of the expansion
Returns (expand-flag (remaining-tokens) (expansion))"
    (let* ((token (pop tokens))
	   (dict-entry (gethash (token-value token) definitions))
	   (def (first dict-entry))
	   (type (second dict-entry))
	   (expand-flag nil)
	   (result (list token))
	   (source-id (token-source-id token)))
      
      (when def
	(setf result (make-tokens def source-id))
	(setf expand-flag t))

      (cond	
	;; Insert the next token into the definition as well (expansion occurs)
	((eq type 'opcodes::next-token)
	 (setf result (append (make-tokens def source-id)
			      (list (pop tokens))
			      (make-tokens (third dict-entry) source-id))))
	
	;; Sets offset label with its initial offset dict-entry in the return table
	((eq type 'opcodes::offset-label)
	 (let ((sym (gensym)))
	   (insert-return-table sym (third dict-entry))
	   (setf result (append (make-tokens def source-id)
				(make-tokens (list sym) source-id)
				(make-tokens (fourth dict-entry) source-id)))))
	;; Sets up local labels if applicable
	((eq type 'opcodes::local-labels)
	 (let* ((local-count (third dict-entry))
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
	;; Splits larger numbers into a series of nb for LIT PUSH
	((eq type 'opcodes::next-to-nb)
	 (let ((nb-count (third dict-entry)))
	   (setf result (make-tokens (append def (convert-number (pop tokens) nb-count)) source-id))))
	;; When theres a normal expansion, use it 
	(def (setf result (make-tokens def source-id))))

	    
      (list expand-flag tokens result))))
	


(let ((bytecodes (make-hash-table :test 'eq)))
  (mapcar (lambda (def)
	    (setf (gethash (car def) bytecodes) (cadr def)))
	  (opcodes:get-bytecodes-raw))
  (defun get-bytecode (token)
      "Returns the associated bytecode for a basic opcode token"
    (gethash token bytecodes)))


(defmacro get-emacs-regex-function ()
  "Call this with macroexpand, copy into and run in scratch buffer"
  (let* ((not-included '("CALL" "GOTO" "RET"))
	 (names (remove-if 'null (mapcar (lambda (item)
					   (let ((name (symbol-name (first item))))
					     (unless (member name not-included)
					       name)))
					 (opcodes:get-dictionary)))))
    
    `(regexp-opt ' (,@names))))
