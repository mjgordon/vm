(in-package :assembler)

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
	      (setf (gethash (first def) definitions) (rest def)))
	    (opcodes:get-dictionary))
    ;(opcodes::shadow-dict definitions)
    (lambda (tokens)
      (let* ((token (car tokens))
	     (next (cdr tokens))
	     (value (gethash token definitions))
	     (def (car value))
	     (type (cadr value)))
	(cond ((not def) (cons nil (list token)))
	      ((eq type 'opcodes::next-token) (cons t (append def (list next) (caddr value))))
	      ((eq type 'opcodes::offset-label) (let ((sym (gensym)))
					 (insert-return-table sym (caddr value))
					 (cons nil (append def (list sym) (cadddr value)))))
	      ((eq type 'opcodes::local-labels) (let* ((local-count (caddr value))
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
    (mapcar (lambda (def)
	      (setf (gethash (car def) bytecodes) (cadr def)))
	    (opcodes:get-bytecodes-raw))
    bytecodes))
