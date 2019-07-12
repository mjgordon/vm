(in-package :compiler)

(defmacro append-line (line)
  `(setf output (cons ,line output)))

(defmacro append-lines (lines)
  `(mapcar (lambda (line)
	     (setf output (cons line output)))
	   ,lines))

(defun generate-expression (branch-expression)
  ;;(format t "~a~%"  (type-of (string (token-value (first (token-value branch-expression))))))
  (token-value (first (token-value branch-expression))))

(defun generate-statement (branch-statement)
  (let* ((sm-values (token-value branch-statement))
	 (sm-type (token-type (first sm-values))))
    (cond ((eq sm-type 'key-return) (list (concatenate 'string "LIT PUSH " (generate-expression (second sm-values)))))
	  (t (format t sm-type)))))
			     

(defun generate-function (branch-function)
  (let* ((fun-values (token-value branch-function))
	 (fun-type (token-type (first fun-values)))
	 (fun-id (token-value (second fun-values)))
	 (fun-statements (cddr fun-values))
	 (output ()))
    (append-line (concatenate 'string "@" fun-id))
    (mapcar (lambda (statement)
	      (append-lines (generate-statement statement)))
	    fun-statements)
    (append-line "RET")
    (reverse output)))
    
    

(defun generate (ast)
  (print-token-tree ast)
  (let ((output ()))
    (append-line "CALL >main")
    (append-line "GOTO >END")
    (mapcar (lambda (branch-function)
	      (append-lines (generate-function branch-function)))
	    (token-value (car ast)))
    (append-line "@END")
    (reverse output)))

(defun output-assembly (filename hxa)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (mapcar (lambda (line)
	      (write-line line stream))
	    hxa)))

(defun compile-hxc (filename-hxc)
  (let* ((path-divisor (search "/" filename-hxc :from-end t))
	 (filename-stripped (subseq filename-hxc path-divisor (search ".hxc" filename-hxc)))
	 (filepath (subseq filename-hxc 0 path-divisor))
	 (output-filename (concatenate 'string filepath filename-stripped ".hxa")))
    
    (output-assembly output-filename (generate (parse (lex (load-file-as-strings filename-hxc)))))))
	
    
(defmacro testmacro (input)
  (mapcar (lambda (word)
	    (format t "~a ~a~%" word (type-of word)))
	  input))


