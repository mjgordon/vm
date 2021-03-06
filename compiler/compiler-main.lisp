(in-package :compiler)

;;; HXA-writing utilities

(defmacro append-line (line)
  `(setf output (cons ,line output)))

(defmacro append-lines (lines)
  `(mapcar (lambda (line)
	     (append-line line))
	   ,lines))


;;; Generating HXA from AST

(defun generate-unop (branch-unop)
  (case (token-type (first (token-value branch-unop)))
    ;; Negation operator (-)
    (unop-negation
     (concatenate 'string (generate-expression (second (token-value branch-unop))) " NEG "))
    (unop-bitwise-negation
     (concatenate 'string (generate-expression (second (token-value branch-unop))) " TRUE NOT "))
    (unop-bitwise-complement
     (concatenate 'string (generate-expression (second (token-value branch-unop))) " NOT "))
    ))

    

(defun generate-expression (branch-expression)
  (let ((exp-head (first (token-value branch-expression))))
    (case (token-type exp-head)
      (<unop-exp> (generate-unop exp-head))
      (literal-int (token-value exp-head)))))
		    

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
  "Generator entry. Accepts an AST and returns a list of HXA mnemonics"
  (if *error-list*
      (progn
	(when *verbose*
	  (print-error-list *error-list*))
	(list))
      (progn	
	(when *verbose*
	  (print-token-tree ast))
	(let ((output ()))
	  (append-line "CALL >main")
	  (append-line "GOTO >END")
	  (mapcar (lambda (branch-function)
		    (append-lines (generate-function branch-function)))
		  (token-value (car ast)))
	  (append-line "@END")
	  (reverse output)))))


(defun print-error-list (error-list)
  (format t "COMPILATION ERRORS~%")
  (format t "LINE   : ISSUE~%")
  
  (mapcar (lambda (e)
	    (let* ((token (cadadr e))
		   (line-number (token-line-number token))
		   (error-type (car e))
		   (expected-type (caadr e))
		   (actual-type (token-type token)))
	      (format t "~6d : ~a : Expected ~a, got ~a~%" line-number error-type expected-type actual-type)))
	  error-list))

  

;;; Main functions
(defun output-assembly (filename hxa)
  "Writes HXA mnemonics as text file"
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (mapcar (lambda (line)
	      (write-line line stream))
	    hxa)))

;;TODO convert this to arrow syntax for clarity?
(defun compile-hxc (filename-hxc &key (verbose t))
  "Main entry function. Reads an hxc file and attempts to output an hxa file"
  (setf *verbose* verbose)
  (clear-error-list)
  (let* ((path-divisor (search "/" filename-hxc :from-end t))
	 (filename-stripped (subseq filename-hxc path-divisor (search ".hxc" filename-hxc)))
	 (filepath (subseq filename-hxc 0 path-divisor))
	 (output-filename (concatenate 'string filepath filename-stripped ".hxa")))
    (load-file filename-hxc)
    (output-assembly output-filename (generate (parse (lex (get-reader
							    (list #'empty-string-p #'comment-string-p)))))))
  *error-list*)
	
    



