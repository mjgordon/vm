(in-package :compiler)

(defparameter *function-datatype* nil)

;;; HXA-writing utilities

(let ((output-list ()))
  (defun output-list-reset()
    (setf output-list ()))

  (defun append-line (line)
    (setf output-list (cons line output-list)))

  (defun append-lines (lines)
    (mapcar (lambda (line)
	      (append-line line))
	    lines))

  (defun get-output-list ()
    (reverse output-list)))


;;; Macros

(defmacro pass-datatype (parent generator branch)
  "Run the branch through the supplied generator, then set the parent datatype to match the branch"
  `(let ((output (funcall ,generator ,branch)))
     (setf (token-datatype ,parent) (token-datatype ,branch))
     output))

(defmacro defun-level-head (name body-name child-name)
  `(defun ,name (branch)
     (let ((values (token-value branch)))
       (format nil "~{~a~}"
	       (cons (pass-datatype branch #',child-name (first values))
		     (mapcar (lambda (value)
			       (prog1 (,body-name branch value)
				 (setf (token-datatype branch)
				       (max-datatype (token-datatype branch)
						     (token-datatype value)))))
			     (rest values)))))))
			      
     


;;; Generating HXA from AST

(defun generate-unop (branch)
  (let* ((values (token-value branch))
	 (factor (second values)))
    (concatenate 'string
		 (pass-datatype branch #'generate-factor factor)
		 (case (token-type (first values))
		   (unop-negation (format nil "NEG_~a " (get-datatype-size (token-datatype branch))))
		   (unop-logical-negation "TRUE NOT ")
		   (unop-bitwise-complement "NOT ")))))




(defun generate-factor (branch)
  (let ((values (token-value branch)))
    (case (token-type (first values))
      (literal-int (let* ((lit (token-value (first values)))
			  (lit-numeric (parse-integer lit)))
		      (concatenate 'string
				   (cond ((< lit-numeric 16)
					  (setf (token-datatype branch) 'int4)
					  "LIT PUSH ")
					 ((< lit-numeric 256)
					  (setf (token-datatype branch) 'int8)
					  "LIT PUSH_8 ")
					 ((< lit-numeric 4096)
					  (setf (token-datatype branch) 'int12)
					  "LIT PUSH_12 ")
					 ((< lit-numeric 65536)
					  (setf (token-datatype branch) 'int16)
					  "LIT PUSH_16 "))
				   lit
				   " ")))
      ;;(<paren-exp> (generate-expression (first (token-value (first values)))))
      (<paren-exp> (pass-datatype branch
				 #'generate-expression
				 (first (token-value (first values)))))
      (<unop-exp> (pass-datatype branch
				#'generate-unop
				(first values))))))

(defun generate-term-body(parent branch)
  (let* ((values (token-value branch))
	 (factor-b (second values))
	 (factor-b-result (generate-factor factor-b))
	 (type-a (token-datatype parent))
	 (type-b (token-datatype factor-b)))
    (concatenate 'string
		 factor-b-result
		 (if (compare-types type-b type-a) (get-type-swap type-a type-b) "")
		 (case (token-type (first values))
		   ;; MULTIPLACTION
		   (binop-multiplication
		    (setf (token-datatype branch) (add-datatype-sizes type-a type-b))
		    (let ((ordered-types (order-datatype-sizes type-a type-b)))
		      (format nil "MULT_~a_~a " (first ordered-types) (second ordered-types))))
		   ;; DIVISION
		   (binop-division
		    (setf (token-datatype branch) (max-datatype type-a type-b))
		    (let ((ordered-types (order-datatype-sizes type-a type-b)))
		      "DIV_4_4 "))))))

(defun-level-head generate-term generate-term-body generate-factor)

(defun generate-expression-body (parent branch)
  (let* ((values (token-value branch))
	 (term-b (second values))
	 (term-b-result (generate-term term-b))
	 (type-a (token-datatype parent))
	 (type-b (token-datatype term-b))
	 (type-order (compare-types type-b type-a)))
    (concatenate 'string
		 term-b-result
		 (if type-order (get-type-swap type-a type-b) "")
		 (case (token-type (first values))
		   ;; ADDITION
		   (binop-addition
		    (setf (token-datatype branch) (add-datatype-sizes type-a type-b))
		    (let ((ordered-types (order-datatype-sizes type-a type-b)))
		      (format nil "ADD_~a_~a " (first ordered-types) (second ordered-types))))
		   ;; SUBTRACTION
		   (unop-negation
		    (setf (token-datatype branch) (max-datatype type-a type-b))
		    (let ((ordered-types (order-datatype-sizes type-a type-b)))
		      (format nil "~aSUB_~a_~a "
			      (if type-order
				  (format nil "NEG_~a " (get-datatype-size type-a))
				  "")
			      (first ordered-types)
			      (second ordered-types))))))))


(defun-level-head generate-expression generate-expression-body generate-term)

;;(defun generate-expression-relational (branch)
;;  (let ((values (token-value branch)))
				       


	       
(defun generate-statement (branch)
  (let ((values (token-value branch)))
    (case (token-type (first values))
      (key-return (let ((branch-exp (second values)))
		    (list (generate-expression branch-exp)
			  (get-cast (token-datatype branch-exp) *function-datatype*)))))))

			     

(defun generate-function (branch)
  (let* ((values (token-value branch))
	 (fun-type  (intern (string-upcase (token-value (first (token-value (first values))))) :compiler))
	 (fun-id (token-value (second values)))
	 (fun-statements (cddr values)))
    (setf *function-datatype* fun-type)
    (append-line (concatenate 'string "@" fun-id))
    (mapc (lambda (statement)
	    (append-lines (generate-statement statement)))
	  fun-statements)
    (append-line "RET")))
    

(defun generate (ast)
  "Generator entry. Accepts an AST and returns a list of HXA mnemonics"
  (output-list-reset)
  (if *error-list*
      (progn
	(when *verbose*
	  (print-error-list *error-list*))
	(list))
      (progn	
	(when *verbose*
	  (print-token-tree ast))

	(append-line "CALL >main")
	(append-line "GOTO >END")
	(mapcar (lambda (branch-function)
		   (generate-function branch-function))
		(token-value (car ast)))
	(append-line "@END")
	(get-output-list))))


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
	
    



