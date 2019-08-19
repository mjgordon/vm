(in-package :compiler)


(defun get-token-regexes ()
  "Return a list of regex pairs to current language features"
  (mapcar (lambda (arg)
	    (list (ppcre:create-scanner (second arg))
		  (first arg)))
	  '((key-int4 "int4$")
	    (key-int8 "int8$")
	    (key-return "return")
	    (identifier "[a-zA-Z]\w*")
	    (literal-int "[0-9]+")
	    (op-negation "-"))))


(let ((character-scanners (mapcar (lambda (args)
				    (let ((char (first args))
					  (sym (second args))
					  (tok (third args)))
				      (list char
					    sym
					    tok)))
				  '((#\{ open-brace nil)
				    (#\} close-brace nil)
				    (#\( open-paren nil)
				    (#\) close-paren nil)
				    (#\; semicolon nil)
				    (#\- unop-negation t))))
      (other-scanners (mapcar (lambda (args)
				(let ((regex (second args))
				      (sym (first args)))
				  (list (ppcre:create-scanner regex)
					sym)))
			      '((key-int4 "int4$")
				(key-int8 "int8$")
				(key-return "return")
				(identifier "[a-zA-Z]\w*")
				(literal-int "[0-9]+")))))
  (defun check-character-regexes (c)
    "Takes the current character from the source
Returns a token if applicable"
    (loop for scanner in character-scanners do
	 (when (char= (first scanner) c)
	   (return (make-token :type (second scanner) :value c :semantic (third scanner))))
	 (when (char= #\Space c)
	   (return t))))

  
  (defun check-regexes (s)
    "Takes a string being built from source characters
Returns a token if the string represents one"
    (loop for scanner in other-scanners do
	 (when (ppcre:scan (first scanner) s)
	   (return (make-token :type (second scanner) :value s :semantic t))))))
			   
    


					
  

  
      

(defmacro check-regexes-old (current)
  `(loop for regex in regexes do
	(when (ppcre:scan (first regex) ,current)
	  (add-token (second regex) ,current)
	  (setf ,current "")
	  (return))))

(defun lex (strings &optional (tokens ()) (source-string "") (built-string ""))
  "Takes a list of source line strings. 
Returns a list of tokens"
  (when (string-equal source-string "")
    (setf source-string (pop strings)))

  ;; Return the current list of tokens if done reading the source file
  (if (not source-string)
      (progn
	(push (make-token :type 'eof :value "EOF") tokens)
	(reverse tokens))
      (let* ((c (pop-string source-string))
	    (c-token (check-character-regexes c)))
	(if c-token
	    (let ((s-token (check-regexes built-string)))
	      (when s-token
		(setf built-string "")
		(push s-token tokens))
	      (when (not (equal c-token t))
		(push c-token tokens)))
	    (setf built-string (concatenate 'string built-string (string c))))
	(lex strings tokens source-string built-string))))
	
	    
	





    
  
;(defun lex (strings)
;  "Turns a list of source strings into a list of tokens"
;  (let ((tokens '())
;	(current ""))
;    (loop for string in strings do
;;	 (loop for c across string do
;;	      (check-regexes current)
;;		  (setf current (concatenate 'string current (string c))))) ;TODO this concat isn't great
;;	 (check-regexes current))
;;    (add-token 'eof "EOF")
;;    (format t "~a~%" (reverse tokens))
;;    (reverse tokens)

