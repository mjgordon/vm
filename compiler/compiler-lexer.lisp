(in-package :compiler)

(let ((character-scanners '((#\{ open-brace nil)
			    (#\} close-brace nil)
			    (#\( open-paren nil)
			    (#\) close-paren nil)
			    (#\; semicolon nil)
			    (#\- unop-negation t)
			    (#\~ unop-bitwise-complement t)
			    (#\+ binop-addition t)
			    (#\* binop-multiplication t)
			    (#\/ binop-division t)))
      (other-scanners (mapcar (lambda (args)
				(let ((regex (second args))
				      (sym (first args)))
				  (list (ppcre:create-scanner regex)
					sym)))
			      '((key-int4 "int4$")
				(key-int8 "int8$")
				(key-return "return$")
				(identifier "[a-zA-Z]\w*")
				(literal-int "[0-9]+")
				(logical-and "&&$")
				(logical-or "\\|\\|$")
				(comp-eq "==$")
				(comp-neq "\\!=$")
				(comp-lt "<$")
				(comp-gt ">$")
				(comp-lte "<=$")
				(comp-gte ">=$")
				(cast-int4 "\$int4$")
				(cast-int8 "\$int8$")
				(cast-int12 "\$int12$")
				(cast-int16 "\$int16$")
				(unop-logical-negation "!$")))))
  (defun check-character-regexes (c)
    "Takes the current character from the source
Returns a token if applicable
Returns t if the character is a space"
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


(defun lex (reader &optional (tokens ()) (source-string "") (line-number nil) (built-string ""))
  "Takes a list of source line strings. 
Returns a list of tokens"
  ;; If the source-string is exhausted, pop the next one from the source list
  (when (string-equal source-string "")
    (multiple-value-bind (line-t line-number-t) (funcall reader)
      (setf source-string line-t)
      (setf line-number line-number-t)))

  ;; source-string will be nil if the file is finished
  (if (not source-string)
      ;; Return the current list of tokens if done reading the source file
      (progn
	(push (make-token :type 'eof :value "EOF" :line-number line-number) tokens)
	(reverse tokens))
      ;; Otherwise pop the next character and check if its a Token Ending Character (TEC)
      (let* ((c (pop-string source-string))
	     (c-token (check-character-regexes c))
	     (unop-flag (and (equal (uiop:last-char built-string) #\!) (alphanumericp c))))
	;; TODO: Refactor this area
	(if (or c-token unop-flag)
	    ;; If we have a TEC, add a string token if the built string is token-like
	    ;; and add a char token if the char is token-like
	    (let ((s-token (check-regexes built-string)))
	      (when s-token
		(setf built-string "")
		(setf (token-line-number s-token) line-number)
		(push s-token tokens))
	      (when unop-flag
		(setf built-string (concatenate 'string built-string (string c))))
	      (unless (or (equal c-token t) unop-flag) ;; TEC was not a space
		(setf (token-line-number c-token) line-number)
		(push c-token tokens)))
	    ;; If not a TEC, add the token to the built string
	    (setf built-string (concatenate 'string built-string (string c))))
	(lex reader tokens source-string line-number built-string))))
	

