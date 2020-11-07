(in-package :compiler)

(let ((character-scanners '((#\{ open-brace nil)
			    (#\} close-brace nil)
			    (#\( open-paren nil)
			    (#\) close-paren nil)
			    (#\; semicolon nil)
			    (#\- unop-negation t)
			    (#\~ unop-bitwise-complement t)
			    (#\! unop-bitwise-negation t)))
      (other-scanners (mapcar (lambda (args)
				(let ((regex (second args))
				      (sym (first args)))
				  (list (ppcre:create-scanner regex)
					sym)))
			      '((key-int4 "int4$")
				(key-int8 "int8$")
				(key-return "return$")
				(identifier "[a-zA-Z]\w*")
				(literal-int "[0-9]+")))))
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
	    (c-token (check-character-regexes c)))
	(if c-token
	    ;; If we have a TEC, add a string token if the built string is token-like
	    ;; and add a char token if the char is token-like
	    (let ((s-token (check-regexes built-string)))
	      (when s-token
		(setf built-string "")
		(setf (token-line-number s-token) line-number)
		(push s-token tokens))
	      (unless (equal c-token t)
		(setf (token-line-number c-token) line-number)
		(push c-token tokens)))
	    ;; If not a TEC, add the token to the built string
	    (setf built-string (concatenate 'string built-string (string c))))
	(lex reader tokens source-string line-number built-string))))
	
;; TODO : Delete this	    
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

