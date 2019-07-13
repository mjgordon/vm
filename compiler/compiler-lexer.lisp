(in-package :compiler)


(defun get-token-regexes ()
  "Return a list of regex pairs to current language features"
  (mapcar (lambda (arg)
	    (let ((type (first arg)) (expr (second arg)))
	      (list (ppcre:create-scanner expr)
		    type)))
	  '((key-int4 "int4$")
	    (key-int8 "int8$")
	    (key-return "return")
	    (identifier "[a-zA-Z]\w*")
	    (literal-int "[0-9]+"))))

(defmacro check-regexes (current)
  `(loop for regex in regexes do
	(when (ppcre:scan (first regex) ,current)
	  (add-token (second regex) ,current)
	  (setf ,current "")
	  (return))))

(defun lex (strings)
  "Turns a list of source strings into a list of tokens"
  (let ((tokens '())
	(current "")
	(regexes (get-token-regexes))
	(regex-end (ppcre:create-scanner "[{}(); ]")))
    (loop for string in strings do
	 (loop for c across string do
	      (if (ppcre:scan regex-end (string c))
		  (progn
		    (check-regexes current)
		    (cond ((equal c #\{) (add-token 'open-brace c nil))
			  ((equal c #\}) (add-token 'close-brace c nil))
			  ((equal c #\() (add-token 'open-paren c nil))
			  ((equal c #\)) (add-token 'close-paren c nil))
			  ((equal c #\;) (add-token 'semicolon c nil))
			  ((equal c #\ ) nil)))
		  (setf current (concatenate 'string current (string c))))) ;TODO this concat isn't great
	 (check-regexes current))
    (add-token 'eof "EOF")
    (reverse tokens)))

