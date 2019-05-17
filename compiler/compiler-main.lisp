(in-package :compiler)

(defclass token ()
  ((type :initarg :type
	:accessor token-type)
  (value :initarg :value
	 :accessor token-value)))


(defun get-scanners ()
  (mapcar (lambda (arg)
	    (let ((type (first arg)) (expr (second arg)))
	      (list (ppcre:create-scanner expr)
		    type)))
     '((key-int "int\b")
       (key-return "return")
       (identifier "[a-zA-Z]\w*")
       (literal-int "[0-9]+"))))


(defun load-file-as-strings (filename)
  "Load a hxc file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defmacro add-token (type value)
  `(setf tokens (cons (make-instance 'token :type ,type :value ,value) tokens)))

(defmacro check-scanners (current)
  `(loop for scanner in scanners do
	(when (ppcre:scan (first scanner) ,current)
	  (add-token (second scanner) current)
	  (setf current "")
	  (return))))



(defun lex (strings)
  (let ((tokens '())
	(current "")
	(scanners (get-scanners))
	(end (ppcre:create-scanner "[{}(); ]")))
    (loop for string in strings do
	 (loop for c across string do
	      (if (ppcre:scan end (string c))
		  (progn
		    (check-scanners current)
		    (cond ((equal c #\{) (add-token 'open-brace nil))
			  ((equal c #\}) (add-token 'close-brace nil))
			  ((equal c #\() (add-token 'open-paren nil))
			  ((equal c #\)) (add-token 'close-paren nil))
			  ((equal c #\;) (add-token 'semicolon nil))
			  ((equal c #\ ) nil)))
		  (setf current (concatenate 'string current (string c))))) ;TODO this concat isn't great
	 (check-scanners current))

    tokens))

(defun print-tokens (tokens)
  (mapcar (lambda (token)
	    (format t "~a ~a~%" (token-type token) (token-value token)))
	  tokens))
