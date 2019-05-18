(in-package :compiler)

(defstruct token
  type
  value
  tree)

(defun get-scanners ()
  (mapcar (lambda (arg)
	    (let ((type (first arg)) (expr (second arg)))
	      (list (ppcre:create-scanner expr)
		    type)))
     '((key-int4 "int4$")
       (key-return "return")
       (identifier "[a-zA-Z]\w*")
       (literal-int "[0-9]+"))))


(defun load-file-as-strings (filename)
  "Load a hxc file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defmacro add-token (type value &optional (tree t))
  `(setf tokens (cons (make-token  :type ,type :value ,value :tree ,tree) tokens)))

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
		    (cond ((equal c #\{) (add-token 'open-brace c nil))
			  ((equal c #\}) (add-token 'close-brace c nil))
			  ((equal c #\() (add-token 'open-paren c nil))
			  ((equal c #\)) (add-token 'close-paren c nil))
			  ((equal c #\;) (add-token 'semicolon c nil))
			  ((equal c #\ ) nil)))
		  (setf current (concatenate 'string current (string c))))) ;TODO this concat isn't great
	 (check-scanners current))

    (reverse tokens)))

(defun get-rules ()
  (let ((rule-table (make-hash-table)))
    (mapcar (lambda (rule)
	      (setf (get-hash (first rule) rule-table) (second rule)))
	    
	    '((_program (_func))
	      (_func (key-int4 identifier open-paren close-paren open-brace _statement close-brace))
	      (_statement (key-return _exp semicolon))
	      (_exp (literal-int))))
    rule-table))

(defun parse (tokens)
  (let ((tree '(_program . ()))
	(rules (get-rules))
	(get-token (lambda () (pop tokens))))
    (remove nil (parse-r get-token tree rules))))

(defun parse-r (get-token tree rules)
  (mapcar (lambda (branch)
	    (let ((rule (get-hash branch rules)))
	      (if rule
		  (make-token :type branch :value (remove nil (parse-r get-token rule rules)))
		  (let* ((token (funcall get-token))
			 (type (token-type token)))
		    (if (eq type branch)
			(if (token-tree token)
			    token
			    nil)
			(format t "bad token : ~a | Expected : ~a ~%" type branch))))))
	  tree))

    
	
    
(defmacro testmacro (input)
  (mapcar (lambda (word)
	    (format t "~a ~a~%" word (type-of word)))
	  input))


