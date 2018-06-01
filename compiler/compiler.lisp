(load "~/lisp/quicklisp/setup.lisp")
(load "dictionary")
(load "util-assembler")

(ql:quickload "split-sequence" :silent t)

(defparameter *label-table* (make-hash-table :test 'eq))
(defparameter *call-table* (make-hash-table :test 'eq))
(defparameter *label-list* ())
(defparameter *call-list* ())


(defun get-file (filename)
  "Load a source file as a collection of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun generate-tables (lines)  
  "Splits lines to one list of words, removes and logs label tags"  
  (setf lines
	(mapcan (lambda (line)
		  (split-sequence:split-sequence #\Space line))
		(remove-lines lines)))

  (mapcar (lambda (word)
	    (let ((sym (intern word)))
	      (cond ((char= (char word 0) #\@) (setf *label-list* (adjoin sym *label-list*)))
		    ((char= (char word 0) #\>) (progn (setf (gethash sym *call-table*) (intern (substitute #\@ #\> word)))
						      (setf *call-list* (adjoin sym *call-list*))))
		    ((numberp (read-from-string word)) (setf sym (read-from-string word)))
		    (t sym))
	      sym))
	  lines))

(defun expand-tokens (tokens &optional (expander (get-dictionary-expander)))
  (let ((expansion (expand-pass tokens expander)))
    (if (car expansion)
	(expand-tokens (rest expansion) expander)
	(rest expansion))))

(defun resolve-labels (tokens)
  (let ((count 0))
    (setf tokens
	  (remove-if (lambda (token)
		       (cond ((member token *label-list*) (progn (setf (gethash token *label-table*) count)
								 t))
			     ((member token *call-list*) (progn (incf count 7)
								nil))
			     (t (progn (incf count)
				       nil))))
		     tokens)))
  (mapcan (lambda (token)
	    (if (member token *call-list*)
		(convert-address (gethash (gethash token *call-table*) *label-table*))
		(list token)))
	  tokens))
	      
		       

(defun write-bytecode (tokens)
  "Performs the final pass of writing the unrolled source to a binary file"

  (with-open-file (stream "program.hxb"
			  :direction :output
			  :element-type 'unsigned-byte
			  :if-exists :supersede)
    (let ((bytecodes (get-bytecodes)))
      (mapcar (lambda (token)
		(if (symbolp token)
		    (let ((byte (gethash token bytecodes)))
		      (if byte
			  (write-byte (gethash token bytecodes) stream)
			  (format t "Bad token: ~a" token)))
		    (write-byte token stream)))
	      tokens))))

(defun compile-hex (filename)
  (write-bytecode (resolve-labels (expand-tokens (generate-tables (get-file filename)))))
  (mapcar (lambda (key)
	    (format t "~a ~a ~%" key (gethash key *label-table*)))
	  (loop for key being the hash-keys of *label-table* collect key))
  nil)


(compile-hex (cadr sb-ext:*posix-argv*))
