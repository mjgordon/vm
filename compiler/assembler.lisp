;;;;; assembler.lisp

(load "~/lisp/quicklisp/setup.lisp")
(load "dictionary")
(load "util-assembler")

(ql:quickload "split-sequence" :silent t)


(defparameter *label-list* ())
(defparameter *label-table* (make-hash-table :test 'eq))

(defparameter *ref-list* ())
(defparameter *ref-table* (make-hash-table :test 'eq))

(defparameter *return-table* (make-hash-table :test 'eq))




(defun get-file (filename)
  "Load an assembly file as a list of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))


(defun generate-tables (lines)
  "Removes comments and blank lines, splits lines into one list of words. Converts to tokens, 
while recording lists of label tags and references."
  (setf lines
	(mapcan (lambda (line)
		  (split-sequence:split-sequence #\Space line))
		(remove-lines lines)))

  (mapcar (lambda (word)
	    (let ((sym (intern word)) (ch0 (char word 0)))
	      (cond ((char= ch0 #\@) (insert-label-set sym))
		    ((char= ch0 #\>) (progn (insert-ref-map sym (intern (substitute #\@ #\> word)))
					    (insert-ref-set sym)))
		    ((char= ch0 #\+) (let ((offset 0))
				       (progn (setf sym (gensym))
					      (when (> (length word) 1)
						(setf offset (read-from-string (subseq word 1))))
					      (insert-return-map sym offset))))
		    ((numberp (read-from-string word)) (setf sym (read-from-string word)))
		    (t sym))
	      sym))
	  lines))


(defun expand-tokens (tokens &optional (expander (get-dictionary-expander)))
  "Expands folded opcodes into the basic opcode forms. Called recursively until run reports no expansions."
  (let ((expansion (expand-pass tokens expander)))
    (if (car expansion)
	(expand-tokens (rest expansion) expander)
	(rest expansion))))


(defun resolve-labels (tokens)
  "Finds label tag locations and replaces label references with these locations"
  (let ((count 0))
    (setf tokens
	  (remove-if (lambda (token)
		       (cond ((member token *label-list*) (progn (insert-label-map token count)
								 t))
			     ((gethash token *return-table*) (progn (incf (gethash token *return-table*) count)
								    (incf count 7)
								    nil))
			     ((member token *ref-list*) (progn (incf count 7)
							       nil))
			     (t (progn (incf count)
				       nil))))
		     tokens)))
  (mapcan (lambda (token)
	    (cond ((member token *ref-list*) (convert-address (gethash (gethash token *ref-table*) *label-table*)))
		  ((gethash token *return-table*) (convert-address (gethash token *return-table*)))
		  (t (list token))))
	  tokens))
		       

(defun write-bytecode (tokens)
  "Converts list of opcode-tokens to list of associated bytes, and writes these to the output .hxb file"
  (with-open-file (stream "program.hxb"
			  :direction :output
			  :element-type 'unsigned-byte
			  :if-exists :supersede)
    (let ((bytecodes (get-bytecodes)))
      (mapcar (lambda (token)
		(if (symbolp token)
		    (let ((byte (gethash token bytecodes)))
		      (if byte
			  (write-byte byte stream)
			  (format t "Bad token: ~a ~%" token)))
		    (write-byte token stream)))
	      tokens))))


(defun compile-hex (filename)
  "Composites all previous assembly steps. Reports any feedback"
  (write-bytecode (resolve-labels (expand-tokens (generate-tables (get-file filename)))))
  nil)


;;; Only called when run from bash
(compile-hex (cadr sb-ext:*posix-argv*))
