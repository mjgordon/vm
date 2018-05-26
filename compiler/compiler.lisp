(load "~/lisp/quicklisp/setup.lisp")
(load "dictionary.lisp")
(load "util-assembler.lisp")

(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)



(defun get-file (filename)
  "Load a source file as a collection of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun generate-label-table (lines)  
  "Splits lines to one list of words, removes and logs label tags"
  (defparameter *label-table* (make-hash-table :test 'eq))
  (setf lines
	(mapcan (lambda (line)
		  (split-sequence:split-sequence #\Space line))
		(remove-lines lines)))

  (let ((count 0))
    (remove-if (lambda (word)
		 (cond ((char= (char word 0) #\@) (progn (setf (gethash (subseq word 1) *label-table*) count)
							 (print count)
							 t))
		       ((char= (char word 0) #\>) (progn (incf count 4)
							 nil))
		       (t (progn (incf count) nil))))
	       lines)))
 
  
(defun convert-to-symbols (lines)
  (mapcar (lambda (line)
	    (mapcar (lambda (word)
		      (if (numberp (read-from-string word))
			  (read-from-string word)
			  (intern word)))
		    (split-sequence:split-sequence #\Space line)))
	  lines))
  

(defun unroll (lines)
  "Very dumb right now, will just unroll certain keywords into lists of other opcodes"

  (setf lines (alexandria:flatten lines))
  (let ((dictionary (get-dictionary)) (flag nil))
    (setf lines
	  (mapcar (lambda (item)
		    (let ((hashvalue (gethash item dictionary)))
		      (if (eq hashvalue nil)
			  item
			  (progn (setf flag t) hashvalue))))
		  lines))
    (if flag
	(setf lines(preprocess lines)))
    lines))

(defun write-bytecode (lines)
  "Performs the final pass of writing the unrolled source to a binary file"
  (setf lines (alexandria:flatten lines))

  (with-open-file (stream "program.hxb"
			  :direction :output
			  :element-type 'unsigned-byte
			  :if-exists :supersede)
    (let ((bytecodes (get-bytecodes)))
      (mapcar (lambda (item)
		(if (eq (type-of item) 'symbol)
		    (write-byte (gethash item bytecodes) stream)
		    (write-byte item stream)))
	      lines))))

(defun compile-hex (filename)
  (write-bytecode (unroll (convert-to-symbols (generate-label-table (get-file filename))))))


(compile-hex (cadr sb-ext:*posix-argv*))
