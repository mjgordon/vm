(load "~/lisp/quicklisp/setup.lisp")
(load "dictionary.lisp")

(ql:quickload "split-sequence" :silent t)
(ql:quickload "alexandria" :silent t)

(defun get-file (filename)
  "Load a source file as a collection of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun convert-to-symbols (lines)
  (mapcar (lambda (line)
	    (setf line (split-sequence:split-sequence #\Space line))
	    (mapcar (lambda (word)
		      (if (numberp (read-from-string word))
			  (read-from-string word)
			  (intern word)))
		    line))
	  lines))
  

(defun preprocess (lines)
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
  (write-bytecode (preprocess (convert-to-symbols (get-file filename)))))


(compile-hex (cadr sb-ext:*posix-argv*))
