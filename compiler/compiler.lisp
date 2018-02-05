(load "~/lisp/quicklisp/setup.lisp")

(ql:quickload "split-sequence" :silent t)

(defun get-bytecodes ()
  "Generates a hashtable of the opcodes and their mnemonics"
  (let ((bytecodes (make-hash-table :test 'equal)))
    (mapcar #'(lambda (def)
		(setf (gethash (car def) bytecodes) (cadr def)))
	    '(("COLOR" #x0)
	      ("X" #x1)
	      ("Y" #x2)
	      ("PC" #x3)
	      ("MEM" #x4)
	      ("IO" #x5)
	      ("FLAG" #x6)
	      ("LIT" #x7)
	      ("ADD" #x8)
	      ("SUB" #x9)
	      ("PUSH" #xA)
	      ("POP" #xB)
	      ("PEEK" #xC)
	      ("COND" #xD)
	      ("NOR" #xE)
	      ("MOVE" #xF)))
    bytecodes))

(defun get-file (filename)
  "Load a source file as a collection of strings"
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
	 collect line)))

(defun preprocess (file)
  "Very dumb right now, will just unroll certain keywords into lists of other opcodes"
  (let ((output))
    (setf output file)
    output))


(defun write-bytecode (lines)
  "Performs the final pass of writing the unrolled source to a binary file"
  (with-open-file (stream "output.b"
			  :direction :output
			  :element-type 'unsigned-byte
			  :if-exists :supersede)
    (let ((bytecodes (get-bytecodes)))
	 (mapcar #'(lambda (line)
		     (setf line (split-sequence:split-sequence #\Space line))
		     (mapcar #'(lambda (item)
				 (if (numberp (read-from-string item))
				     (write-byte (read-from-string item) stream)
				     (write-byte (gethash item bytecodes) stream)))
			     line))
		 lines))))

(defun compile-hex (filename)
  (write-bytecode (preprocess (get-file filename))))

(compile-hex (cadr sb-ext:*posix-argv*))
