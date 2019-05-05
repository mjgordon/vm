(in-package :tests)

(defun get-result (results)
  (read-line (sb-ext:process-output results) nil)) 


(defmacro prove-datatype (type-string)
  `(mapcar (lambda (value)
	     (is (get-result results) (concatenate 'string ,type-string (write-to-string value))))
	   values))


(defun prove-nb (values results)
  (prove-datatype "NB : "))

(defun prove-int8 (values results)
  (prove-datatype "INT8 : "))

(defun prove-int12 (values results)
  (prove-datatype "INT12 : "))

(defun prove-int16 (values results)
  (prove-datatype "INT16 : "))


(defmacro with-hxa (filename &body body)
  `(progn
     (assembler::assemble-hex ,(concatenate 'string "../programs/" filename ".hxa") :verbose nil)
     (let ((results (sb-ext:run-program "./../bin/vm"
					(list "-p" "-t" "-f" ,(concatenate 'string "../programs/" filename ".hxb"))
					:search "/usr/bin/sh"
					:output :stream)))
       ,@body)))


(defmacro count-tests (test-lists)
  `(+ ,@(mapcar (lambda (test-list)
		  (- (length test-list) 1))
		test-lists)))


(defmacro mnemonic-test (testname test-lists)
  `(progn
     (subtest ,testname
     (plan (count-tests ,test-lists))
     ,@(mapcar (lambda (test-list)
		 (let ((function-name (case (first test-list)
					(:nb 'prove-nb)
					(:int8 'prove-int8)
					(:int12 'prove-int12)
					(:int16 'prove-int16))))
		   `(,function-name ',(rest test-list) results)))
	       test-lists)
     (prove:finalize))))
      

  
