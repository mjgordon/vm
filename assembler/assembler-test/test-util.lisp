(defpackage :tests
  (:use :cl :asdf :prove)
  (:export :test-opcodes :test-macros))

(in-package :tests)

(defun get-result (results)
  "Returns one line of output from an external process"
  (read-line (sb-ext:process-output results) nil)) 


(defmacro prove-datatype (type-string)
  "Writes a single test, comparing to the vm datatype listed"
  `(mapcar (lambda (value)
	     (is (get-result results) (concatenate 'string ,type-string (write-to-string value))))
	   values))


(defun prove-nb (values results)
  "Write a test comparing nibbles"
  (prove-datatype "NB : "))

(defun prove-int8 (values results)
  "Write a test comparing 8-bit ints"
  (prove-datatype "INT8 : "))

(defun prove-int12 (values results)
  "Write a test comparing 12-bit ints"
  (prove-datatype "INT12 : "))

(defun prove-int16 (values results)
  "Write a test comparing 16-bit ints"
  (prove-datatype "INT16 : "))


(defmacro with-hxa (filename &body body)
  "Compiles and runs a hxa program, making the results available for tests"
  `(progn
     (assembler::assemble-hex ,(concatenate 'string "../programs/" filename ".hxa") :verbose nil)
     (let ((results (sb-ext:run-program "./../bin/vm"
					(list "-p" "-t" "-f" ,(concatenate 'string "../programs/" filename ".hxb"))
					:search "/usr/bin/sh"
					:output :stream)))
       ,@body)))


(defmacro count-tests (test-lists)
  "Sum the number of tests that are requested"
  `(+ ,@(mapcar (lambda (test-list)
		  (- (length test-list) 1))
		test-lists)))


(defmacro mnemonic-test (testname test-lists)
  "Run a group of tests on a single mnemonic"
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
      

  
