(defpackage :compiler-tests
  (:use :cl :asdf :prove)
  (:export :run-nls-test-suite))

(in-package :compiler-tests)

(defun get-filetype (filename)
  (subseq filename (- (length filename) 3 )))

(defun is-hxc-file (filename)
  (string-equal (get-filetype filename) "hxc"))

(defun run-nls-test-suite ()
  (diag "stage_1")
  (test-files "../programs/aux/write_a_c_compiler/stage_1/valid" t)
  (plan 2)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_paren.hxc" 'compiler::error-missing-paren)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_retval.hxc" 'compiler::error-missing-retval)
  (prove:finalize))



(defun test-files (folder &optional (valid t))
  "Arguments are folder to look for single-file programs in, and whether the program is valid or not"
  ;;(setf prove:*default-reporter* :fiveam)
			    
  (let ((filenames (directory (concatenate 'string folder "/*.hxc"))))
    (plan (length filenames))
    (loop for filename in filenames do
	 (is (compiler:compile-hxc (format nil "~a" filename) :verbose nil)
	     (not valid)
	     (file-namestring filename)))
    (prove:finalize)))

(defun test-invalid (filepath expected-error)
  (let ((error-list (compiler:compile-hxc filepath :verbose nil)))
    (is (caar error-list) expected-error)))




  
  
