(defpackage :compiler-tests
  (:use :cl :asdf :prove)
  (:export :run-nls-test-suite))

(in-package :compiler-tests)

(defun get-filetype (filename)
  "Naive way to get file extension, just returns last three characters"
  (subseq filename (- (length filename) 3 )))

(defun is-hxc-file (filename)
  (string-equal (get-filetype filename) "hxc"))

(defun run-nls-test-suite ()
  "Test the suite of programs adapted from nlsandler's 'write a c compiler' guide"

  ;;(setf prove:*default-reporter* :dot)
  ;;(setf prove:*default-reporter* :tap) ;; No
  (setf prove:*default-reporter* :fiveam)
  
  
  (diag "~%== stage_1 ==")
  
  (diag "valid")
  (test-files "../programs/aux/write_a_c_compiler/stage_1/valid" t)
  (diag "invalid")
  (plan 6)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_paren.hxc" 'compiler::error-missing-paren)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_retval.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_brace.hxc" 'compiler::error-missing-close-brace)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_semicolon.hxc" 'compiler::error-missing-semicolon)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_space.hxc" 'compiler::error-missing-close-brace) ;; TODO : Wrong
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/wrong_case.hxc" 'compiler::error-missing-close-brace) ;; TODO : Also wrong
  (prove:finalize)

  (diag "~%== stage_2 ==")
  (diag "valid")
  (test-files "../programs/aux/write_a_c_compiler/stage_2/valid" t)
  (diag "invalid")
  (plan 4)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/missing_const.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/missing_semicolon.hxc" 'compiler::error-missing-semicolon)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/nested_missing_const.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/wrong_order.hxc" 'compiler::error-missing-semicolon)
  (prove:finalize))



(defun test-files (folder &optional (valid t))
  "Arguments are folder to look for single-file programs in, and whether the program is valid or not"
			    
  (let ((filenames (directory (concatenate 'string folder "/*.hxc"))))
    (plan (length filenames))
    (loop for filename in filenames do
	 (is (compiler:compile-hxc (format nil "~a" filename) :verbose nil)
	     (not valid)
	     (file-namestring filename)))
    (prove:finalize)))

(defun test-invalid (filepath expected-error)
  "Supply a file and the error that should be reported"
  (let ((error-list (compiler:compile-hxc filepath :verbose nil)))
    (is (caar error-list)
	expected-error
	(file-namestring filepath))))




  
  
