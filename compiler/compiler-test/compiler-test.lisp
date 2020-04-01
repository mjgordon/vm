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
  
  (diag "valid")
  (test-files "../programs/aux/write_a_c_compiler/stage_1/valid" t)
  
  (diag "invalid")
  (plan 4)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_paren.hxc" 'compiler::error-missing-paren)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_retval.hxc" 'compiler::error-missing-close-brace)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_brace.hxc" 'compiler::error-missing-close-brace)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_semicolon.hxc" 'compiler::error-missing-semicolon)
  (prove:finalize)
  (diag "what"))



(defun test-files (folder &optional (valid t))
  "Arguments are folder to look for single-file programs in, and whether the program is valid or not"
  (setf prove:*default-reporter* :fiveam)
  (setf prove:*default-reporter* :dot)
			    
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




  
  
