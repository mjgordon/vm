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
  ;;(setf prove:*default-reporter* :fiveam)
  
  
  ;;(run-nls-stage-1)
  ;;(run-nls-stage-2)
  ;;(run-nls-stage-3)
  ;;(run-nls-stage-4)
  (run-nls-stage-5)
  )

(defun run-nls-stage-1 ()
  (diag "~%== stage_1 ==")
  
  (diag "valid : compilation")
  (test-files "../programs/aux/write_a_c_compiler/stage_1/valid" t)
  
  (diag "valid : execution")
  (plan 6)
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/multi_digit" "4 6")
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/newlines" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/no_newlines" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/return_0" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/return_2" "2")
  (test-valid "../programs/aux/write_a_c_compiler/stage_1/valid/spaces" "0")
  (prove:finalize)

  (diag "invalid")
  (plan 6)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_paren.hxc" 'compiler::error-missing-paren)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/missing_retval.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_brace.hxc" 'compiler::error-missing-close-brace)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_semicolon.hxc" 'compiler::error-missing-semicolon)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/no_space.hxc" 'compiler::error-missing-close-brace) ;; TODO : Wrong
  (test-invalid "../programs/aux/write_a_c_compiler/stage_1/invalid/wrong_case.hxc" 'compiler::error-missing-close-brace) ;; TODO : Also wrong
  (prove:finalize)
  )

(defun run-nls-stage-2 ()
  (diag "~%== stage_2 ==")
  
  (diag "valid : compilation")
  (test-files "../programs/aux/write_a_c_compiler/stage_2/valid" t)

  (plan 7)
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/bitwise" "3")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/bitwise_zero" "15")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/neg" "11")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/nested_ops_2" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/nested_ops" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/not_five" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_2/valid/not_zero" "15")
  (prove:finalize)
  
  (diag "invalid")
  (plan 4)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/missing_const.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/missing_semicolon.hxc" 'compiler::error-missing-semicolon)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/nested_missing_const.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_2/invalid/wrong_order.hxc" 'compiler::error-unexpected-token)
  (prove:finalize))

(defun run-nls-stage-3 ()
  (diag "~%== stage_3 ==")
  
  (diag "valid : compilation")
  (test-files "../programs/aux/write_a_c_compiler/stage_3/valid" t)

  (diag "valid : execution")
  (plan 7)
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/add" "3")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/associativity_2" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/associativity" "12")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/div" "2")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/mult" "6")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/parens" "14")
  (test-valid "../programs/aux/write_a_c_compiler/stage_3/valid/precedence" "14")
  (prove:finalize)
  
  (diag "invalid")
  (plan 4)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_3/invalid/malformed_paren.hxc" 'compiler::error-missing-semicolon)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_3/invalid/missing_first_op.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_3/invalid/missing_second_op.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_3/invalid/no_semicolon.hxc" 'compiler::error-missing-semicolon)
  (prove:finalize))

(defun run-nls-stage-4 ()
  (diag "~%== stage_4 ==")
  (diag "valid : compilation")
  (test-files "../programs/aux/write_a_c_compiler/stage_4/valid" t)
  (diag "valid : execution")
  (plan 20)
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/and_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/and_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/eq_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/eq_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/ge_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/ge_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/gt_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/gt_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/le_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/le_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/lt_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/lt_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/ne_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/ne_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/or_false" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/or_true" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/precedence_2" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/precedence_3" "0")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/precedence_4" "1")
  (test-valid "../programs/aux/write_a_c_compiler/stage_4/valid/precedence" "1")
  ;; readd skip-on-failures here
  (prove:finalize)

  (diag "invalid")
  (plan 1)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_4/invalid/missing_first_op.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_4/invalid/missing_mid_op.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_4/invalid/missing_second_op.hxc" 'compiler::error-unexpected-token)
  (test-invalid "../programs/aux/write_a_c_compiler/stage_4/invalid/missing_semicolon.hxc" 'compiler::error-missing-semicolon)

  
  )

(defun run-nls-stage-5 ()
  (diag "~%== stage_5 ==")
  (diag "valid : compilation")
  (test-files "../programs/aux/write_a_c_compiler/stage_5/valid" t))
  


(defun run-program (filename)
  (let ((process (sb-ext:run-program "/home/matt/projects/vm/bin/vm"
				     (list "-f" filename "-t")
				     :output :stream
				     :wait t)))
    (let* ((stream (sb-ext:process-output process))
	   (output (loop for line = (read-line stream nil nil)
		      while line collect line)))
      (sb-ext:process-close process)
      output)))

(defun test-valid (filepath expected-return-value)
  "Tests a single valid program, for parsing validity and for a simple expected output value"
  (compiler:compile-hxc (format nil "~a.hxc" filepath) :verbose nil :use-runtime t)
  (assembler:assemble-hex (format nil "~a.hxa" filepath) :verbose nil)
  (let ((output (run-program (format nil "~a.hxb" filepath))))
    (is (first output)
	expected-return-value
	(file-namestring filepath))))
  

(defun test-files (folder &optional (valid t))
  "Arguments are folder to look for single-file programs in, and whether the program is valid or not"
			    
  (let ((filenames (directory (concatenate 'string folder "/*.hxc"))))
    (plan (length filenames))
    (loop for filename in filenames do
	 (is (compiler:compile-hxc (format nil "~a" filename) :verbose nil :use-runtime t)
	     (not valid)
	     (file-namestring filename)))
    (prove:finalize)))

(defun test-invalid (filepath expected-error)
  "Supply a file and the error that should be reported"
  (let ((error-list (compiler:compile-hxc filepath :verbose nil :use-runtime t)))
    (is (caar error-list)
	expected-error
	(file-namestring filepath))))
