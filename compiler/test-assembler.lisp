(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :assembler)))

(in-package :tests)

(defun get-result (results)
  (read-line (sb-ext:process-output results) nil)) 

(defun prove-nb (values results)
  (mapcar (lambda (value)
	    (is (concatenate 'string "NB : " (write-to-string value)) (get-result results)))
	  values))




(defun test-opcodes ()
  (assembler::assemble-hex "../programs/test-opcodes.hxa" :verbose nil)
  (let ((results (sb-ext:run-program "./../bin/vm" (list "-p" "-t" "-f" "../programs/test-opcodes.hxb") :search "/usr/bin/sh" :output :stream)))
    (diag "Testing Opcodes:")
    (subtest "Opcode PC"
      (plan 1)
      (is (get-result results) "NB : 1")
      (prove:finalize))
    (subtest "Opcode ADD"
      (plan 4)
      (prove-nb '(5 0 0 1) results)
      (prove:finalize))
    (subtest "Opcode SUB"
      (plan 10)
      (prove-nb '(2 0 0 0 15 1 15 1 1 1) results)
      (prove:finalize))))


     

(test-opcodes)
  


