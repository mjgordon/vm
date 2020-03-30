;; Calling tests from bash script
(load "~/lisp/quicklisp/setup.lisp")
(load "assembler.asd")
(load "assembler-test/assembler-test.asd")
(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :assembler)
    (asdf:load-system :assembler-test)))

(let ((arg (cadr sb-ext:*posix-argv*)))
  (cond
    ((string= arg "opcodes") (tests:test-opcodes))
    ((string= arg "macros") (tests:test-macros))))
      
(exit)
