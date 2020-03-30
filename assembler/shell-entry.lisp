;; Used when running the assembler from a bash script

(load "~/lisp/quicklisp/setup.lisp")
(load "assembler.asd")
(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :assembler)))

(assembler::assemble-hex (cadr sb-ext:*posix-argv*))

(exit)
