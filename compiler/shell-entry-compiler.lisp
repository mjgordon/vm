;; Used when running the compiler from a bash script
(load "~/lisp/quicklisp/setup.lisp")
(load "compiler.asd")
(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :compiler)))

(compiler::compile-hxc (cadr sb-ext:*posix-argv*))

(exit)
