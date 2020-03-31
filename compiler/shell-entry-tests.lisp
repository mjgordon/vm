;; Calling tests from bash script
(load "~/lisp/quicklisp/setup.lisp")
(load "compiler.asd")
(load "compiler-test/compiler-test.asd")
(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :compiler)
    (asdf:load-system :compiler-test)))

(let ((arg (cadr sb-ext:*posix-argv*)))
  (cond
    ((string= arg "nls-test-suite") (compiler-tests:run-nls-test-suite))))

(exit)
