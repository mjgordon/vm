;; Calling tests from bash script
(load "~/lisp/quicklisp/setup.lisp")
(load "compiler.asd")
(load "compiler-test/compiler-test.asd")

(with-open-stream (*standard-output* (make-broadcast-stream))
  (handler-bind ((asdf:bad-system-name #'muffle-warning))
    (asdf:load-system :compiler)
    (asdf:load-system :compiler-test)))

(in-package :compiler-tests)

(diag "=== TESTING COMPILER ===")

(let ((arg (cadr sb-ext:*posix-argv*)))
  (cond
    ((string= arg "nls-test-suite")
     (progn
       (diag "=== NLS TEST SUITE === ")
       (compiler-tests:run-nls-test-suite)))))

(sb-ext:exit)
