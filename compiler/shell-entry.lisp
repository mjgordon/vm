;; Used when running the assembler from a bash script

(load "~/lisp/quicklisp/setup.lisp")
(ql:quickload "split-sequence" :silent t)
(asdf:load-system 'assembler)
(assembler::assemble-hex (cadr sb-ext:*posix-argv*))
(exit)
