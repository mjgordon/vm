(defpackage :assembler
  (:use :cl :asdf)
  (:export :assemble-hex))

(defpackage :opcodes
  (:export :get-dictionary :get-bytecodes-raw))

(defpackage :tests
  (:use :cl :asdf :prove)
  (:export :test-opcodes :test-macros))
