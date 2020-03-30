(defpackage :assembler
  (:use :cl :asdf)
  (:export :assemble-hex))

(defpackage :opcodes
  (:export :get-dictionary :get-bytecodes-raw))

