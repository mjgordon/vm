(defpackage :assembler
  (:use :cl :asdf)
  (:export :assemble-hex))

(defpackage :opcodes
  (:use :cl :asdf)
  (:export :get-dictionary))
