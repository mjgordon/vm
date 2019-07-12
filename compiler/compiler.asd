(asdf:defsystem :compiler
  :name "compiler"
  :depends-on (:prove :split-sequence :cl-ppcre)
  :components ((:file "packages")
	       (:file "compiler-util")
	       (:file "token")
	       (:file "compiler-lexer")
	       (:file "compiler-parser")
	       (:file "compiler-main")))
