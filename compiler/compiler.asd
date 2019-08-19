(asdf:defsystem :compiler
  :name "compiler"
  :depends-on (:split-sequence :cl-ppcre :arrows)
  :components ((:file "packages")
	       (:file "error-handling")
	       (:file "compiler-util")
	       (:file "token")
	       (:file "compiler-lexer")
	       (:file "compiler-parser")
	       (:file "compiler-main")))
