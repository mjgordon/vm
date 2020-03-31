(asdf:defsystem :compiler
  :name "compiler"
  :depends-on (:split-sequence :cl-ppcre :arrows)
  :components ((:file "packages")
	       (:file "error-handling")
	       (:file "hxc-source-file")
	       (:file "token")
	       (:file "compiler-util")
	       (:file "compiler-lexer")
	       (:file "compiler-parser")
	       (:file "compiler-main")))
