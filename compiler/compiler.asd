(asdf:defsystem :compiler
  :name "compiler"
  :depends-on (:prove :split-sequence :cl-ppcre)
  :components ((:file "packages")
	       (:file "compiler-main")
	       (:file "compiler-util")))
