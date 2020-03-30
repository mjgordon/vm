(asdf:defsystem :assembler-test
  :name "assembler-test"
  :depends-on (:prove :assembler)
  :components((:file "test-util")
	      (:file "test-assembler")
	      (:file "test-macros")))
	       
