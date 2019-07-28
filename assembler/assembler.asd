(asdf:defsystem :assembler
  :name "assembler"
  :depends-on (:prove :split-sequence)
  :components ((:file "packages")
	       (:file "token")
	       (:file "assembler-globals")
	       (:file "assembler-util"
		      :depends-on ("assembler-globals"))
	       (:file "instruction-set"
		      :depends-on ("assembler-util"))
	       (:file "assembler-dictionary"
		      :depends-on ("assembler-globals" "assembler-util" "instruction-set"))
	       (:file "assembler-main"
		      :depends-on ("assembler-globals" "assembler-dictionary" "assembler-util"))

	       (:file "test-util")
	       (:file "test-assembler"
		      :depends-on("test-util"))
	       (:file "test-macros"
		      :depends-on("test-util"))))
		
