(defsystem assembler
  :name "assembler"
  :components ((:file "packages")
	       (:file "assembler-globals")
	       (:file "assembler-util"
		      :depends-on ("assembler-globals"))
	       (:file "instruction-set"
		      :depends-on ("assembler-util"))
	       (:file "assembler-dictionary"
		      :depends-on ("assembler-globals" "assembler-util" "instruction-set"))
	       (:file "assembler-main"
		      :depends-on ("assembler-globals" "assembler-dictionary" "assembler-util"))))
