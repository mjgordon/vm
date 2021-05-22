(asdf:defsystem :compiler-test
  :name "compiler-test"
  :depends-on(:prove :compiler :assembler)
  :components((:file "compiler-test")))
