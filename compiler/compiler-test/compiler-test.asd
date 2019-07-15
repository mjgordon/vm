(asdf:defsystem :compiler-test
  :name "compiler-test"
  :depends-on(:prove :compiler)
  :components((:file "compiler-test")))
