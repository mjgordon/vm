(in-package :compiler)

(defstruct token
  "Basic compilation unit. 
Line number stores the originating source-code line number.
Value is the original source text. 
Datatype is datatype of data chunk at that position, if it exists.
Semantic is whether the token should survive into the AST (i.e. vs 'syntactic')"
  type
  line-number
  value
  datatype
  semantic)
