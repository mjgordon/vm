(in-package :compiler)

(defstruct token
  "Basic compilation unit. Value is the original source text. Semantic is whether the token should survive into the AST (i.e. vs 'syntactic')"
  type
  value
  semantic)
