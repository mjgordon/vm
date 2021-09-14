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

(defun tv (token)
  "Returns the token-value of a token"
  (token-value token))

(defun ftv (token)
  "Returns the first token-value of a token"
  (first (token-value token)))

(defun stv (token)
  "Returns the second token-value of a token"
  (second (token-value token)))


