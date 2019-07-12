(in-package :compiler)

(defstruct token
  "Basic compilation unit. Value is the original source text. Semantic is whether the token should survive into the AST (i.e. vs 'syntactic')"
  type
  value
  semantic)

(defmacro add-token (type value &optional (semantic t))
  "Adds a new token to a cons list named 'tokens'"
  `(setf tokens (cons (make-token  :type ,type :value ,value :semantic ,semantic) tokens)))
