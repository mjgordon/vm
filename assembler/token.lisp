(in-package :assembler)

(defstruct token
  "A single assembler token" 
  value
  source-id)
  
	    
(defun make-tokens (input-list &optional parent-source-id)
  (mapcar (lambda (val)
	    (make-token :value val :source-id parent-source-id))
	  input-list))
  
