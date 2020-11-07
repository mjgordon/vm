(in-package :compiler)

(defparameter *error-list* ())

(defun get-error-type (expected-token received-token)
  (cond ((equal expected-token 'close-paren) 'error-missing-paren)
	((equal expected-token 'close-brace) 'error-missing-close-brace)
	((equal expected-token 'semicolon) 'error-missing-semicolon)))

(defun clear-error-list ()
  (setf *error-list* ()))

(defun log-error (type data)
  (setf *error-list* (cons (list type data) *error-list*)))
