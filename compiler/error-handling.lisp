(in-package :compiler)

(defparameter *error-list* ())

(defun get-error-type (token)
  (cond ((equal token 'close-paren) 'error-missing-paren)
	((equal token 'close-brace) 'error-missing-close-brace)
	((equal token 'semicolon) 'error-missing-semicolon)))

(defun clear-error-list ()
  (setf *error-list* ()))

(defun log-error (type data)
  (setf *error-list* (cons (list type data) *error-list*)))
