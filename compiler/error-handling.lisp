(in-package :compiler)

(defparameter *error-list* ())

(defun clear-error-list ()
  (setf *error-list* ()))

(defun log-error (type data)
  (setf *error-list* (cons (list type data) *error-list*)))
