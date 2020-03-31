(in-package :compiler)

(defparameter *source-file* nil)

(defun load-file (filename)
  (with-open-file (stream filename)
    (let ((lines (loop for line = (read-line stream nil)
		    while line
		    collect (concatenate 'string line " "))))
      (setf *source-file* lines))))

(defun get-reader (&optional (strip-predicates ()))
  (let ((lines (copy-list *source-file*))
	(line-counter 0))
    (lambda ()
      (let ((line nil)
	    (line-number nil)
	    (flag t))
	(loop while flag do
	     (setf line (pop lines))
	     (setf line-number (incf line-counter))
	     (setf flag (some (lambda (item)
				(not (null item)))
			      (mapcar (lambda (fun)
					(funcall fun line))
				      strip-predicates))))
	(values line line-number)))))
		    
		 
       
	    
