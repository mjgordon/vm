(in-package :compiler)

;;;; Functions pertaining to loading and saving files, and dealing with filenames

(defparameter *source-file* nil)

(defun load-file (filename)
  "Loads a file on disk into a list of strings"
  (with-open-file (stream filename)
    (let ((lines (loop for line = (read-line stream nil)
		    while line
		    collect (concatenate 'string line " "))))
      (setf *source-file* lines))))

(defun get-reader (&optional (strip-predicates ()))
  "Returns a function that when called returns the next line and associated line number"
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
		    
		 
       
	    
