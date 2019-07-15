(defpackage :compiler-tests
  (:use :cl :asdf :prove))

(in-package :compiler-tests)

(defun get-filetype (filename)
  (subseq filename (- (length filename) 3 )))

(defun is-hxc-file (filename)
  (string-equal (get-filetype filename) "hxc"))

(defun test-files (folder &optional (valid t))
  "Arguments are folder to look for single-file programs in, and whether the program is valid or not"
  (setf prove:*default-reporter* :fiveam)
  (let ((filenames (directory (concatenate 'string folder "/*.hxc"))))
    (plan (length filenames))
    (loop for filename in filenames do
	 (is (compiler:compile-hxc (format nil "~a" filename)) (not valid) (file-namestring filename)))
    (prove:finalize)))




  
  
