;;(load "~/lisp/quicklisp/setup.lisp")
(ql:quickload :cl-svg :silent t)
(ql:quickload "split-sequence" :silent t)

(load "graphing")

(defun empty-string-p (string)
  "Predicate for whether a string is empty"
  (string= string ""))

(defun comment-string-p (string)
  "Predicate for if a string is an assembly comment (begins with '#')"
  (char= (char string 0) #\#))

(defun load-data-64 (filename)
  (with-open-file (stream filename
			  :direction :input
			  :element-type '(unsigned-byte 64))
    (loop for n = (read-byte stream nil)
       while n
       collect n)))

(defun load-data-32 (filename)
  (with-open-file (stream filename
			  :direction :input
			  :element-type '(unsigned-byte 32))
    (loop for n = (read-byte stream nil)
       while n
       collect n)))


(defun arrange-data (file-data)
  (let* ((opcode-names '("COLOR" "X" "Y" "PC" "MEM" "IO" "RSTK" "LIT" "ADD" "SUB"
			"PUSH" "POP" "PEEK" "COND" "NOR" "DRAW"))
	 (data (mapcar (lambda (name datapoint)
			 (cons name datapoint))
		       opcode-names file-data)))
    (make-instance 'datablock
		   :data data
		   :named-x T)))
		 
  

(defun create-heatmap (filename)
  (let* ((canvas (make-canvas 1000 800)))
    (svg:title (content canvas) "Opcodes Heatmap")
    (let* ((run-data (arrange-data (load-data-64 "../heatmapOpcodes")))
	   (program-data (arrange-data (load-data-64 "../heatmapUsage"))))
      (setf (data-range-y program-data) (data-range-y run-data))
      (create-scatter-plot run-data canvas
			   :color "red"
			   :y-legend :left)
      (create-scatter-plot program-data canvas
			   :color "blue"
			   :y-legend :right
			   :radius 8))
    (with-open-file (s #p"test.svg" :direction :output :if-exists :supersede)
      (svg:stream-out s (content canvas)))))

;; ====================
;; HTML Program Heatmap
;; ====================

(defun total-mapped-counts (program run-data expansion-map)
  (let ((output (make-list (length-nested program) :initial-element 0))
	(max 0))
    (loop for data in run-data and i from 0 do
	 (when (> data max)
	   (setf max data))
	 ;;(format t "~a ~a~%" (nth (nth i expansion-map) output) (length program))
	 (incf (nth (nth i expansion-map) output) data))
    (format t "~a~%" (length expansion-map))
    (cons max output)))

(defun length-nested (input)
  (let ((total 0))
    (mapcar (lambda (item)
	      (if (listp item)
		  (incf total (length-nested item))
		  (incf total)))
	    input)
    total))

(defun load-hxa (filename)
  (with-open-file (Stream filename :direction :input)
    (loop for line = (read-line stream nil)
       while line
       collect (cond ((or (empty-string-p line) (comment-string-p line)) (list line))
		     (t (split-sequence:split-sequence #\Space line))))))

(defun get-color-string (value)
  (concatenate 'string
	       "rgb("
	       (write-to-string (float (* value 255)))
	       ",0,"
	       (write-to-string (float (- 255 (* value 255))))
	       ")"))

(defun arrange-program (lines heat-package)
  (let ((counter 0)
	(max (car heat-package))
	(heat-data (cdr heat-package)))
	  
    (mapcar (lambda (line)
	      (mapcar (lambda (token)
			(let* ((value (/ (nth counter heat-data) max))
			       (bg-color (get-color-string value)))
			  (cond ((string= token "") (list token "black" nil))
				((char= (char token 0) #\#) (list token "LightGray" nil))
				((member (char token 0) (list #\@ #\>)) (progn (incf counter)
									       (list token "Black" bg-color))) 
				(t (progn (incf counter)
					  (list token "DarkGray" bg-color)))))) ; Token
		      line))
	    lines)))



(defun create-program-heatmap (filename)
  (let* ((stripped-filename (subseq filename 0 (- (length filename) 4)))
	 (run-data-filename (concatenate 'string stripped-filename "-hmProgram.hxo"))
	 (expansion-map-filename (concatenate 'string stripped-filename "-mapping.hxo"))
	 (run-data (load-data-64 run-data-filename))
	 (expansion-map (load-data-32 expansion-map-filename))
	 (program (load-hxa filename))
	 (heat-data (total-mapped-counts program run-data expansion-map))
	 (html-data (arrange-program program heat-data)))

    (with-open-file (Stream (concatenate 'string (subseq filename 0 (- (length filename) 4)) "-heatmap.html")
			    :direction :output
			    :if-exists :supersede)
      (write-line "<body>")
      (write-line "<p style='font-family:Lucida Console, Monaco, monospace;'>" stream)
      (mapcar (lambda (line)
		(mapcar (lambda (token)
			  (write-string (format nil "<span style=\"color:~a" (second token)) stream)
			  (when (third token)
			    (write-string (format nil ";background-color:~a" (third token)) stream))
			  (write-string (format nil "\">~a</span>" (first token)) stream)
			  (write-string " " stream))
			line)
		(write-line "<br>" stream))
	      html-data)
      (write-line "</p></body>" stream))))





  

