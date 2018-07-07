;;(load "~/lisp/quicklisp/setup.lisp")
(ql:quickload :cl-svg :silent t)

(load "graphing")

(defun load-heatmap-data (filename)
  (with-open-file (stream filename
			  :direction :input
			  :element-type '(unsigned-byte 64))
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
		 
  

(defun create-heatmap ()
  (let* ((canvas (make-canvas 1000 800)))
    (svg:title (content canvas) "Opcodes Heatmap")
    (let* ((run-data (arrange-data (load-heatmap-data "../heatmapOpcodes")))
	   (program-data (arrange-data (load-heatmap-data "../heatmapUsage"))))
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






  

