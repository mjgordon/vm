;;; GRAPHING FUNCTIONS

(defun create-scatter-plot (datablock canvas &key color (y-legend :left) (radius 10))
  ;; X Axis Legend and datapoints
  (let ((cv (content canvas))
	(grx (graph-range-x canvas))
	(gry (graph-range-y canvas))
	(drx (data-range-x datablock))
	(dry (data-range-y datablock)))
    (loop for datapoint in (data datablock) and i from 0 do
	 (let* ((y-value (cdr datapoint))
		(name (car datapoint))
		(x (lerp i '(0 15) grx))
		(y (invert-in-range (lerp y-value dry gry) gry)))
	   (svg:draw cv (:ellipse :cx x :cy y :rx radius :ry radius) :fill color :stroke "none")
	   (svg:transform ((svg:rotate 45 (- x 10) (+ (second gry) 50)))
	     (svg:text cv (:x (- x 10) :y (+ (second gry) 50)) name))))
    ;; Y Axis Legend
    (let* ((tick-pow (expt 10 (floor (log (second dry) 10))))
	   (max-tick (* tick-pow (floor (/ (second dry) tick-pow))))
	   (tick (/ max-tick 10))
	   (x (cond ((equal y-legend :left) 60)
		    ((equal y-legend :right) (- (width canvas) 50))
		    (t 60)))
	   (anchor (cond ((equal y-legend :right) "start")
			 (t "end"))))
      (loop for y from 0 to max-tick by tick do
	   (svg:text cv
	       (:x x :y (invert-in-range (lerp y dry gry) gry) :text-anchor anchor :dominant-baseline "middle")
	     (write-to-string y))))))

;;; CLASSES

(defun make-canvas (width height)
  (make-instance 'canvas
		 :content (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
						  :width width :height height
						  :viewbox (format nil "~{~a ~}" (list 0 0 width height)))
		 :width width
		 :height height
		 :graph-range-x (list 100 (- width 100))
		 :graph-range-y (list 100 (- height 100))))						  

(defclass canvas ()
  ((content :accessor content :initarg :content)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (graph-range-x :accessor graph-range-x :initarg :graph-range-x)
   (graph-range-y :accessor graph-range-y :initarg :graph-range-y)))

(defclass datablock ()
  ((data :accessor data :initarg :data)
   (named-x :accessor named-x :initarg :named-x :initform nil)
   (data-range-x :accessor data-range-x :initform (list 0 0))
   (data-range-y :accessor data-range-y :initform (list 0 0))))

(defmethod initialize-instance :after ((this datablock) &key)
  ;;(format t "~a X : ~a~%" this (data-range-x this))
  ;;(format t "~a Y : ~a~%" this (data-range-y this))
  (mapcar (lambda (datapoint)
	    ;;(format t "~a~%" datapoint)
	    (update-range (cdr datapoint) (data-range-y this))
	    (unless (named-x this)
	      (update-range (car datapoint) (data-range-x this))))
	  (data this))
  this)
	  

;;; HELPER FUNCTIONS

(defun range-size (range)
  (- (second range) (first range)))

(defun lerp (value input-range output-range)
  (let ((normalized-value (/ (- value (first input-range)) (range-size input-range))))
    (float (+ (first output-range) (* normalized-value (range-size output-range))))))

(defun invert-in-range (value range)
  (- (second range) (- value (first range))))

(defun update-range (value range)
    (when (< value (first range))
      (setf (first range) value))
    (when (> value (second range))
      (setf (second range) value)))
