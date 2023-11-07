
(in-package :cl-easel.canvas)

(defparameter
    *vertical-histogram-symbols*
  `("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"))

(defparameter
    *horizontal-histogram-symbols*
  `("▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"))

(defmethod render-bar ((direction (eql :horizontal))
		       count
		       max
		       plot-size)
  (assert (= (length *vertical-histogram-symbols*) 8)
	  ()
	  "Assertion Failed: The length of parameter *vertical-histogram-symbols* should be set to 8.")
  (let ((ratio (rationalize (* plot-size (/ count max)))))
    (with-output-to-string (out)
      (multiple-value-bind (real float) (round ratio)
	(dotimes (i real)
	  (format out "~a" (car (last *horizontal-histogram-symbols*))))
	(format
	 out
	 (nth (round (abs (* float 8))) *horizontal-histogram-symbols*))))))

(defmethod render-bar ((direction (eql :vertical))
		       count
		       max
		       plot-size)
  (let* ((*horizontal-histogram-symbols* *vertical-histogram-symbols*)
	 (result (render-bar :horizontal count max plot-size)))
    (lazy-reshape result (transform i j to j i))))

(defun fit-histogram (array nbins)
  (let* ((max        (compute (lazy-reduce #'max array)))
	 (min        (compute (lazy-reduce #'min array)))
	 (range-step (/ (abs (- min max)) nbins))
	 ;; (A, B]
	 (ranges     (loop for A
			   upfrom min
			     below max
			   by range-step
			   collect
			   `(,(float A)
			     ,(float (+ A range-step)))))
	 (plots (loop for range in ranges
		      collect
		      0)))
    ;; Plots:
    ;; AList where
    ;;  Range -> Count
    (compute
     (lazy
      #'(lambda (element)
	  (let ((nrange (position element (reverse ranges)
				  :key #'car
				  :test #'>=)))
	    (incf (nth nrange plots))))
      array))
    (setq plots (reverse plots))
    
    (values plots ranges max min)))

;; [TODO] Horizontal
(defun histogram (easel
		  array
		  &key
		    (direction :vertical)
		    (nbins
		     (min
		      10
		      (lazy-array-size (lazy-array array))))
		    (digits 4)
		    (title nil))
  (declare (type array array)
	   (type Easel easel))
  (let* ((x-plot
	   (lazy-array
	    #+sbcl(sb-ext:array-storage-vector array)
	    #-sbcl(make-array (apply #'* (array-dimensions array))
			      :element-type (array-element-type array)
			      :displaced-to array)))
	 (len (lazy-array-size x-plot))
	 (Σ (compute (lazy-reduce #'+ x-plot)))
	 (μ (float (/ Σ len)))
	 (σ (sqrt
	     (/
	      (expt
	       (compute
		(lazy-reduce #'+ (lazy #'- x-plot μ)))
	       2)
	      len)))
	 (report   (format nil "μ±σ:~a±~a" (lpad μ digits) (lpad σ digits)))
	 (report-n (length report)))
    (with-slots ((width width) (height height) (frame frame)) easel
      (case direction
	(:vertical
	 (draw-horizontal! easel -2)
	 (carve-text easel report -1 (round (- (/ width 2) (/ report-n 2) 1))))
	(:horizontal))
      (multiple-value-bind (bars ranges max min) (fit-histogram x-plot nbins)
	(case direction
	  (:vertical
	   )
	  (:horizontal
	   ;; [TODO]
	   (let ((align-to 0))
	     (loop for i upfrom 2
		   for r in ranges do
		     (let ((content			     
			     (format
			      nil
			      "(~a, ~a]"
			      (lpad (first r)  digits)
			      (lpad (second r) digits))))
		       (setq align-to (max align-to (length content)))
		       (carve-text easel content i)))

	     (when title
	       (carve-text easel title 1 (+ 2 align-to)))
	     (draw-vertical! easel align-to)
	     (draw-horizontal! easel 0  align-to)
   	     (draw-horizontal! easel -1 align-to)

	     (loop with max = (apply #'max bars)
		   for N     upfrom 2
		   for r     in ranges
		   for count in  bars do
		     (carve-text
		      easel
		      (format
		       nil
		       "~a ~a"
		       (render-bar :horizontal count max (/ width 2))
		       count)
		      N
		      (1+ align-to))))))))))
#|
(load "../waffe2-develop-latest/cl-waffe2/cl-waffe2.asd")
(ql:quickload :cl-waffe2)
(with-easel (x (1 14 49))
  (histogram
   x
   (wf/t:tensor-vec (wf/d:beta `(10000) 2 2))
   :direction :vertical :title "[Be(2, 2)]")
  (realize x)
  (print x))
|#

