
(in-package :cl-easel.core)

(defun curve-text (easel sentence &optional (from-height 0) (from-width 0) (direction :horizontal))
  ""
  (declare (type easel easel)
	   (type string sentence))
  (let* ((from-height (if (< from-height 0)
			  (+ from-height (easel-height easel))
			  from-height))
	 (from-width  (if (< from-width 0)
			  (+ from-width (easel-width easel))
			  from-width))
	 (size (length sentence))
	 (sentence (lazy-reshape
		    (lazy-array sentence)
		    (~
		     (easel-t easel)
		     ~
		     from-height (1+ from-height)
		     ~
		     from-width (+ from-width size))))
	 (sentence (ecase direction
		     (:horizontal sentence)
		     (:vertical  (lazy-reshape sentence (transform i j k to i k j))))))
    
    (symbol-macrolet ((frame (easel-frame easel)))  
      (setf frame
	    (lazy-overwrite
	     frame
	     sentence)))))

