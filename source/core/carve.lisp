
(in-package :cl-easel.core)

(defun carve-text (easel sentence &optional (from-height 0) (from-width 0) (direction :horizontal))
  "Carves the given sentence (a type of sequence) into easel."
  (declare (type easel easel))
  (let* ((from-height (parse-absolute (easel-height easel) from-height))
	 (from-width  (parse-absolute (easel-width easel)  from-width))
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

