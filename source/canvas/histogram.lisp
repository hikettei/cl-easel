
(in-package :cl-easel.canvas)

(defparameter
    *vertical-histogram-symbols*
  `("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"))

(defparameter
    *horizontal-histogram-symbols*
  `("▏" "▎" "▍" "▌" "▋" "▊" "▉" "█"))

;; [TODO] Horizontal

(defun histogram (array
		  &key
		    (direction :vertical)
		    (nbins     10))
  (declare (type array array))
  (let* ((dropped-array
	   (progn
	     #+sbcl(sb-ext:array-storage-vector array)
	     #-sbcl(make-array (apply #'* (array-dimensions array))
			       :element-type (array-element-type array)
			       :displaced-to array)))
	 (len (length dropped-array)))
    
    dropped-array))

(print (histogram #2A((1 2 3) (4 5 6))))

