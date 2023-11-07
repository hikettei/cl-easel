
(in-package :cl-easel.core)

;; Ref: https://github.com/xyzzy-022/xyzzy-unicode/blob/master/xyzzy/lisp/cnamedef.l
;; Ref: https://en.wikipedia.org/wiki/Box-drawing_character
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter
      *materials*
    (alexandria:alist-hash-table
     `((:horizontal       . #\─)
       (:vertical         . #\│)
       (:top-and-left     . #\┌)
       (:top-and-right    . #\┐)
       (:down-and-right   . #\┘)
       (:down-and-left    . #\└)
       (:top-and-down-and-right . #\├)
       (:top-and-down-and-left  . #\┤)
       (:down-and-right-and-left . #\┬)
       (:top-and-right-and-left  . #\┴)       
       (:cross      . #\┼))))

  (defun from-materials (name)
    (or (gethash name *materials*)
	(error "from-materials: unknown keyword: ~a" name)))

  (defparameter
      *materials->keyword*
    (alexandria:alist-hash-table
     #.`(loop for key being the hash-keys in *materials*
	      collect `(,(from-materials key) . ,key))
     :test
     #'equal))

  (defun maybe-to-material (value)
    (or (gethash value *materials->keyword*) value)))

;; [TODO] with-table-style

(defun make-material+ (easel)
  (with-slots ((width width) (height height) (now now) (frame frame)) easel
    #'(lambda (index-x index-y from to
	       &aux
		 (width (1- width))
		 (height (1- height)))
	(labels ((merge-materials (from1 to1)
		   (trivia:ematch (list from1 to1)
		     ;; If crossed:
		     ((list :horizontal :vertical)
		      ;; draw-row!
		      ;; - + | -> +
		      (from-materials :cross))
		     ((list :vertical :horizontal)
		      (from-materials :cross))
		     ((list (or :vertical :horizontal) :cross)			 
		      (cond
			((and (= index-x height)
			      (= index-y width))
			 (from-materials :down-and-right))
			((and (= index-x 0)
			      (= index-y width))
			 (from-materials :top-and-right))
			((and (= index-x 0)
			      (= index-y 0))
			 (from-materials :top-and-left))
			((and (= index-x height)
			      (= index-y 0))
			 (from-materials :down-and-left))
			((= index-x 0)
			 (from-materials :down-and-right-and-left))
			((= index-x height)
			 (from-materials :top-and-right-and-left))
			((= index-y 0)
			 (from-materials :top-and-down-and-right))
			((= index-y width)
			 (from-materials :top-and-down-and-left))
			(T
			 (from-materials :cross))))
		     ((list _ _)
		      to))))
	  ;; also merge-materials XXX
	  (merge-materials
	   (maybe-to-material from)
	   (maybe-to-material
	    (merge-materials
	     (maybe-to-material from)
	     (maybe-to-material to))))))))

(defun clean-up-crossing-points (easel frame)
  (let ((actual-frame (compute frame))
	(now          (easel-now easel)))
    (loop for height upfrom 1 below (1- (easel-height easel)) do
      (loop for width upfrom 1 below (1- (easel-width easel)) do
	;; Creating a small window to determine the direction of cross
	;; XNX  X ... nothings to do with
	;; NON  O ... the very processing element
	;; XNX  N ... something to do with
	(when (eql (maybe-to-material (aref actual-frame now height width)) :cross)
	  (setf
	   (aref actual-frame now height width)
	   (let* ((N1
		    (maybe-to-material
		     (aref
		      actual-frame
		      now
		      (1+ height)
		      width)))
		  (N2 (maybe-to-material
		       (aref
			actual-frame
			now
			(1- height)
			width)))
		  (N3 (maybe-to-material
		       (aref
			actual-frame
			now
			height
			(1- width))))
		  (N4 (maybe-to-material
		       (aref
			actual-frame
			now
			height
			(1+ width)))))
	     
	     (trivia:ematch (list (keywordp N1)
				  (keywordp N2)
				  (keywordp N3)
				  (keywordp N4))
	       ;; TOP DOWN LEFT RIGHT
	       ;;       TOP
	       ;;    LEFT RIGHT
	       ;;       DOWN
	       ;; 2 2 2 2 = 16

	       ((list NIL NIL NIL NIL)
		(from-materials :cross))
	       ((list T NIL NIL NIL)
		(from-materials :vertical))
	       ((list NIL T NIL NIL)
		(from-materials :vertical))
	       ((list NIL NIL T NIL)
		(from-materials :horizontal))
	       ((list NIL NIL NIL T)
		(from-materials :horizontal))
	       ((list T T NIL NIL)
		(from-materials :vertical))
	       ((list T NIL T NIL)
		(from-materials :top-and-right))
	       ((list T NIL NIL T)
		(from-materials :top-and-left))
	       ((list NIL T NIL T)
		(from-materials :down-and-left))
	       ((list NIL T T NIL)
		(from-materials :down-and-right))	      
	       ((list T T T NIL)
		(from-materials :top-and-down-and-left))
	       ((list T T NIL T)
		(from-materials :top-and-down-and-right))
	       ((list T NIL T T)
		(from-materials :down-and-right-and-left))
	       ((list NIL T T T)
		(from-materials :top-and-right-and-left))
	       ((list T T T T)
		(from-materials :cross))	       
	       (_ #\?)))))))
    (lazy-array actual-frame)))

(defun draw-horizontal! (easel index &optional (from 0) (to (easel-width easel)))
  (declare (easel easel))
  
  (let ((index (if (< index 0)
		   (+ index (easel-height easel))
		   index))
	(to    (if (< to 0)
		   (+ to (easel-height easel))
		   to))
	(from  (if (< from 0)
		   (+ from (easel-height easel))
		   from)))
    (symbol-macrolet ((frame (easel-frame easel)))
      (setf frame
	    (lazy
	     (make-material+ easel)
	     (lazy-index-components
	      (lazy-array-shape frame)
	      1)
	     (lazy-index-components
	      (lazy-array-shape frame)
	      2)
	     frame
	     (lazy-overwrite
	      frame
	      (lazy-reshape
	       (from-materials :horizontal)
	       (~
		(easel-t easel)
		~
		index (1+ index)
		~
		from to)))))
      (setf frame (clean-up-crossing-points easel frame)))))

(defun draw-vertical! (easel index &optional (from 0) (to (easel-height easel)))
  (declare (easel easel))
  (let ((index (if (< index 0)
		   (+ index (easel-width easel))
		   index))
	(to    (if (< to 0)
		   (+ to (easel-width easel))
		   to))
	(from  (if (< from 0)
		   (+ from (easel-width easel))
		   from)))
    (symbol-macrolet ((frame (easel-frame easel)))
      (setf frame
	    (lazy
	     (make-material+ easel)
	     (lazy-index-components
	      (lazy-array-shape frame)
	      1)
	     (lazy-index-components
	      (lazy-array-shape frame)
	      2)
	     frame
	     (lazy-overwrite
	      frame
	      (lazy-reshape
	       (from-materials :vertical)
	       (~
		(easel-t easel)
		~
		from to
		~
		index (1+ index))))))
      (setf frame (clean-up-crossing-points easel frame)))))

