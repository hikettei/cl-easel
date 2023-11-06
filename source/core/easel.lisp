
(in-package :cl-easel.core)

(deftype frame-size-t ()
  `(unsigned-byte 16))

;; [TODO] Adjustable Size depending on elements displayed
;; [TODO] Animation
;; [TODO] Color
(defclass Easel ()
  ((t-size  :initarg :t :initform 1 :type frame-size-t :reader    easel-t)

   (now     :initform 0      :type frame-size-t        :accessor easel-now)
   (width   :initarg :width  :type frame-size-t        :reader   easel-width)
   (height  :initarg :height :type frame-size-t        :reader   easel-height)

   (frame   :accessor easel-frame))
  (:documentation "
[class] Easel
Time x Width x Height
 /---/
┌───┐ |
│   │ |
│   │ |
└───┘/
"))

(defmethod initialize-instance :after ((easel Easel) &rest args)
  (declare (ignore args))
  (setf (easel-frame easel)
	;; (Time Height Weight)
	(lazy-reshape #\Space
		      (~
		       (easel-t easel)
		       ~
		       (easel-height easel)
		       ~
		       (easel-width  easel)))))

(defmethod easel-output-to-string ((easel Easel))
  (with-output-to-string (out)
    (dotimes (h (easel-height easel))
      (dotimes (w (easel-width easel))
	(format out "~a" (aref (easel-frame easel)
			       (easel-now easel)
			       h
			       w)))
      (format out "~%"))))

(defmethod print-object ((easel Easel) stream)
  (typecase (easel-frame easel)
    (lazy-array
     (format stream "Easel[status=:on-going t=~a]{~a}"
	     (easel-now easel)
	     (easel-frame easel)))
    (T
     (format stream "~a" (easel-output-to-string easel)))))

(defmethod realize ((easel Easel))
  "[method] realize"
  (when (lazy-array-p (easel-frame easel))
    (setf (easel-frame easel) (compute (easel-frame easel))))
  easel)

(defmacro with-easel ((bind (&optional (time 1) (height 10) (width 10)))
		      &body body)
  "[macro] with-easel"
  `(let ((,bind (make-instance 'Easel
			       :t      ,time
			       :width  ,width
			       :height ,height)))
     (prog1 ,@body (realize ,bind))))

(defmacro with-fixed-time ((bind easel time-at) &body body)
  ""
  (let ((placeholder (gensym)))
    `(let ((,placeholder (easel-now ,easel)))
       (unwind-protect
	    (let ((,bind ,easel))
	      (setf (easel-now ,bind) ,time-at)
	      ,@body)
	 (setf (easel-now ,easel) ,placeholder)))))

