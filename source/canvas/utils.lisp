
(in-package :cl-easel.canvas)

(defun lpad (sentence digits &aux (sentence (format nil "~a" sentence)))
  "This function converts the given object as sentence into stringm trimming the given sentence to fit the given `digits`."
  (declare (type unsigned-byte digits))
  (let ((sentence-n (length sentence)))
    (if (>= sentence-n digits)
	(subseq sentence 0 digits)
	sentence)))

