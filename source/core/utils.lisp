
(in-package :cl-easel.core)

(declaim (inline parse-absolute)
	 (ftype (function (fixnum fixnum) fixnum) parse-absolute))
(defun parse-absolute (total-size index)
  "Parses absoluted index considering total-size"
  (declare (type fixnum total-size index))
  (if (minusp index)
      (+ total-size index)
      index))

