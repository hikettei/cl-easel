
(cl:in-package :cl-user)

(defpackage :cl-easel.core
  (:nicknames :cl-easel)
  (:use :cl :petalisp)
  (:export
   #:frame-size-t
   #:Easel
   #:realize
   #:with-easel
   #:easel-t
   #:easel-now
   #:easel-width
   #:easel-height
   #:easel-frame
   #:with-fixed-time
   )
  (:export
   #:curve-text))

