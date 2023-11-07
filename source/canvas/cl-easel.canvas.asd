
(asdf:defsystem :cl-easel.canvas
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on (:petalisp
	       :trivia
	       :mgl-pax
	       :alexandria
	       "cl-easel.core")
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "histogram")
   ))


