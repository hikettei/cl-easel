
(asdf:defsystem :cl-easel.core
  :author      "hikettei <ichndm@gmail.com>"
  :depends-on (:petalisp
	       :trivia
	       :mgl-pax
	       :alexandria)
  :serial t
  :components
  ((:file "package")
   (:file "easel")
   (:file "drawline")
   (:file "curve")
   ))

