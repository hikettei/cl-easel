
#+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
(asdf:defsystem :cl-easel
  :description "A library dedicated to 2d rendering on terminal"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     ""
  ;; :in-order-to ((test-op
  :depends-on
  ("cl-easel.core"
   "cl-easel.canvas"))

