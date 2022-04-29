(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/setup.lisp")


(in-package :paren6)

(writePs "routes/index.jsx"
 ;ps
  (defun *index ()
    (who-ps-html (:div "hello!")))
  (export-default *index))
 

