(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/setup.lisp")


(in-package :paren6)

(writeFile "/Volumes/EP_1TB/Development/remix-lisp-test/js/remix/app/routes/index.jsx" 
 (special-formatted
   (ps
    (defun *index ()
      (who-ps-html (:button :on-click (:jsx (=> () (alert "Hello!!!") (nil))) 
                    (:jsx (+ "Click " "me!")))))
    (export-default *index))))

