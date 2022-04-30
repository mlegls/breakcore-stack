(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/setup.lisp")

(in-package :cl-user)
(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/collect.lisp")

(in-package :paren6)


;; WORK UNDER HERE

(workspace:write-file "/Volumes/EP_1TB/Development/remix-lisp-test/js/remix/app/routes/index.jsx" 
 (workspace:special-formatted
   (ps
    (defun *index ()
      (who-ps-html (:button :on-click (:jsx (=> () (alert "Hello!!!") (nil))) 
                    (:jsx (+ "Click " "me! again!")))))
    (export-default *index))))


(ps 
 (try (throw "i")
      (:catch (error)
       (chain console (log error)))))
        
  
    
   

