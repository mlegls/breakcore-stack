(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/setup.lisp")
(in-package :paren6)

;; WORK UNDER HERE

(workspace::write-file "/Volumes/EP_1TB/Development/remix-lisp-test/js/remix/app/routes/index.jsx"
 ;write-string
   (ps
     ;; body

     (ps::lisp-raw "import { Button, Alert } from 'antd'")
     (defun *index ()
        (ps::psx (:div (:*button :on-click (:ps (=> () (alert "Hello!!!") (nil))) :type "primary" "Click Me!")
                     (:*alert :message "Hello from the Remix Breakcore Stack!" :type "success"))))
     (export-default *index)))


(ps 
 (try (throw "i")
      (:catch (error)
       (chain console (log error)))))
        
  
    
   

