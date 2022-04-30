(ql:quickload :parenscript)
(ql:quickload :paren6)

(ql:quickload :str)
(ql:quickload :cl21)
(ql:quickload :arrows)


(in-package :cl21-user)
(defpackage workspace 
  (:export #:write-file
           #:special-formatted))

(defpackage #:paren6
  (:use #:cl
        #:parenscript
        #:alexandria)
  (:shadowing-import-from #:parenscript #:switch)
  (:export
   #:export
   #:export-default
   #:import
   #:list6
   #:create6
   #:=>
   #:defclass6
   #:defconstant6
   #:super
   #:import-into
   #:for-of))

;; jsx
(in-package :parenscript)
(defmethod ps-print ((string string))
  (let ((jsx? (and (str:starts-with? "<" string) (str:ends-with? ">" string))))
      (unless jsx? (psw *js-string-delimiter*))
      (loop for char across string do
       (acond ((getf *js-lisp-escaped-chars* char)
               (psw #\\ it))
              ((or (<= (char-code char) #x1F)
                   (<= #x80 (char-code char) #x9F)
                   (member (char-code char) '(#xA0 #xAD #x200B #x200C)))
               (format *psw-stream* "\\u~:@(~4,'0x~)" (char-code char)))
              (t
               (psw char))))
      (unless jsx? (psw *js-string-delimiter*))))

; compile to expression
(defun pse* (&rest body)
  (let ((*psw-stream* (or *parenscript-stream* (make-string-output-stream))))
    (parenscript-print (compile-expression `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

(defun formatted-jsx (body)
  (format nil "{~A}" (apply #'pse* body)))

(defun process-html-forms-cl-who (forms)
  (let ((r ()))
    (labels ((process-form (form)
               (cond ((keywordp form) (process-form (list form)))
                     ((atom form) (push form r))
                     ; jsx
                     ((and (consp form) (eq (car form) :jsx))
                      (let ((body (cdr form)))
                        (push (formatted-jsx body) r)))
                     ((and (consp form) (keywordp (car form)))
                      (push (format nil "<~A" (symbol-to-js-string (car form))) r) ; use same casing as js var
                      (labels ((process-attributes (el-body)
                                 (when el-body
                                   (if (keywordp (car el-body))
                                       (if (consp (cadr el-body))
                                           (progn
                                            (push (format nil " ~A=" (symbol-to-js-string (car el-body))) r)
                                            (push (formatted-jsx (cadr el-body)) r)
                                            (process-attributes (cddr el-body)))
                                           (progn
                                            (push (format nil " ~A=\""
                                                          (symbol-to-js-string (car el-body))) r)
                                            (push (cadr el-body) r)
                                            (push "\"" r)
                                            (process-attributes (cddr el-body))))
                                      el-body))))
                       (let ((content (process-attributes (cdr form))))
                         (if (or content (not (empty-tag-p (car form))))
                             (progn (push ">" r)
                                    (when content (map nil #'process-form content))
                                    (push (format nil "</~A>" (symbol-to-js-string (car form))) r))
                             (progn (when (eql *ps-html-mode* :xml)
                                      (push "/" r))
                                    (push ">" r))))))
                     (t (push form r)))))
      (map nil #'process-form forms)
      (concat-constant-strings (reverse r)))))


(in-package :workspace)
;; write to file
(defun write-file (name content)
  (with-open-file (stream name
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream content)))

(defun special-formatted (str)
  (str:replace-all "\\n" "" 
    (str:replace-all "\\'" "'" str)))


(in-package :paren6)
;; async
(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/async.lisp")
           
;; arrows
(ps:import-macros-from-lisp
  '->
  '->>)

