(ql:quickload :parenscript)

(ql:quickload :str)
(ql:quickload :alexandria)
(ql:quickload :rutils)
(ql:quickload :arrows)

(defpackage #:workspace
  (:use #:cl #:parenscript)
  (:export #:write-file #:make-js))

(in-package :parenscript)
; compile to expression
(defun pse* (&rest body)
  (let ((*psw-stream* (or *parenscript-stream* (make-string-output-stream))))
    (parenscript-print (compile-expression `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

; TODO fix __PS_MV_REG
(defpsmacro multiple-value-bind (vars form &body body)
  (let* ((form       (ps-macroexpand form))
         (progn-form (when (and (consp form)
                                (member (car form)
                                        '(with label let flet labels
                                           macrolet symbol-macrolet progn)))
                       (pop form))))
    (if progn-form
        `(,progn-form
           ,@(butlast form)
           (multiple-value-bind ,vars
               ,@(last form)
             ,@body))
        ;; assume function call
        `(progn
           (var __PS_MV_REG '())
           (let ((,(car vars) ,form))
             (destructuring-bind (&optional ,@(cdr vars))
                 __PS_MV_REG
               ,@body))))))

(defpsmacro multiple-value-list (form)
  (with-ps-gensyms (first-value values-list)
    `(let* ((,first-value (progn
                            (var __PS_MV_REG '())
                            ,form))
            (,values-list (funcall (getprop __PS_MV_REG 'slice))))
       (funcall (getprop ,values-list 'unshift) ,first-value)
       ,values-list)))

; jsx instead of html
(defun formatted-jsx (body)
  (format nil "{~A}" (apply #'pse* body)))

(defun process-html-forms-cl-who (forms)
  (let ((r ()))
    (labels ((process-form (form)
               (cond ((keywordp form) (process-form (list form)))
                     ((atom form) (push form r))
                     ; psx inline ps
                     ((and (consp form) (eq (car form) :ps))
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

;; jsx
(define-expression-operator psx (&rest html-forms)
  `(ps-js::psx ,@html-forms))

(defprinter ps-js::psx (&rest html-forms)
  (apply #'psw (with-standard-io-syntax (process-html-forms-cl-who html-forms))))

;; literal
(define-expression-operator raw (lisp-form)
  `(ps-js:escape
     ,lisp-form))

(defun lisp-raw (x) x)

;; const and let
(define-statement-operator defconstant (form val)
  `(ps-js::defconstant ,(compile-expression form) ,(compile-expression val)))

(defprinter ps-js::defconstant (form val)
  (psw "const ") (ps-print form)
  (psw " = ") (ps-print val))

(define-statement-operator jlet (form &optional val)
  `(ps-js::jlet ,(compile-expression form) ,(compile-expression val)))

(defprinter ps-js::jlet (form &optional (val))
  (psw "let ") (ps-print form)
  (when val (psw " = ") (ps-print val)))

;; destructured object
(define-expression-operator dobj (&rest symbols)
  `(ps-js::dobj ,(mapcar #'compile-expression symbols)))

(defprinter ps-js::dobj (symbols)
  "{ "(print-comma-delimited-list symbols)" }")

;; flat destructured symbols
(define-expression-operator flat (&rest symbols)
  `(ps-js::flat ,symbols))

(defprinter ps-js::flat (symbols)
  (print-comma-delimited-list symbols))

;; arrow functions
(defun print-=> (args body)
    (psw "(")
    (loop for (arg . remaining) on args do
        (psw (symbol-to-js-string arg)) (when remaining (psw ", ")))
    (psw ") => ")
    (ps-print body))

(ps::define-expression-operator => (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (ps::parse-extended-function lambda-list body)
    `(ps-js::=> ,effective-args
       ,(let ((ps::*function-block-names* ()))
          (ps::compile-function-body effective-args effective-body)))))

(ps::defprinter ps-js::=> (args body-block)
  (ps::print-=> args body-block))

(ps::define-expression-operator async-=> (lambda-list &rest body)
  (multiple-value-bind (effective-args effective-body)
      (ps::parse-extended-function lambda-list body)
    `(ps-js::async-=> ,effective-args
       ,(let ((ps::*function-block-names* ()))
          (ps::compile-function-body effective-args effective-body)))))

(ps::defprinter ps-js::async-=> (args body-block)
  (ps::psw "async ")
  (ps::print-=> args body-block))
         
;; async
(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/async.lisp")

;; arrows
(import '(rutils:->
           rutils:->>))
(ps:import-macros-from-lisp
  '->
  '->>)

;; import, export 
(define-expression-operator as (arg1 arg2)
  `(ps-js::as ,(compile-expression arg1) ,(compile-expression arg2)))

(defprinter ps-js::as (arg1 arg2)
  (ps-print arg1)
  (psw " as ")
  (ps-print arg2))

(define-statement-operator esm-import (name &optional source)
  `(ps-js::esm-import ,(compile-expression name) ,(compile-expression source)))

(defprinter ps-js::esm-import (name &optional source)
  (psw "import ")
  (ps-print name)
  (when source (psw " from ") (ps-print source)))

(define-statement-operator esm-export (name &optional default?)
  `(ps-js::esm-export ,(compile-expression name) ,(if (eq default? :default)
                                                      t nil)))

(defprinter ps-js::esm-export (name &optional default?)
  (psw "export ")
  (when default? (psw "default "))
  (ps-print name))

;; macros
(defpsmacro ps-compat ()
  `(var *__PS_MV_REG*))

(defpsmacro import-many (&rest modules)
  (cons 'progn
   (mapcar (lambda (module) (if (consp module)
                                (cons 'esm-import module)
                                (list 'esm-import module)))
           modules)))

;; remix macros
(defpsmacro export-remix (default &optional options)
  (cons 'progn
   (list (when default `(esm-export ,default :default))
         (case options
           (:l '(esm-export (dobj loader)))
           (:a '(esm-export (dobj action)))
           (:la '(esm-export (dobj loader action)))))))
     
(defpsmacro defaction (&rest body)
  `(async-defun action (req)
                (ps-compat)
                (defconstant request (@ req request))
                (defconstant body (await (chain request (form-data))))
                ,@body))

(defpsmacro defloader (&rest body)
  `(async-defun loader (req)
                (ps-compat)
                (defconstant request (@ req request))
                ,@body))

;; utils
(in-package :workspace)
;; write to file
(defun write-file (name content)
  (with-open-file (stream name
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string content stream)))

(defmacro make-js (path &rest body)
  `(workspace::write-file ,(format nil "~{~A~}" 
                                   (list "/Volumes/EP_1TB/Development/common-ai/solution-market/app-cl/js/remix/app/" path))
                          (ps::ps ,@body)))

;; editing namespace
(ql:quickload :paren6)
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
