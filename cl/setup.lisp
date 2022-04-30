(ql:quickload :parenscript)

(ql:quickload :str)
(ql:quickload :alexandria)
(ql:quickload :rutils)
(ql:quickload :arrows)

(defpackage workspace
  (:use #:cl)
  (:export #:write-file
           #:special-formatted))

(in-package #:cl)
(pushnew :parenscript *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (named-readtables:find-readtable :parenscript)
    (named-readtables:defreadtable :parenscript
      (:merge :standard)
      (:case #.(if (eql :upcase (readtable-case *readtable*))
                   :invert
                   (readtable-case *readtable*))))))

(named-readtables:in-readtable :parenscript)

(defpackage #:parenscript
  (:use #:cl #:anaphora)
  (:nicknames #:ps)
  (:export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiler interface

   ;; compiler
   #:*js-target-version*
   #:ps
   #:*parenscript-stream*
   #:ps-to-stream
   #:ps-doc
   #:ps-doc*
   #:ps*
   #:ps-inline
   #:ps-inline*
   #:*ps-read-function*
   #:ps-compile-file
   #:ps-compile-stream
   ;; for parenscript macro definition within lisp
   #:defpsmacro
   #:defmacro+ps
   #:import-macros-from-lisp
   #:*defined-operators*
   #:*version*

   ;; gensym
   #:ps-gensym
   #:with-ps-gensyms
   #:ps-once-only
   #:maybe-once-only
   #:*ps-gensym-counter*

   ;; naming and namespaces
   #:in-package
   #:use-package
   #:ps-package-prefix
   #:obfuscate-package
   #:unobfuscate-package

   ;; printer
   #:symbol-to-js-string
   #:*js-string-delimiter*
   #:*js-inline-string-delimiter*
   #:*ps-print-pretty*
   #:*indent-num-spaces*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language

   ;; literals
   #:t
   #:nil

   ;; array literals
   #:array
   #:list
   #:aref
   #:elt
   #:make-array
   #:[]

   ;; operators
   ;; logical boolean
   #:not
   #:and
   #:or

   ;; bitwise boolean
   #:logand
   #:logior
   #:logxor
   #:lognot
   #:ash

   #:*
   #:/
   #:rem
   #:mod
   #:+
   #:-
   #:<
   #:>
   #:<=
   #:>=
   #:incf
   #:decf
   #:equal
   #:eql
   #:eq
   #:=

   ;; compile-time stuff
   #:eval-when

   ;; body forms
   #:progn

   ;; if
   #:if
   #:when
   #:unless

   ;; control flow
   #:return
   #:return-from
   #:throw

   ;; assignment and binding
   #:setf
   #:defsetf
   #:psetf
   #:setq
   #:psetq
   #:let*
   #:let

   ;; variables
   #:defvar

   ;; iteration
   #:do
   #:do*
   #:dotimes
   #:dolist
   #:loop

   ;; case
   #:switch
   #:case
   #:default

   ;; function definition
   #:defun
   #:async-defun
   #:lambda
   #:async-lambda
   #:flet
   #:labels

   ;; lambda lists
   #:&key
   #:&rest
   #:&body
   #:&optional
   #:&aux
   #:&environment
   #:&key-object

   ;; macros
   #:macrolet
   #:symbol-macrolet
   #:define-symbol-macro
   #:define-ps-symbol-macro
   #:defmacro

   ;; utils
   #:max
   #:min
   #:floor
   #:ceiling
   #:round
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:pi
   #:sinh
   #:cosh
   #:tanh
   #:asinh
   #:acosh
   #:atanh
   #:1+
   #:1-
   #:abs
   #:evenp
   #:oddp
   #:exp
   #:expt
   #:log
   #:sqrt
   #:random
   #:ignore-errors
   #:concatenate
   #:length
   #:stringp
   #:numberp
   #:functionp
   #:append
   #:apply
   #:destructuring-bind

   ;; js runtime utils
   #:*ps-lisp-library*
   #:mapcar
   #:map-into
   #:map
   #:member
   #:append
   #:set-difference

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-Common Lisp functionality

   ;; DOM accessing utils
   #:inner-html
   #:uri-encode
   #:attribute
   #:offset
   #:scroll
   #:inner
   #:client

   ;; utils
   #:@
   #:chain
   #:defined
   #:undefined
   #:booleanp
   #:objectp
   #:stringify

   ;; html generator for javascript
   #:*ps-html-empty-tag-aware-p*
   #:*ps-html-mode*
   #:ps-html
   #:who-ps-html

   ;; lisp eval
   #:lisp

   ;; js object stuff
   #:delete
   #:typeof
   #:instanceof
   #:new
   #:create

   ;; slot access
   #:with-slots
   #:getprop
   #:in

   ;; literals
   #:regex
   #:this
   #:undefined
   #:{}
   #:false

   ;; iteration
   #:for
   #:for-in
   #:while

   ;; global var
   #:var

   ;; control flow
   #:try
   #:break
   #:continue
   #:await

   ;; jsx
   #:jsx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deprecated functionality

   #:define-script-symbol-macro
   #:gen-js-name
   #:with-unique-js-names
   #:defjsmacro
   #:js-compile
   #:js-inline
   #:js-inline*
   #:js
   #:js*
   #:symbol-to-js
   #:slot-value
   #:compile-script
   #:defmacro/ps
   #:%
   #:==
   #:===
   #:!=
   #:!==
   #:labeled-for
   #:do-set-timeout
   #:concat-string
   #:with
   #:label
   #:f
   #:bind
   #:bind*))


(in-package :parenscript)
; compile to expression
(defun pse* (&rest body)
  (let ((*psw-stream* (or *parenscript-stream* (make-string-output-stream))))
    (parenscript-print (compile-expression `(progn ,@body)) t)
    (unless *parenscript-stream*
      (get-output-stream-string *psw-stream*))))

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
(define-expression-operator lisp-raw (lisp-form)
  `(ps-js:escape
    ,lisp-form))

(defun lisp-raw (x) x)

;; async
(load "/Volumes/EP_1TB/Development/remix-lisp-test/cl/async.lisp")

(ps
  (defun test ()
    (psx (:div
          (:h1 "hello")))))
;; arrows
(import '(rutils:->
          rutils:->>))
(ps:import-macros-from-lisp
  '->
  '->>)

(in-package :workspace)
;; write to file
(defun write-file (name content)
  (with-open-file (stream name
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (write-string content stream)))

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
