(in-package #:tpd2.lib)

(declaim (inline convert-continuation-to-normal-function))
(defun convert-continuation-to-normal-function (k)
  k
  #+use-arnesi-for-continuations (lambda(&optional x) (arnesi:kall k x)))

(cl-cont:defcpstransformer without-call/cc (cons k-expr env) 
  (declare (ignore env))
  `(funcall ,k-expr (locally ,@(cdr cons))))

(defmacro cl-cont-pass-through-one-construct (name)
  `(cl-cont:defcpstransformer ,name (cons k-expr env)
     "Pass this construct through without converting it to call/cc."
     (declare (ignore env))
     `(funcall ,k-expr ,cons)))

(defmacro cl-cont-pass-through-constructs (&rest names)
  `(progn
     ,@(loop for n in names collect
	     `(cl-cont-pass-through-one-construct ,n))))
  
(eval-always
  (cl-cont-pass-through-constructs
   handler-case
   handler-bind
   restart-case
   restart-bind
   
   without-call/cc
   cl-irregsexp::with-match))

#+extra-bugs-please 
(defmacro cl-cont:call/cc (cc)
  "Implements delimited continuations."
  (declare (ignore cc))
  (error "Please ensure CALL/CC is called from within WITH-CALL/CC macro."))


