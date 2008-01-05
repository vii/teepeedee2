(in-package #:tpd2.lib)

(defun convert-continuation-to-normal-function (k)
  k
  #+use-arnesi-for-continuations (lambda(&optional x) (arnesi:kall k x)))

(cl-cont:defcpstransformer without-call/cc (cons k-expr env) 
  (declare (ignore env))
  `(funcall ,k-expr (locally ,@(cdr cons))))

(cl-cont:defcpstransformer handler-case (cons k-expr env)
  "Basic support for now."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

(cl-cont:defcpstransformer handler-bind (cons k-expr env)
  "Basic support for now."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))


(cl-cont:defcpstransformer restart-bind (cons k-expr env)
  "Basic support for now."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

(cl-cont:defcpstransformer restart-case (cons k-expr env)
  "Basic support for now."
  (declare (ignore env))
  `(funcall ,k-expr ,cons))

#+extra-bugs-please 
(defmacro cl-cont:call/cc (cc)
  "Implements delimited continuations."
  (declare (ignore cc))
  (error "Please ensure CALL/CC is called from within WITH-CALL/CC macro."))
