(in-package #:tpd2.io)

(defmacro defprotocol (name (con-var &rest args) &body body)
  (check-symbols con-var)
  (with-unique-names (done)
    `(defun ,name (,con-var ,done ,@args)
	 (arnesi:with-call/cc 
	   (funcall ,done (locally ,@body))))))

(defun convert-continuation-to-normal-function (k)
  (lambda(&optional x) (arnesi:kall k x)))

(defmacro io (func con-var &rest args)
  (check-symbols con-var)
  (with-unique-names (k)
    `(arnesi:call/cc 
      (lambda(,k)
	(funcall ,func ,con-var (convert-continuation-to-normal-function ,k) ,@args)))))

(defmacro launch-io (func con-var &rest args)
  `(progn
     (funcall ,func ,con-var (constantly nil) ,@args)
     (values)))


  
