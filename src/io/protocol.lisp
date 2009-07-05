(in-package #:tpd2.io)

(define-condition protocol-error
    (socket-error)
  ())

(defmacro simple-io-function (sym)
  `(get ,sym 'simple-io-function))

(defmacro with-simple-io (&body body)
  `(macrolet ((io (func con-var &rest args)
		(check-symbols con-var)
		`(funcall (simple-io-function ,func) ,con-var ,@args)))
     (flet ((hangup (con)
	      (declare (ignore con)))
	    (reset-timeout (con timeout)
	      (declare (ignore con timeout))))
       (declare (ignorable #'hangup #'reset-timeout))
       ,@body)))

(defmacro defun-simple-io (name lambda-list &body body)
  `(eval-always
     (setf (simple-io-function ',name)
	   (defun ,(concat-sym-from-sym-package name 'simple-io- name)
	       ,lambda-list (with-simple-io ,@body)))))

(defun-simple-io recv (stream amount)
  (let ((buf (make-string amount)))
    (let ((amt (read-sequence buf stream)))
      (assert (= amt (length buf))))
    (let ((r (force-byte-vector buf)))
      (assert (= (length r) amount))
      r)))

(defun-simple-io recvline (stream)
  (force-byte-vector (read-line stream nil nil)))

(defun-simple-io send (stream sendbuf)
  (loop for buf in (sendbuf-head sendbuf)
	do (write-string (force-string buf) stream))
  (values))

#-tpd2-untransformed-io
(defmacro defprotocol (name (con-var &rest args) &body body)
  (check-symbols con-var name)
  (with-unique-names (done)
    `(progn
       (defun-simple-io ,name (,con-var ,@args)
	   ,@body)
       (defun ,name (,con-var ,done ,@args)
	 (with-call/cc 
	   (funcall ,done (locally ,@body)))))))

#-tpd2-untransformed-io
(defmacro io (func con-var &rest args &environment env)
  (check-symbols con-var)
  (with-unique-names (k)
    (let* (gensyms
	   (func (if (and (listp func) (eq 'quote (first func))) `(function ,@(rest func)) func))
	   (arg-syms (loop for a in args collect 
			  (cond 
			    ((constantp a env) 
			     a)
			    (t
			     (let ((g (gensym (force-string a))))
			      (push `(,g ,a) gensyms)
			      g))))))
      `(let ,(reverse gensyms)
	 (call/cc 
	  (lambda(,k)
	    (funcall ,func ,con-var (convert-continuation-to-normal-function ,k) ,@arg-syms)))))))

(defmacro launch-io (func con-var &rest args)
  (once-only (con-var)
    `(progn
       (con-set-callback ,con-var 
			 (lambda()
			   (funcall ,func ,con-var 
				    (lambda (&rest args) (declare (ignore args))) 
				    ,@args)))
       (con-run ,con-var)
       (values))))


#| ; cl-cont might overflow stack
(defprotocol accept-forever (con proto)
  (loop for n = (io 'accept con)
	do (launch-io proto n)))
; |#


(defvar *socket-accept-burst* 16)
(my-defun con 'accept-forever (done proto)
  (declare (ignore done))
  (loop repeat *socket-accept-burst* 
	for new = (socket-accept (my socket))
	while new
	do (launch-io proto new))
  (my when-ready-to-read #'my-call)
  (values))

#+tpd2-untransformed-io
(defmacro defprotocol (name (con-var &rest args) &body body)
  (check-symbols con-var name)
  (with-unique-names (done)
    `(progn
       (defun-simple-io ,name (,con-var ,@args)
	   ,@body)
       (defun ,name (,con-var ,done ,@args)
	   (funcall ,done (locally ,@body))))))

#+tpd2-untransformed-io
(defmacro io (func con-var &rest args)
  (check-symbols con-var)
  (with-unique-names (val)
    `(let (,val)
       (funcall ,func ,con-var (lambda (&optional arg &rest args) 
			 (declare (ignore args))
			 (setf ,val arg))
	      ,@args)
       ,val)))


;; These preserve the status quo but are convenient for emacs use
#+tpd2-untransformed-io
(pushnew :tpd2-untransformed-io *features*)
#-tpd2-untransformed-io
(deletef :tpd2-untransformed-io *features*)
