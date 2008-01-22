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
	   (defun ,(intern (strcat 'simple-io- name) (symbol-package name))
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


(defmacro defprotocol (name (con-var &rest args) &body body)
  (check-symbols con-var name)
  (with-unique-names (done)
    `(progn
       (defun-simple-io ,name (,con-var ,@args)
	   ,@body)
       (defun ,name (,con-var ,done ,@args)
	 (with-call/cc 
	   (funcall ,done (locally ,@body)))))))

(defmacro io (func con-var &rest args)
  (check-symbols con-var)
  (with-unique-names (k atmp)
    `(let ((,atmp (list ,@args)))
       (call/cc 
	(lambda(,k)
	  (apply ,func ,con-var (convert-continuation-to-normal-function ,k) ,atmp))))))

(defmacro launch-io (func con-var &rest args)
  (once-only (con-var)
    `(progn
       (con-set-callback ,con-var 
			 (lambda()
			   (funcall ,func ,con-var (constantly nil) ,@args)))
       (con-run ,con-var)
       (values))))

(defprotocol accept-forever (con proto)
  (loop for n = (io 'accept con)
	do (launch-io proto n)))


