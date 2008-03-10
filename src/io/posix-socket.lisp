(in-package #:tpd2.io)

(defmacro socket-io-syscall (call)

  #+never `(handler-bind 
	       ((syscall-failed #'(lambda(e)
				    (when (not (member (syscall-failed-errno e) 
					       '(+EINVAL+ +EBADF+)))
				      (error 'socket-closed)))))
	     ,call)
  call)


(defmethod socket-read ((fd integer) buf)
  (declare (optimize speed))
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-read fd ptr (length buf))))))
    (case-= s
	    (-1 nil)
	    (0 (error 'socket-closed))
	    (t s))))

(defmethod socket-write ((fd integer) buf)
  (declare (optimize speed))
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-write fd ptr (length buf))))))
    (case-= s
	    (-1 nil)
	    (t s))))

(defmethod socket-writev ((fd integer) iovec count)
  (declare (optimize speed))
  (let ((s
	 (socket-io-syscall (syscall-writev fd iovec count))))
    (case-= s
	    (-1 nil)
	    (t s))))

(defmethod socket-accept ((fd integer))
  (cffi:with-foreign-object (sa 'sockaddr_in)
    (cffi:with-foreign-object (len :int)
      (setf (cffi:mem-aref len :int) (cffi:foreign-type-size 'sockaddr_in))
      (let ((s
	     (socket-io-syscall (syscall-accept fd sa len))))
	(case-= s
		(-1 nil)
		(t
		 (set-fd-nonblock s)
		 (make-con 
		  :socket s
		  :peer-info (make-peer-info :address (cffi:foreign-slot-value sa 'sockaddr_in 'addr)))))))))


(defmethod socket-close ( (fd integer) )
  (deregister-fd fd)
  (handler-case (syscall-close fd)
    (syscall-failed ())))

(defmethod socket-register ((fd integer) events con)
  (debug-assert (eql fd (con-socket con)))
  (register-fd events con))

(defmethod socket-supports-writev ( (fd integer))
  (declare (ignore fd))
  #+tpd2-byte-vectors-do-not-move-arbitrarily
  t)

(defmethod socket-recvfrom ( (fd integer) buf)
  (cffi:with-foreign-object (sa 'sockaddr_in)
    (cffi:with-foreign-object (len :int)
      (setf (cffi:mem-aref len :int) (cffi:foreign-type-size 'sockaddr_in))
      (with-pointer-to-vector-data (ptr buf)
	(let ((s (socket-io-syscall (syscall-recvfrom fd ptr (length buf) 0 sa len))))
	  (case-= s
		  (-1 (values nil nil))
		  (0 (error 'socket-closed))
		  (t 

		   (let ((sa-out (make-byte-vector (cffi:mem-aref len :int))))
		     (loop for i from 0 below (length sa-out) do
			   (setf (aref sa-out i) (cffi:mem-ref sa :unsigned-char i)))
		     (values s sa-out)))))))))

(defmethod socket-sendto ((fd integer) sa buf)
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (with-pointer-to-vector-data (sa-ptr sa)
	     (socket-io-syscall (syscall-sendto fd ptr (length buf) 0 sa-ptr (length sa)))))))
    (case-= s
	    (-1 nil)
	    (t s))))
