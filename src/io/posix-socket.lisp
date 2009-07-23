(in-package #:tpd2.io)

(defmacro socket-io-syscall (call)
  #+never `(handler-bind 
	       ((syscall-failed #'(lambda(e)
				    (when (not (member (syscall-failed-errno e) 
					       '(+EINVAL+ +EBADF+)))
				      (error 'socket-closed)))))
	     ,call)
  call)


(defmethod socket-read ((fd integer) buf offset)
  (declare (type simple-byte-vector buf))
  (declare (type fixnum offset))
  (debug-assert (not (zerop (length buf))))
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-read fd (cffi:inc-pointer ptr offset) (- (length buf) offset))))))
    (case-= s
	    (-1 nil)
	    (t s))))

(defmethod socket-write ((fd integer) buf offset)
  (declare (type simple-byte-vector buf))
  (declare (type fixnum offset))
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-write fd (cffi:inc-pointer ptr offset) (- (length buf) offset))))))
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
		 #-tpd2-untransformed-io
		 (set-fd-nonblock s)
		 (socket-set-tcp-nodelay s)
		 (make-con 
		  :socket s
		  :peer-info (sockaddr-address-bv sa))))))))


(defmethod socket-close ((fd integer))
  (declare (optimize speed))
  (syscall-close fd)
  (deregister-fd fd)
  (values))

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

#+broken
(defmethod socket-sendto ((fd integer) sa buf)
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (with-pointer-to-vector-data (sa-ptr sa)
	     (socket-io-syscall (syscall-sendto fd ptr (length buf) 0 sa-ptr (length sa)))))))
    (case-= s
	    (-1 nil)
	    (t s))))


(defmethod socket-peer ((fd integer))
  (cffi:with-foreign-object (sa 'sockaddr_in)
    (cffi:with-foreign-object (len :int)
      (setf (cffi:mem-aref len :int) (cffi:foreign-type-size 'sockaddr_in))
      (when (zerop (getpeername fd sa len))
	(sockaddr-address-string sa)))))

(defmethod socket-shutdown-write ((fd integer))
  (syscall-shutdown fd +SHUT_WR+))

(defmethod socket-only-accept-if-data-ready ((fd integer) timeout)
  (setsockopt-int fd +IPPROTO_TCP+ +TCP_DEFER_ACCEPT+ (round timeout)))
