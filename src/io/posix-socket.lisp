(in-package #:tpd2.io)

(defmacro socket-io-syscall (call)
  `(handler-bind 
       ((syscall-failed #'(lambda(e)
			    (when (not (member (syscall-failed-errno e) 
					       '(+EINVAL+ +EBADF+)))
			      (error 'socket-closed)))))
     ,call))


(defun socket-read (fd buf)
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-read fd ptr (length buf))))))
    (case-= s
	    (-1 nil)
	    (0 (error 'socket-closed))
	    (t s))))

(defun socket-write (fd buf)
  (let ((s
	 (with-pointer-to-vector-data (ptr buf)
	   (socket-io-syscall (syscall-write fd ptr (length buf))))))
    (case-= s
	    (-1 nil)
	    (t s))))

(defun socket-accept (fd)
  (cffi:with-foreign-object (sa 'sockaddr_in)
    (let ((s
	   (socket-io-syscall (wrap-syscall-accept fd sa))))
      (case-= s
	(-1 nil)
	(t
	 (set-fd-nonblock s)
	 (make-con 
	  :socket s
	  :peer-info (make-peer-info :address (cffi:foreign-slot-value sa 'sockaddr_in 'addr))))))))

(defun socket-when-ready-to-write (fd con callback)
  (declare (ignore fd))
  (socket-when-ready +POLLOUT+ con callback))

(defun socket-when-ready-to-read (fd con callback)
  (declare (ignore fd))
  (socket-when-ready (logior +POLLIN+ +POLLRDHUP+) con callback))

(defun socket-when-ready (events con callback)
  (con-set-callback con callback)
  (register-fd events con))

(defun socket-close (fd)
  (deregister-fd fd)
  (handler-case (syscall-close fd)
    (syscall-failed ())))
