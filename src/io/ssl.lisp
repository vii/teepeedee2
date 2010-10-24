(in-package #:tpd2.io)

(defstruct ssl-socket
  transport
  ssl
  (event-wanted 0)
  state)

(cffi:defcfun ("SSL_CTX_ctrl" ssl-ctx-ctrl)
    :long
  (ssl-ctx :pointer)
  (cmd :int)
  (larg :long)
  (parg :pointer))
    
(defun ssl-socket-init (ss)
  (initialize-openssl)
  (let ((context *ssl-ctx*))
    (ssl-ctx-set-mode context +SSL_MODE_ENABLE_PARTIAL_WRITE+)
    (ssl-ctx-set-mode context +SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER+)
    (setf (ssl-socket-ssl ss) (ssl-new context)))

  (ssl-set-fd (ssl-socket-ssl ss) (ssl-socket-transport ss))
  (setf (ssl-socket-state ss) 'connect)

  (let ((socket (ssl-socket-ssl ss)))
    (trivial-garbage:finalize ss (lambda() 
		   (ssl-free socket)))))

(defun convert-con-to-ssl (con)
  (let ((ss (make-ssl-socket :transport (con-socket con))))
    (ssl-socket-init ss)
    (setf (con-socket con) ss)
    con))

(define-condition ssl-error (socket-error)
  ((call-name :initarg :call-name :initform nil)
   (return-code :initarg :rc :initform nil)
   (ssl-error-code :initarg :ssl-error-code :initform nil)
   (errno :initform errno)))

(defmethod print-object ((ss ssl-error) stream)
  (print-unreadable-object (ss stream :identity t)
    (with-slots (call-name return-code ssl-error-code errno) ss
      (format stream "~A returned ~A; SSL_Get_Error ~A; errno ~A"
	      call-name return-code ssl-error-code errno))))
  
(defun ssl-socket-check-error (ss rc call-name)
  (when (> 0 rc)
    (case (ssl-get-error (ssl-socket-ssl ss) rc)
      (#.+SSL_ERROR_NONE+ nil)
      (#.+SSL_ERROR_WANT_READ+ (setf (ssl-socket-event-wanted ss) +POLLIN+))
      (#.+SSL_ERROR_WANT_WRITE+ (setf (ssl-socket-event-wanted ss) +POLLOUT+))
      (otherwise 
       (error 'ssl-error :call-name call-name :rc rc :ssl-error-code (ssl-get-error (ssl-socket-ssl ss) rc))))))

(defun ssl-socket-process-state (ss)
  (setf (ssl-socket-event-wanted ss) 0)
  (ecase (ssl-socket-state ss)
    (connect 
     (unless 
	 (or
	  (ssl-socket-check-error ss (ssl-connect (ssl-socket-ssl ss)) "SSL_Connect")
	  (eq +SSL_ST_CONNECT+ (ssl-state (ssl-socket-ssl ss))))
       (setf (ssl-socket-state ss) 'running)))
    (running
     nil))
  (not (eq (ssl-socket-state ss) 'running)))

(defmethod socket-write ((ss ssl-socket) buf offset)
  (unless (ssl-socket-process-state ss)
    (let ((written
	   (cffi:with-pointer-to-vector-data (out-ptr buf)
	     (ssl-write (ssl-socket-ssl ss) (cffi:inc-pointer out-ptr offset) (- (length buf) offset)))))
      (ssl-socket-check-error ss written "SSL_Write")
      (when (> written 0)
	written))))

(defmethod socket-read ((ss ssl-socket) buf offset)
  (unless (ssl-socket-process-state ss)
    (let ((amount
	   (cffi:with-pointer-to-vector-data (in-ptr buf)
	     (ssl-read (ssl-socket-ssl ss) (cffi:inc-pointer in-ptr offset) (- (length buf) offset)))))
      (ssl-socket-check-error ss amount "SSL_Read")
      (cond ((and (zerop amount) 
		  (eql (ssl-get-error (ssl-socket-ssl ss) 0) +SSL_ERROR_ZERO_RETURN+))
	     0)
	    ((> amount 0) amount)
	    (t nil)))))
  
(defmethod socket-peer ((ss ssl-socket))
  (socket-peer (ssl-socket-transport ss)))

(defmethod socket-close ((ss ssl-socket))
  (awhen (ssl-socket-transport ss)
    (setf (ssl-socket-transport ss) nil)
    (socket-close it)))

(defmethod socket-register ((ss ssl-socket) events con)
  (debug-assert (eql ss (con-socket con)) (ss con))
  (register-fd (ssl-socket-transport ss) 
	       (if (zerop (ssl-socket-event-wanted ss))
		   events
		   (ssl-socket-event-wanted ss))
	       con))
