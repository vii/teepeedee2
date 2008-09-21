(in-package #:tpd2.io)

(define-condition socket-error (error)
  ())
(define-condition socket-closed (socket-error) 
  ())
(define-condition socket-explicitly-hungup (socket-error)
  ())

(defgeneric socket-read (socket buf))
(defgeneric socket-write (socket buf))
(defgeneric socket-accept (socket)) ; returns a CON
(defgeneric socket-close (socket))
(defgeneric socket-register (socket events con))

(defgeneric socket-supports-writev (socket))
(defgeneric socket-writev (socket iovec count))

(defmethod socket-supports-writev (socket)
  (declare (ignore socket)))

(defmethod socket-register (socket events con)
  (declare (ignore events))
  (debug-assert (eql socket (con-socket con)))
  (values))

(defgeneric socket-recvfrom (socket buf)) ; returns (values length/nil address)
(defgeneric socket-sendto (socket address buf))

(defmethod socket-recvfrom (socket buf)
  (values (socket-read socket buf) nil))
(defmethod socket-sendto (socket address buf)
  (declare (ignore address))
  (socket-write socket buf))

(defgeneric socket-peer (socket))

(defmethod socket-peer ((socket null))
  (declare (ignore socket)))
