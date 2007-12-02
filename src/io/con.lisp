(in-package #:tpd2.io)

(defstruct (con (:constructor %make-con))
  socket
  peer-info
  (recv (make-recvbuf) :type recvbuf)

  ready-callback
  (err (constantly nil)))

(defun make-con (&rest args)
  (let ((con (apply '%make-con args)))
    (let ((sock (con-socket con)))
      (assert sock)
      (finalize con 
		(lambda()
		  (ignore-errors (socket-close sock)))))
    con))

(my-defun con run ()
  (funcall (my ready-callback)))

(my-defun con hangup ()
  (cancel-finalization me)
  (socket-close (my socket))
  (setf (my socket) nil))

(defconstant +newline+ (force-byte-vector #(13 10)))

(my-defun con set-callback (func)
  (setf (my ready-callback) func))

(my-defun con 'recv (done amount)
  (cond
    ((>= (recvbuf-available-to-eat (my recv)) amount)
     (funcall done (recvbuf-eat (my recv) amount)))
    (t
     (recvbuf-prepare-read (my recv) amount)
     (recvbuf-recv (my recv) me #'my-call))))

(my-defun con 'recvline (done)
  (acond
   ((recvbuf-eat-to-delimiter (my recv) +newline+)
    (funcall done it))
   (t 
    (recvbuf-prepare-read (my recv))
    (recvbuf-recv (my recv) me #'my-call))))

(my-defun con 'send (done sendbuf)
  (cond
    ((sendbuf-done sendbuf)
     (funcall done))
    (t
     (sendbuf-send sendbuf me #'my-call))))

(my-defun con 'accept (done)
  (acond 
      ((socket-accept (my socket))
       (funcall done it))
      (t
       (socket-when-ready-to-read (my socket) me #'my-call))))

