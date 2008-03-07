(in-package #:tpd2.io)

(defstruct repeater
  con
  other)

(my-defun repeater buf-to-send ()
  (con-recv (repeater-con (my other))))

(my-defun repeater recv ()
  (con-recv (my con)))

(my-defun repeater fd ()
  (con-socket (my con)))

(my-defun repeater other-has-hung-up ()
  (not (repeater-fd (my other))))

(my-defun repeater register-wait ()
  (when (my fd)
    (let ((events 0))
      (unless (recvbuf-empty (my buf-to-send))
	(setf events (logior events +POLLOUT+)))
      (unless (or (recvbuf-half-full-or-more (my recv)) (my other-has-hung-up))
	(setf events (logior events +POLLIN+)))
      
      (con-when-ready (my con) events))))

(my-defun repeater io ()
  (when (my fd)
    (when (not (recvbuf-empty (my buf-to-send)))
      (let ((s (socket-write (my fd) (recvbuf-peek (my buf-to-send)))))
	(when s
	  (recvbuf-eat (my buf-to-send) s))))

    (when (and (recvbuf-empty (my buf-to-send)) (my other-has-hung-up))
      (hangup (my con))))

  (when (my fd)
    (unless (recvbuf-half-full-or-more (my recv))
      (recvbuf-recv (my recv) (my con))))

  (repeater-register-wait (my other))
  (my register-wait))

(my-defun repeater launch-io (other)
  (setf (my other) other)
  (setf (repeater-other (my other)) me)
  (con-set-callback (my con)
		    (lambda() (my io)))
  (my register-wait))

(defun do-repeat (con generator)
  (let ((target (funcall generator)))
    (let ((a (make-repeater :con con))
	  (b (make-repeater :con target)))
      (repeater-launch-io a b)
      (repeater-launch-io b a))))

(defprotocol repeat (con generator)
  (loop for new = (io 'accept con)
	do
	(do-repeat new generator)))

(defun forward-port (&key (src-address "127.0.0.1") src-port (dst-address "127.0.0.1") dst-port)
  (launch-io 'repeat (make-con-listen :port src-port :address src-address)
	     (lambda()(make-con-connect :address dst-address :port dst-port))))

