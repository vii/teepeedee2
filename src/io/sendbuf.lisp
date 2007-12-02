(in-package #:tpd2.io)

(defstruct sendbuf
  (head nil)
  (tail nil)
  (num-bufs 0))

(my-defun sendbuf add (buf)
  (unless (zerop (length buf))
    (incf (my num-bufs))
    (let ((n (cons buf nil)))
      (cond ((my head)
	     (setf (cdr (my tail)) n)
	     (setf (my tail) n))
	    (t (setf (my head) n
		     (my tail) n))))))

(defmacro build-sendbuf (&body body)
  (with-unique-names (sendbuf)
  `(let ((,sendbuf (make-sendbuf)))
     ,@(loop for form in body
	     collect `(sendbuf-add ,sendbuf 
				   (force-byte-vector ,form)))
     ,sendbuf)))

(my-defun sendbuf done ()
  (not (my head)))

(my-defun sendbuf send (con done)
  (loop for buf = (car (my head))
	while 		   
	(let ((s (socket-write (con-socket con) buf)))
	  (when s
	    (cond ((> (length buf) s)
		   (setf (car (my head)) (make-displaced-vector buf :start s))
		   nil)
		  (t
		   (setf (my head) (cdr (my head)))
		   (decf (my num-bufs))
		   (my head))))))
  (if (my head)
    (socket-when-ready-to-write (con-socket con) con #'my-call)
    (funcall done)))
