(in-package #:tpd2.io)

(deftype sendbuf-small-integer ()
  '(unsigned-byte 24))

(defstruct sendbuf
  (head nil :type list)
  (tail nil :type list)
  (num-bufs 0 :type sendbuf-small-integer)
  (len 0 :type sendbuf-small-integer))

(my-defun sendbuf add (x)
  (my-declare-fast-inline)
  (cond ((sendbuf-p x)
	 (my merge x))
	(x
	 (let ((buf (force-byte-vector x)))
	   (unless (zerop (length buf))
	     (incf (my num-bufs))
	     (incf (my len) (the sendbuf-small-integer (length buf)))
	     (let ((n (cons buf nil)))
	       (cond ((my head)
		      (setf (cdr (my tail)) n)
		      (setf (my tail) n))
		     (t (setf (my head) n
			      (my tail) n))))))))
  (values))

(my-defun sendbuf merge (other)
  (my-declare-fast-inline)
  (cond 
    ((my head)
     (setf (cdr (my tail)) (sendbuf-head other))
     (when (sendbuf-tail other)
       (setf (my tail) (sendbuf-tail other))))
    (t (setf (my head) (sendbuf-head other)
	     (my tail) (sendbuf-tail other))))

  (incf (my len) (sendbuf-len other))
  (incf (my num-bufs) (sendbuf-num-bufs other))
  (setf (sendbuf-num-bufs other) 0 
	 (sendbuf-head other) nil
	 (sendbuf-tail other) nil
	 (sendbuf-len other) 0)
  (values))

(defmacro with-sendbuf-continue ((var) &body body &environment env)
  `(progn
     ,@(loop for form in body
	     collect 
	     (case (force-first form) 
	       (quote
		(second form))
	       (t
		(if (load-time-constantp form env)
		    `(sendbuf-add ,var (load-time-value (force-byte-vector ,form) t))
		    `(sendbuf-add ,var
				  ,form)))))
     (values)))

(defmacro with-sendbuf ((&optional (var (gensym "sendbuf"))) &body body)
  (check-symbols var)
  `(let ((,var (make-sendbuf)))
     (with-sendbuf-continue (,var)
       ,@body)
     ,var))

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

(my-defun sendbuf to-byte-vector ()
  (declare (optimize speed))
  (let ((result (make-byte-vector (my len))) (i 0))
    (loop for s in (my head) do
	  (loop for c across (the simple-byte-vector s) do
		(setf (aref result i) c)
		(incf i)))
    result))

(my-defun sendbuf 'print-object (stream)
  (cond (*print-readably* (call-next-method))
	(t (write (force-string (my to-byte-vector)) :stream stream :escape t))))
