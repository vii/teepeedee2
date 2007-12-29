(in-package #:tpd2.io)

(defstruct sendbuf
  (head nil)
  (tail nil)
  (num-bufs 0)
  (len 0))

(my-defun sendbuf add (x)
  (cond ((sendbuf-p x)
	 (my merge x))
	(t (let ((buf (force-byte-vector x)))
	     (unless (zerop (length buf))
	       (incf (my num-bufs))
	       (incf (my len) (length buf))
	       (let ((n (cons buf nil)))
		 (cond ((my head)
			(setf (cdr (my tail)) n)
			(setf (my tail) n))
		       (t (setf (my head) n
				(my tail) n))))))))
  (values))

(my-defun sendbuf merge (other)
  (cond ((my head)
	(setf (cdr (my tail)) (its head other)
	      (my tail) (its tail other)))
	(t (setf (my head) (its head other)
		 (my tail) (its tail other))))
  (incf (my len) (its len other))
  (incf (my num-bufs) (its num-bufs other))
  (setf (its num-bufs other) 'merged
	(its head other) 'merged
	(its tail other) 'merged
	(its len other) 'merged)
  (values))

(defmacro with-sendbuf ((&optional (var (gensym "sendbuf"))) &body body)
  (check-symbols var)
  `(let ((,var (make-sendbuf)))
     (macrolet ((with-sendbuf-continue ((&optional (var ',var)) &body body)
		  `(progn
		     ,@(loop for form in body
			     collect 
			     (case (force-first form) 
			       (quote
				(second form))
			       (t
				`(sendbuf-add ,var
					      ,form))))
		     (values))))
       (with-sendbuf-continue ()
	   ,@body))
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

(my-defun sendbuf 'print-object (stream)
  (cond (*print-readably* (call-next-method))
	(t (write (apply 'concatenate 'string (mapcar (lambda(s)(force-string s)) (my head))) :stream stream :escape t))))