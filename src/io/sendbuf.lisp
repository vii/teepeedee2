(in-package #:tpd2.io)

(deftype sendbuf-small-integer ()
  '(unsigned-byte 24))

(declaim (inline make-sendbuf))
(defstruct sendbuf
  (head nil :type list) ; a list of simple byte-vectors
  (tail nil :type list)
  (num-bufs 0 :type sendbuf-small-integer)
  (len 0 :type sendbuf-small-integer)
  (offset 0 :type sendbuf-small-integer))

(my-defun sendbuf to-byte-vector ()
  (my-declare-fast-inline)
  (the simple-byte-vector
    (if (not (cdr (my head)))
	(if (my head) (the simple-byte-vector (car (my head))) (force-byte-vector nil))
	(let ((result (make-byte-vector (+ (my len) (my offset)))) (i 0))
	  (declare (type fixnum i))
	  (loop for s in (my head) do
		(loop for c of-type (unsigned-byte 8) across (the simple-byte-vector s) do
		      (setf (aref result i) c)
		      (incf i)))
	  result))))

(my-defun sendbuf add-simple (buf)
	  (my-declare-fast-inline)
	  (declare (type simple-byte-vector buf))
	  (unless (zerop (length buf))
	    (incf (my num-bufs))
	    (incf (my len) (the sendbuf-small-integer (length buf)))
	    (let ((n (cons buf nil)))
	      (cond ((my head)
		     (setf (cdr (my tail)) n
			   (my tail) n))
		    (t (setf (my head) n
			     (my tail) n))))))

(my-defun sendbuf add (x)
  (my-declare-fast-inline)
  (typecase x
    (simple-byte-vector
     (my add-simple x))
    (sendbuf 
     (my merge x))
    (t
     (my add-simple (force-simple-byte-vector x))))
  (values))

(my-defun sendbuf merge (other)
  (my-declare-fast-inline)
  (declare (dynamic-extent other))
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
     ,@(loop for form in (merge-constant-arguments body :join 'byte-vector-cat :env env)
	     collect 
	     (typecase form
	       (null nil)
	       (simple-byte-vector 
		(when (length form)
		  `(sendbuf-add-simple ,var ,form)))
	       (t
		`(sendbuf-add ,var
			      ,form))))
     (values)))

(defmacro with-sendbuf ((&optional (var (gensym "sendbuf"))) &body body)
  (check-symbols var)
  `(let ((,var (make-sendbuf)))
     (with-sendbuf-continue (,var)
       ,@body)
     ,var))

(my-defun sendbuf done ()
  (my-declare-fast-inline)
  (not (my head)))

(my-defun sendbuf check-done (con finished my-call)
  (my-declare-fast-inline)
  (cond 
    ((my done)
     (setf (my tail) nil)
     (funcall finished))
    (t
     (con-when-ready-to-write con my-call))))


(my-defun sendbuf shift-up (s)
  (my-declare-fast-inline)
  (declare (type sendbuf-small-integer s))
  (decf (my len) s)
  (incf s (my offset))
  (incf (my len) (my offset))
  (setf (my offset) 0)
  (loop until (zerop s)
	do
	(debug-assert (my head) (me s))
	(let ((buf (car (my head))))
	  (declare (type simple-byte-vector buf))
	  (cond ((>= s (length buf))
		 (decf s (length buf))
		 (decf (my num-bufs))
		 (setf (my head) 
		       (cdr (my head))))
		(t
		 (setf (my offset) s)
		 (setf s 0))))))

#- (and) ;; broken
(my-defun sendbuf send-write-piece-by-piece (con done)
  (loop for buf of-type simple-byte-vector = (car (my head))
	for tmp-buf = (make-displaced-vector buf :start (my offset)) then buf
	while 		   
	(let ((s (socket-write (con-socket con) tmp-buf)))
	  (declare (type (or null sendbuf-small-integer) s))
	  (when s
	    (decf (my len) s)
	    (cond ((> (length tmp-buf) s)
		   (setf (my offset) s)
		   nil)
		  (t
		   (setf (my head) (cdr (my head))
			 (my offset) 0)
		   (decf (my num-bufs))
		   (my head))))))
  (my check-done con done #'my-call))

(my-defun sendbuf send-write (con done)
  (let ((buf (my to-byte-vector)))
    (declare (dynamic-extent buf))
    (let ((s (socket-write (con-socket con) buf (my offset))))
      (declare (type (or null sendbuf-small-integer) s))
      (when s
	(my shift-up s))))
  (my check-done con done #'my-call))

(my-defun sendbuf send-writev (con done)
  (my-declare-fast-inline)
  (unless (my done)
    (let ((count (min +max-iovecs+ (my num-bufs))))
      (declare (type (integer 0 #.+max-iovecs+) count))
      (cffi:with-foreign-object (vecs 'iovec count)
	(loop for i below count
		  for buf of-type simple-byte-vector in (my head)
	      for offset fixnum = (my offset) then 0
	      do 
	      (with-pointer-to-vector-data (ptr buf)
		(cffi:with-foreign-slots ((base len) (cffi:mem-aref vecs 'iovec i) iovec)
		  (setf base (cffi:inc-pointer ptr offset))
		  (setf len (length buf)))))
	(let ((s (socket-writev (con-socket con) vecs count)))
	  (declare (type (or null sendbuf-small-integer) s))
	  (when s
	    (my shift-up s))))))
  (my check-done con done #'my-call))



(my-defun sendbuf 'print-object (stream)
  (cond (*print-readably* (call-next-method))
	(t (write (force-string (my to-byte-vector)) :stream stream))))

(my-defun sendbuf empty ()
  (my-declare-fast-inline)
  (not (my head)))
