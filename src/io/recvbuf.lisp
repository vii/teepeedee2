(in-package #:tpd2.io)

(defstruct recvbuf
  (store (make-byte-vector 1024) :type simple-byte-vector)
  (read-idx 0 :type (integer 0 #x1000000))
  (write-idx 0 :type (integer 0 #x1000000)))

(my-defun recvbuf len ()
  (my-declare-fast-inline)
  (length (my store)))

(my-defun recvbuf half-full-or-more ()
  (my-declare-fast-inline)
  (or (>= (* 2 (- (my write-idx) (my read-idx))) (my len))
      (my full)))

(my-defun recvbuf empty ()
  (my-declare-fast-inline)
  (= (my read-idx) (my write-idx)))

(my-defun recvbuf full ()
  (my-declare-fast-inline)
  (= (my write-idx) (my len)))

(my-defun recvbuf available-to-eat ()
  (my-declare-fast-inline)
  (- (my write-idx) (my read-idx)))

(my-defun recvbuf prepare-read (&optional (size 1024))
  (declare (type fixnum size))
  (when (> size (- (my len) (my read-idx)))
    (cond 
      ((= (my write-idx) (my read-idx))
       (when (> size (my len))
	 (setf (my store) (make-byte-vector size)))
       (setf
        (my read-idx) 0
        (my write-idx) 0))
      (t
       ;; Unfortunately cannot use adjust-array as that might make non "simple" arrays
       (let ((new-store (make-byte-vector (max (my len) size))))
	 (replace new-store (my store) :start2 (my read-idx) :end2 (my write-idx))
	 (decf (my write-idx) (my read-idx))
	 (setf (my read-idx) 0)
	 (setf (my store) new-store)))))
  (debug-assert (>= (- (my len) (my read-idx)) size))
  (values))

(my-defun recvbuf read-some (con &optional retry)
  (debug-assert (not (my full)))
  (let ((s
	 (socket-read (con-socket con)
		      (make-displaced-vector (my store) :start (my write-idx)))))
    (cond 
      ((not s)
       (when retry
	 (con-when-ready-to-read con retry))
       nil)
      (t
       (locally
	   (declare (type fixnum s))
	 (incf (my write-idx) s)
	 s)))))

(my-defun recvbuf recv (con &optional done)
  (my-declare-fast-inline)
  (let ((s (my read-some con (when done #'my-call))))
    (cond ((not s))
	  ((zerop s)
	   (error 'socket-closed))
	  (t
	   (when done
	     (funcall done))))))

(my-defun recvbuf sync ()
  (my-declare-fast-inline)
  (when (my empty)
    (setf (my write-idx) 0)
    (setf (my read-idx) 0))
  (values))

(my-defun recvbuf peek ()
  (my-declare-fast-inline)
  (make-displaced-vector (my store) :start (my read-idx) :end (my write-idx)))

(my-defun recvbuf eat-to-idx (&optional (ending (recvbuf-write-idx recvbuf)))
  (my-declare-fast-inline)
  (prog1
      (make-displaced-vector (my store) :start (my read-idx) :end ending)
    (setf (my read-idx) ending)
    (my sync)))

(my-defun recvbuf eat (amount)
  (my-declare-fast-inline)
  (my eat-to-idx (+ (my read-idx) amount)))

(my-defun recvbuf find (delimiter)
  (declare (type simple-byte-vector delimiter))
  (let ((limit (- (my write-idx) (1- (length delimiter)))))
    (loop for i from (my read-idx) below limit
          thereis 
          (unless
            (loop for j from 0 below (length delimiter)
                  thereis (/= (aref delimiter j) (aref (my store) (+ i j))))
            i))))

(my-defun recvbuf eat-to-delimiter (delimiter)
  (my-declare-fast-inline)
  (let ((ending (my find delimiter)))
    (when ending
      (prog1 
          (my eat-to-idx ending)
        (incf (my read-idx) (length delimiter))
	(my sync)))))

(my-defun recvbuf 'print-object (stream)
  (print-unreadable-object (me stream :type t :identity t)
    (format stream "read ~D/~D bytes: ~A~%|~%~A"
	    (my read-idx)
	    (my write-idx)
	    (force-string (subseq (my store) 0 (my read-idx)))
	    (force-string (subseq (my store) (my read-idx) (my write-idx))))))
