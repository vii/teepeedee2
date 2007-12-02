(in-package #:tpd2.io)

(defstruct recvbuf
  (store (make-byte-vector 2048) :type byte-vector)
  (read-idx 0 :type fixnum)
  (write-idx 0 :type fixnum))

(my-defun recvbuf len ()
  (length (my store)))

(my-defun recvbuf full ()
  (= (my write-idx) (my len)))

(my-defun recvbuf available-to-eat ()
  (- (my write-idx) (my read-idx)))

(my-defun recvbuf prepare-read (&optional (size 1024))
  (when (> size (- (my len) (my read-idx)))
    (cond 
      ((= (my write-idx) (my read-idx))
       (when (> size (my len))
	 (setf (my store) (make-byte-vector (my len))))
       (setf
        (my read-idx) 0
        (my write-idx) 0))
      (t
       (setf (my store) (adjust-array 
			 (make-displaced-vector (my store) :start (my read-idx))
			 size))
       (decf (my write-idx) (my read-idx))
       (setf (my read-idx) 0)))))

(my-defun recvbuf recv (con done)
  (let ((s
	 (socket-read (con-socket con)
		      (make-displaced-vector (my store) :start (my write-idx)))))
    (cond 
      (s
	(incf (my write-idx) s)
	(funcall done))
      (t
       (socket-when-ready-to-read (con-socket con) con #'my-call)))))

(my-defun recvbuf eat-to-idx (&optional (ending (recvbuf-write-idx recvbuf)))
  (prog1
      (make-displaced-vector (my store) :start (my read-idx) :end ending)
    (setf (my read-idx) ending)))

(my-defun recvbuf eat (amount)
  (my eat-to-idx (+ (my read-idx) amount)))

(my-defun recvbuf find (delimiter)
  (let ((limit (- (my write-idx) (1- (length delimiter)))))
    (loop for i from (my read-idx) below limit
          thereis 
          (unless
            (loop for j from 0 below (length delimiter)
                  thereis (/= (aref delimiter j) (aref (my store) (+ i j))))
            i))))

(my-defun recvbuf eat-to-delimiter (delimiter)
  (let ((ending (my find delimiter)))
    (when ending
      (prog1 
          (my eat-to-idx ending)
        (incf (my read-idx) (length delimiter))))))

