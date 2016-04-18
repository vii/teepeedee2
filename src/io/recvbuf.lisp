(in-package #:tpd2.io)

(deftype recvbuf-small-integer ()
  `(integer 0 #x10000000))

(defconstant +recvbuf-default-size+ 8192)
(defconstant +recvbuf-maximum-size+ (* 64 1024))
(defconstant +recvbuf-oversize+ 10000)
(defconstant +recvbuf-target-available-size+ 1024)

(defstruct recvbuf
  (store (make-byte-vector +recvbuf-default-size+) :type simple-byte-vector)
  (read-idx 0 :type recvbuf-small-integer)
  (write-idx 0 :type recvbuf-small-integer))

(defvar *recvbufs* nil)

(my-defun recvbuf len ()
  (my-declare-fast-inline)
  (the recvbuf-small-integer (length (my store))))

(my-defun recvbuf reset ()
  (my-declare-fast-inline)
  (setf (my write-idx) 0
        (my read-idx) 0))

(declaim (ftype (function () recvbuf) get-recvbuf))
(defun-speedy get-recvbuf ()
  (or (pop *recvbufs*) (make-recvbuf)))

(my-defun recvbuf 'put-recvbuf ()
  (unless (> (my len) +recvbuf-oversize+)
    (my reset)
    (push me *recvbufs*)))

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
  (the recvbuf-small-integer (- (my write-idx) (my read-idx))))

(my-defun recvbuf shift-up (&optional (desired-available 0) (maximum-size +recvbuf-maximum-size+))
  (my-declare-fast-inline)
  (cond
    ((= (my write-idx) (my read-idx))
     (when (> desired-available (my len))
       (setf (my store) (make-byte-vector (min maximum-size desired-available))))
     (setf
      (my read-idx) 0
      (my write-idx) 0))
    (t
     ;; Unfortunately cannot use adjust-array as that might make non "simple" arrays
     (let ((new-store (make-byte-vector
		       (min maximum-size
			    (max (my len) (+ (my available-to-eat) desired-available))))))
       (replace new-store (my store) :start2 (my read-idx) :end2 (my write-idx))
       (decf (my write-idx) (my read-idx))
       (setf (my read-idx) 0
	     (my store) new-store))))
  (values))

(my-defun recvbuf prepare-read (&optional (desired-available +recvbuf-target-available-size+) (maximum-size +recvbuf-maximum-size+))
  (declare (type recvbuf-small-integer desired-available))
  (when (> desired-available (- (my len) (my write-idx)))
    (my shift-up desired-available maximum-size))
  (debug-assert (>= (- (my len) (my write-idx)) desired-available) (me desired-available))
  (values))

(my-defun recvbuf read-some (con &optional retry)
  (my-declare-fast-inline)
  (assert (not (my full)) () 'connection-buffer-overflow-error :con con :len (my len))
  (let ((s
         (socket-read (con-socket con)
                      (my store)
                      (my write-idx))))
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
  (my-declare-fast-inline)
  (declare (type simple-byte-vector delimiter))
  (let ((limit (- (my write-idx) (1- (length delimiter))))
        (trigger (aref delimiter 0)))
    (loop for i from (my read-idx) below limit
          thereis
          (and (= trigger (aref (my store) i))
           (unless
               (loop for j from 1 below (length delimiter)
                     thereis (/= (aref delimiter j) (aref (my store) (+ i j))))
             i)))))

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
