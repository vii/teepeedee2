(in-package #:tpd2.lib)

(defvar *timeouts* (make-quick-queue))
(defvar *timeout-started* nil)

(declaim (inline %make-timeout-internal))
(defstruct (timeout (:include quick-queue-entry) (:constructor %make-timeout-internal))
  (time nil :type (or null integer))
  func)

(defun make-timeout (&key func delay)
  (let ((timeout (%make-timeout-internal :func func)))
    (quick-queue-entry-init timeout)
    (when delay (timeout-reset timeout delay))
    timeout))

(my-defun timeout 'print-object (stream)
  (print-unreadable-object (me stream :identity t)
    (format stream "~As ~A" (when (my time) (my remaining)) (my func))))

(defun forget-timeouts ()
  (setf *timeout-started* nil
	*timeouts* (make-quick-queue)))

(my-defun timeout remaining ()
  (max (- (my time) (get-universal-time)) 0))

(my-defun timeout due (time)
  (>= time (my time)))

(my-defun timeout position ()
  (quick-queue-get *timeouts* (my time)))

(declaim (inline time-for-delay))
(defun time-for-delay (delay)
  (declare (optimize speed))
  (let ((delay (floor delay)))
    (debug-assert (> (length (quick-queue-entries *timeouts*)) (* delay 2)))
    (+ (get-universal-time) delay)))



(my-defun timeout cancel ()
  (my-declare-fast-inline)
  (quick-queue-entry-del me))

(my-defun timeout merge ()
  (my cancel)
  (when (my time)
    (quick-queue-entry-add me (my position))))

(my-defun timeout reset (delay)
  (my-declare-fast-inline)
  (cond (delay	  
	 (setf (my time) (time-for-delay delay))
	 (my merge))
	(t 
	 (my cancel))))

(my-defun timeout set (delay &optional func)
  (my-declare-fast-inline)
  (when func
    (setf (my func) func))
  (my reset delay))

(my-defun timeout run ()
  (my cancel)
  (when (my func)
    (funcall (my func))))

(defun next-timeout (&optional (time (get-universal-time)))
  (when *timeout-started*
    (loop for x from *timeout-started* upto time do
	  (let ((base (quick-queue-get *timeouts* x)))
	    (loop for cur = (quick-queue-entry-next base)
		  while (not (eq cur base))
		  do 
		  (debug-assert (eql (timeout-time cur) x) (cur x))
		  (timeout-run cur))))
    (setf *timeout-started* nil))
  (loop for x from time below (+ time (quick-queue-len *timeouts*))
	thereis 
	(let ((base (quick-queue-get *timeouts* x)))
	  (let ((timeout (quick-queue-entry-next base)))
	    (when (not (eq base timeout))
	      (debug-assert (eql (timeout-time timeout) x) (timeout x))
	      (setf *timeout-started* time)
	      (- x time))))))

(defmacro with-independent-timeouts (() &body body)
  `(let (*timeout-started*
	 (*timeouts* (make-quick-queue)))
     ,@body))
