(in-package #:tpd2.lib)

(defstruct (timeout (:include quick-queue-entry) (:constructor %make-timeout-internal))
  time
  func)

(defun make-timeout (&key func delay)
  (let ((timeout (%make-timeout-internal :func func)))
    (quick-queue-entry-init timeout)
    (when delay (timeout-reset timeout delay))
    timeout))

(defvar *timeouts* (make-quick-queue))
(defvar *timeouts-last-checked* (get-universal-time))

(my-defun timeout due (time)
  (>= time (my time)))

(my-defun timeout position ()
  (quick-queue-get *timeouts* (my time)))

(defun time-for-delay (delay)
  (assert (> (length (quick-queue-entries *timeouts*)) (* delay 2)))
  (+ (get-universal-time) delay))

(my-defun timeout cancel ()
  (quick-queue-entry-del me))

(my-defun timeout merge ()
  (my cancel)
  (quick-queue-entry-add me (my position)))

(my-defun timeout reset (delay)
  (setf (my time) (time-for-delay delay))
  (my merge))

(my-defun timeout set (delay &optional func)
  (when func
    (setf (my func) func))
  (my reset delay))

(my-defun timeout run ()
  (my cancel)
  (when (my func)
    (funcall (my func))))

(defun next-timeout (&optional (time (get-universal-time)))
  (loop for x from *timeouts-last-checked* upto time do
       (let ((base (quick-queue-get *timeouts* x)))
	 (loop for cur = (quick-queue-entry-next base)
	    while (not (eq cur base))
	    do 
	      (assert (eql (timeout-time cur) x))
	      (timeout-run cur))))
  (setf *timeouts-last-checked* time)
  (loop for x from time below (+ time (quick-queue-len *timeouts*))
       thereis 
       (let ((base (quick-queue-get *timeouts* x)))
	 (let ((timeout (quick-queue-entry-next base)))
	   (when (not (eq base timeout))
	     (assert (eql (timeout-time timeout) x))
	     (- x time))))))
