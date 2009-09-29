(in-package #:tpd2.lib)

(defvar *timeouts* (make-quick-queue))
(defvar *timeout-started* nil)

(declaim (inline %make-timeout-internal))
(defstruct (timeout (:include quick-queue-entry) (:constructor %make-timeout-internal))
  (time nil :type (or null integer))
  func)

(defun-speedy get-timeout-time () ;; much smaller than get-time-of-day so less likely to be a bignum
  (floor (get-internal-real-time) internal-time-units-per-second))

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
  (max (- (my time) (get-timeout-time)) 0))

(my-defun timeout due (time)
  (>= time (my time)))

(my-defun timeout position ()
  (quick-queue-get *timeouts* (my time)))

(defun-speedy max-timeout-period ()
  (length (quick-queue-entries *timeouts*)))

(declaim (inline time-for-delay))
(defun time-for-delay (delay)
  (declare (optimize speed))
  (let ((delay (floor delay)))
    (debug-assert (> (max-timeout-period) (* delay 2)) (delay))
    (+ (get-timeout-time) delay)))

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

(defun describe-timeouts ()
  (let ((start (or *timeout-started* (- (get-timeout-time) (/ (max-timeout-period) 2)))) (count 0) (earliest nil) (latest nil) (biggest-stack-count 0) biggest-stack-time)
    (loop for x from start below (+ start (max-timeout-period))
	  do 
	  (let ((base (quick-queue-get *timeouts* x)) (stack-count 0))
	    (loop for cur = (quick-queue-entry-next base) then (quick-queue-entry-next cur)
		  while (not (eq cur base))
		  do (incf count)
		  (incf stack-count)
		  (debug-assert (= (timeout-time cur) x) (cur x (timeout-time cur) start))
		  (unless earliest (setf earliest cur))
		  (unless latest (setf latest cur))
		  (when (> (timeout-time cur) (timeout-time latest))
		    (setf latest cur))
		  (when (< (timeout-time cur) (timeout-time earliest))
		    (setf earliest cur)))
	    (when (> stack-count biggest-stack-count)
	      (setf biggest-stack-count stack-count
		    biggest-stack-time x))))
    (format t "~&Now ~Ds; ~D timeout~:P active.~%" (get-timeout-time) count)
    (when biggest-stack-time
      (format t "~&The largest concentration is of ~D timeout~:P in ~Ds.~&" biggest-stack-count (- biggest-stack-time (get-timeout-time)))
      (format t "~&The next timeout is in ~Ds: ~A~&" (- (timeout-time earliest) (get-timeout-time)) earliest)
      (describe earliest)
      (unless (eq latest earliest)
	(format t "~&The last timeout is in ~Ds: ~A~&" (- (timeout-time latest) (get-timeout-time)) latest)
	(describe latest)))))

(defun next-timeout (&optional (now (get-timeout-time)))
  (loop for x from (or *timeout-started* (- now (/ (max-timeout-period) 2))) upto now do
	(let ((base (quick-queue-get *timeouts* x)))
	  (loop for cur = (quick-queue-entry-next base)
		while (not (eq cur base))
		do 
		(debug-assert (= (timeout-time cur) x) (cur x now (timeout-time cur) *timeout-started*))
		(timeout-run cur))))
  (setf *timeout-started* nil)
  (describe-timeouts)
  (loop for x from now below (+ now (max-timeout-period))
	thereis 
	(let ((base (quick-queue-get *timeouts* x)))
	  (let ((timeout (quick-queue-entry-next base)))
	    (when (not (eq base timeout))
	      (debug-assert (= (timeout-time timeout) x) (timeout x (timeout-time timeout) now))
	      (setf *timeout-started* now)
	      (- x now))))))

(defmacro with-independent-timeouts (() &body body)
  `(let (*timeout-started*
	 (*timeouts* (make-quick-queue)))
     ,@body))
