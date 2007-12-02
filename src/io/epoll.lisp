(in-package #:tpd2.io)

(defstruct (epoll (:include mux))
  fd
  events
  postpone-registration
  postponed-registrations)

(my-defun epoll max-events ()
  64)

(my-defun epoll init ()
  (assert (not (my fd)))
  (let ((fd (syscall-epoll_create 10)))
    (setf (my fd) fd)
    (finalize me (lambda() (ignore-errors (syscall-close fd)))))
  (let ((events-mem (cffi:foreign-alloc 'epoll-event :count (my max-events))))
    (setf (my events) events-mem)
    (finalize me 
	      (lambda() 
		(ignore-errors
		  (cffi:foreign-free events-mem))))))  

(my-defun epoll ctl (ctl fd-wanted events-wanted)
  (with-foreign-object-and-slots ((events data) event epoll-event)
    (setf events
	  (logior
	   events-wanted
	   +POLLHUP+
	   +POLLERR+))
    (cffi:with-foreign-slots ((fd) data epoll-data)
      (setf fd fd-wanted))
    (syscall-epoll_ctl (my fd) ctl fd-wanted event))
  (values))

(my-defun epoll wait (timeout)
  (setf (my postpone-registration) t)

  (let ((nevents
	 (syscall-epoll_wait (my fd) (my events) (my max-events) 
			     (when timeout (floor (* 1000 timeout))))))
    (assert (>= (my max-events) nevents))
    (dotimes (i nevents)
      (let ((event (cffi:mem-aref (my events) 'epoll-event i)))
	  (cffi:with-foreign-slots ((events data) event epoll-event)
	    (cffi:with-foreign-slots ((fd) data epoll-data)
	      (awhen (my 'mux-find-fd fd)
		(handler-case 
		    (progn
		      (con-run it)
		      (unless (zerop (logand (logior +POLLERR+ +POLLHUP+ +POLLRDHUP+) events))
			(error 'socket-closed)))
		  (socket-closed ()
		    (con-hangup it)))))))))
  (setf (my postpone-registration) nil)
  (adolist (my postponed-registrations)
    (my 'mux-add it))
  (setf (my postponed-registrations) nil)
  
  (values))


(defvar *global-epoll* 
  (let ((e (make-epoll)))
    (epoll-init e)
    e))

(defun register-fd (events con)
  (with-shorthand-accessor (my epoll *global-epoll*)
    (let ((fd (con-socket con)))
      (cond ((my 'mux-find-fd fd) 
	     (my ctl +EPOLL_CTL_MOD+ fd events))
	    (t
	     (if (my postpone-registration)
		 (push con (my postponed-registrations))
		 (my 'mux-add con))
	     (my ctl +EPOLL_CTL_ADD+ fd events))))))

(defun deregister-fd (fd)
  (with-shorthand-accessor (my epoll *global-epoll*)
    (my 'mux-del fd)))

(defun wait-for-next-event (timeout)
  (with-shorthand-accessor (my epoll *global-epoll*)
    (my wait timeout)))

