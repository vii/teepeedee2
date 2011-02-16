(in-package #:tpd2.io)

(defstruct (epoll (:include mux) (:constructor %make-epoll))
  fd
  events
  postpone-registration
  postponed-registrations)

(my-defun epoll max-events ()
  1024)

(my-defun epoll init ()
  (assert (not (my fd)))
  (let ((fd (syscall-epoll_create 10)))
    (setf (my fd) fd)
    (finalize me (lambda() (ignore-errors (syscall-close fd))))
    (let ((events-mem (cffi:foreign-alloc 'epoll-event :count (my max-events))))
      (setf (my events) events-mem)
      (cancel-finalization me)
      (finalize me
                (lambda()
                  (ignore-errors (syscall-close fd))
                  (ignore-errors
                    (cffi:foreign-free events-mem)))))))

(defun make-epoll ()
  (let ((e (%make-epoll)))
    (epoll-init e)
    e))

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


(my-defun epoll handle-postponed-registrations ()
    (assert (not (my postpone-registration)))
    (loop for (fd . con) in (my postponed-registrations) do
      (my 'mux-add fd con))
    (setf (my postponed-registrations) nil))

(my-defun epoll wait (timeout)
  (debug-assert (not (my postponed-registrations)) (me (my postponed-registrations) (my postpone-registration)))
  (setf (my postpone-registration) t)
  (let ((nevents
         (syscall-retry-epoll_wait (my fd) (my events) (my max-events)
                             (if timeout
                                 (floor (* 1000 timeout))
                                 -1))))
    (debug-assert (>= (my max-events) nevents) (me nevents))

    (dotimes (i nevents)
      (let ((event (cffi:mem-aref (my events) 'epoll-event i)))
        (cffi:with-foreign-slots ((events data) event epoll-event)
          (cffi:with-foreign-slots ((fd) data epoll-data)
            (awhen (my 'mux-find-fd fd)
                (unless (zerop (logand (logior +POLLIN+ +POLLOUT+) events))
                  (con-run it))
                (unless (and (zerop (logand (logior +POLLERR+ +POLLHUP+) events))
                             (or (zerop (logand +POLLRDHUP+ events))
                                 (not (zerop (logand +POLLIN+ events)))))
                  (con-fail it)))))))

    (debug-assert (my postpone-registration))
    (setf (my postpone-registration) nil)

    (my handle-postponed-registrations)
    (not (zerop nevents))))

(defvar *epoll* (make-epoll))

(defun register-fd (fd events con)
  (with-shorthand-accessor (my epoll *epoll*)
    (cond ((my 'mux-find-fd fd)
           (debug-assert (eq con (my 'mux-find-fd fd)) (*epoll* con fd))
           (my ctl +EPOLL_CTL_MOD+ fd events))
          (t
           (if (my postpone-registration)
               (push (cons fd con) (my postponed-registrations))
               (my 'mux-add fd con))
           (my ctl +EPOLL_CTL_ADD+ fd events)))))

(defun deregister-fd (fd)
  (declare (optimize speed))
  (with-shorthand-accessor (my epoll *epoll*)
    (my 'mux-del fd)))

(defun-speedy events-pending-p ()
  (not (mux-empty *epoll*)))

(defun wait-for-next-event (&optional timeout)
  (with-shorthand-accessor (my epoll *epoll*)
    (my wait timeout)))

(defun event-loop ()
  (setf (epoll-postpone-registration *epoll*) nil)
  (epoll-handle-postponed-registrations *epoll*)
  (loop for timeout = (next-timeout)
        while (or timeout (events-pending-p)) do
        (wait-for-next-event timeout)
        (let ((start-time (get-timeout-time)))
          (loop do (wait-for-next-event 1)
                thereis (/= start-time (get-timeout-time))))))

(defun event-loop-reset ()
  (setf (epoll-postpone-registration *epoll*) nil)
  (epoll-handle-postponed-registrations *epoll*)
  (mux-close-all *epoll*)
  (forget-timeouts)
  (setf *epoll*
        (make-epoll)))

(defmacro with-independent-event-loop (() &body body)
  `(with-independent-timeouts ()
     (let ((*epoll* (make-epoll)) (*recvbufs* nil))
       ,@body)))

